#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/io_uring.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

#include "common.h"
#include "ioring.h"

/*
 * The Hemlock runtime has some design choices that keep our usage of io_uring remarkably basic.
 *   - We set up one ioring instance per executor. Since executors are pinned to a thread and only
 *     actors managed by that executor may use the ioring instance, the instance is effectively used
 *     in a single-threaded context and does not require any mutex protection.
 *   - Actors emplace SQEs into the submission queue, but there is no need to actually submit (via
 *     `io_uring_enter(2)`) until...
 *     1. The submission queue is full.
 *     2. The actor yields execution to the executor and there are SQEs sitting in the qeueue.
 *     3. The actor blocks on an incomplete completion, possibly based on a SQE that is sitting in
 *        the submission queue. In Hemlock, this will cause the actor to yield and is handled
 *        identically to 2.
 *   - We do not make any use of the submission queue indirection `array` data structure since it is
 *     intended to alleviate contention issues in multi-threaded use-cases. We initialize the
 *     `array` once to point to their respective SQEs in the SQE array.
 *
 * Edge-cases:
 *   - An actor submitting a chain of SQEs can not be idled until it submits the entire chain. When
 *     this chain is submitted across two or more calls to `io_uring_enter(2)`, there is the
 *     possibility that the kernel rejects the tail of the chain with an `EBUSY` error. In this
 *     case, the kernel will not accept any more submissions until we reap some CQEs from the
 *     completion queue.
 */

/* Macros from `io_uring_enter(7)` needed for io_uring memory barriers. */
/*
 * Memory barrier for ensuring that the kernel has a consistent view of our writes to queues.
 * Specifically, use this any time we write:
 *   sqring->tail: Signals that we've added submissions to the tail of the queue.
 *   cqring->head: Signals that we've removed completions from the head of the queue.
 */
#define HM_ATOMIC_STORE_RELEASE(p, v) \
    atomic_store_explicit((_Atomic __typeof__(*(p)) *)(p), (v), memory_order_release)

/*
 * Memory barrier for ensuring that the we have a consistent view of kernel writes to queues.
 * Specifically, use this any time we read:
 *   sqring->head: Signals that the kernel has removed submissions from the head of the queue.
 *   cqring->tail: Signals that the kernel has added completions to the tail of the queue.
 */
#define HM_ATOMIC_LOAD_ACQUIRE(p) \
   atomic_load_explicit((_Atomic __typeof__(*(p)) *)(p), memory_order_acquire)

// Pretty-printer formatting.
#define HM_INDENT_SIZE 4

// io_uring queue size must be a power of two.
#define HM_IORING_ENTRIES 32

static int
io_uring_setup(uint32_t entries, struct io_uring_params *p) {
    return syscall(__NR_io_uring_setup, entries, p);
}

static int
io_uring_enter(unsigned int fd, uint32_t to_submit, uint32_t min_complete, uint32_t flags) {
    return syscall(__NR_io_uring_enter, fd, to_submit, min_complete, flags, (size_t) NULL);
}

static void
hm_opcode_pp(int fd, int indent, unsigned opcode) {
    switch (opcode) {
    case IORING_OP_NOP:
      dprintf(fd, "%*sopcode: IORING_OP_NOP\n", indent, "");
      break;
    case IORING_OP_OPENAT:
      dprintf(fd, "%*sopcode: IORING_OP_OPENAT\n", indent, "");
      break;
    case IORING_OP_CLOSE:
      dprintf(fd, "%*sopcode: IORING_OP_CLOSE\n", indent, "");
      break;
    case IORING_OP_READ:
      dprintf(fd, "%*sopcode: IORING_OP_READ\n", indent, "");
      break;
    case IORING_OP_WRITE:
      dprintf(fd, "%*sopcode: IORING_OP_WRITE\n", indent, "");
      break;
    default:
      dprintf(fd, "%*sopcode: %i\n", indent, "", opcode);
      break;
    };
}

static void
hm_pathname_pp(int fd, int indent,  uint8_t *pathname) {
    if (pathname == NULL) {
        dprintf(fd, "%*spathname: NULL\n", indent, "");
    } else {
        dprintf(fd, "%*spathname: \"%s\"\n", indent, "", pathname);
    }
}

static void
hm_sqe_pp(int fd, int indent, struct io_uring_sqe *sqe) {
    if (sqe == NULL) {
        dprintf(fd, "%*ssqe: NULL\n", indent, "");
    } else {
        dprintf(fd, "%*ssqe:\n", indent, "");
		indent += HM_INDENT_SIZE;
        dprintf(fd,
            "%*sflags: %u\n"
            "%*sioprio: %u\n"
            "%*sfd: %i\n"
            ,
            indent, "", sqe->flags,
            indent, "", sqe->ioprio,
            indent, "", sqe->fd
        );
        hm_opcode_pp(fd, indent, sqe->opcode);
        hm_user_data_pp(fd, indent, (hm_user_data_t *)sqe->user_data);
    }
}

static void
hm_cqe_pp(int fd, int indent, struct io_uring_cqe *cqe) {
    if (cqe == NULL) {
        dprintf(fd, "%*scqe: NULL\n", indent, "");
    } else {
        dprintf(fd, "%*scqe:\n", indent, "");
		indent += HM_INDENT_SIZE;
        dprintf(fd,
            "%*sres: %i\n"
            "%*sflags: %u\n"
            ,
            indent, "", cqe->res,
            indent, "", cqe->flags
        );
        if (cqe->user_data != 0 &&
          cqe->user_data == ((hm_user_data_t *)cqe->user_data)->cqe.user_data) {
            dprintf(fd, "%*suser_data: <same as parent>\n", indent, "");
        } else {
            hm_user_data_pp(fd, indent, (hm_user_data_t *)cqe->user_data);
        }
    }
}

static bool
hm_user_data_is_complete(hm_user_data_t *user_data) {
    // When the cqe is copied out of the completion queue into this user_data, the cqe's user_data
    // field is a valid pointer (i.e. non-zero).
    return user_data->cqe.user_data != 0;
}

void
hm_user_data_pp(int fd, int indent, hm_user_data_t *user_data) {
    if (user_data == NULL) {
        dprintf(fd, "%*suser_data: NULL\n", indent, "");
    } else {
        dprintf(fd, "%*suser_data:\n", indent, "");
		indent += HM_INDENT_SIZE;
        dprintf(fd,
            "%*srefcount: %u\n",
            indent, "", user_data->refcount
        );
        switch (user_data->opcode) {
        case IORING_OP_OPENAT:
            hm_pathname_pp(fd, indent, user_data->pathname);
            break;
        default:
            break;
        }
        hm_opcode_pp(fd, indent, user_data->opcode);
        hm_cqe_pp(fd, indent, &user_data->cqe);
    }
}

void
hm_user_data_decref(hm_user_data_t *user_data) {
    user_data->refcount--;
    if (user_data->refcount == 0) {
        free(user_data);
    }
}

void
hm_sqring_pp(int fd, int indent, hm_sqring_t *sqring) {
    if (sqring == NULL) {
        dprintf(fd, "%*ssqring: NULL\n", indent, "");
    } else {
        dprintf(fd, "%*ssqring:\n", indent, "");
		indent += HM_INDENT_SIZE;
        dprintf(fd,
            "%*shead: %u\n"
            "%*stail: %u\n"
            "%*sring_entries: %u\n"
            "%*sring_mask: %u\n"
            "%*sflags: %u\n"
            ,
            indent, "", HM_ATOMIC_LOAD_ACQUIRE(sqring->head),
            indent, "", *sqring->tail,
            indent, "", *sqring->ring_entries,
            indent, "", *sqring->ring_mask,
            indent, "", *sqring->flags
        );
		dprintf(fd, "%*sarray: [%u" , indent, "", sqring->array[0]);
		for (size_t i = 1; i < *sqring->ring_entries; i++) {
			dprintf(fd, ", %u", sqring->array[i]);
		}
		dprintf(fd, "]\n");

        dprintf(fd, "%*s  sqes:\n", indent, "");
		indent += HM_INDENT_SIZE;
        for (unsigned head = HM_ATOMIC_LOAD_ACQUIRE(sqring->head); head < *sqring->tail; head++) {
            hm_sqe_pp(fd, indent, &sqring->sqes[head & *sqring->ring_mask]);
        }
    }
}

void
hm_cqring_pp(int fd, int indent, hm_cqring_t *cqring) {
    if (cqring == NULL) {
        dprintf(fd, "%*scqring: NULL\n", indent, "");
    } else {
        dprintf(fd, "%*scqring:\n", indent, "");
		indent += HM_INDENT_SIZE;
        dprintf(fd,
            "%*shead: %u\n"
            "%*stail: %u\n"
            "%*sring_entries: %u\n"
            "%*sring_mask: %u\n"
            "%*scqes:\n"
            ,
            indent, "", *cqring->head,
            indent, "", HM_ATOMIC_LOAD_ACQUIRE(cqring->tail),
            indent, "", *cqring->ring_entries,
            indent, "", *cqring->ring_mask,
            indent, ""
        );
		indent += HM_INDENT_SIZE;
        for (unsigned head = *cqring->head; head < HM_ATOMIC_LOAD_ACQUIRE(cqring->tail); head++) {
            hm_cqe_pp(fd, indent, &cqring->cqes[head & *cqring->ring_mask]);
        }
    }
}

void
hm_ioring_pp(int fd, int indent, hm_ioring_t *ioring) {
    if (ioring == NULL) {
        dprintf(fd, "%*sioring: NULL\n", indent, "");
    } else {
        dprintf(fd, "%*sioring:\n", indent, "");
		indent += HM_INDENT_SIZE;
        dprintf(fd, "%*sfd: %i\n" , indent, "", ioring->fd);
        hm_cqring_pp(fd, indent, &ioring->cqring);
        hm_sqring_pp(fd, indent, &ioring->sqring);
    }
}

hm_user_data_t *
hm_user_data_create() {
    hm_user_data_t *user_data = (hm_user_data_t *)calloc(1, sizeof(hm_user_data_t));
    assert(user_data != NULL);

    // Refs from kernel and ocaml.
    user_data->refcount = 2;

    return user_data;
}

static void
hm_sqring_setup(void *vm, struct io_sqring_offsets const *offsets, hm_sqring_t *sqring) {
    sqring->head = vm + offsets->head;
    sqring->tail = vm + offsets->tail;
    sqring->ring_entries = vm + offsets->ring_entries;
    sqring->ring_mask = vm + offsets->ring_mask;
    sqring->flags = vm + offsets->flags;
    sqring->array = vm + offsets->array;
    for (size_t i = 0; i < *sqring->ring_entries; i++) {
        sqring->array[i] = i;
    }
}

static void
hm_cqring_setup(void *vm, struct io_cqring_offsets const *offsets, hm_cqring_t *cqring) {
    cqring->head = vm + offsets->head;
    cqring->tail = vm + offsets->tail;
    cqring->ring_entries = vm + offsets->ring_entries;
    cqring->ring_mask = vm + offsets->ring_mask;
    cqring->cqes = vm + offsets->cqes;
}

static size_t
hm_ioring_get_vm_size(hm_ioring_t *ioring) {
    return ioring->params.sq_off.array + ioring->params.sq_entries * sizeof(uint32_t);
}

static size_t
hm_ioring_get_sqes_size(hm_ioring_t *ioring) {
    return ioring->params.sq_entries * sizeof(struct io_uring_sqe);
}

hm_opt_error_t
hm_ioring_setup(hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;

    memset(ioring, 0, sizeof(*ioring));
    HM_OE_ERRNO_RESULT(oe, ioring->fd, io_uring_setup(HM_IORING_ENTRIES, &ioring->params));

    assert(ioring->params.features & IORING_FEAT_SINGLE_MMAP);
    assert(ioring->params.features & IORING_FEAT_RW_CUR_POS);

    ioring->vm = mmap(
        0,
        hm_ioring_get_vm_size(ioring),
        PROT_READ | PROT_WRITE,
        MAP_SHARED | MAP_POPULATE,
        ioring->fd,
        IORING_OFF_SQ_RING
    );
    if (ioring->vm == (void*) -1) {
        abort();
    }
    ioring->sqring.sqes = (struct io_uring_sqe *)mmap(
        0,
        hm_ioring_get_sqes_size(ioring),
        PROT_READ | PROT_WRITE,
        MAP_SHARED | MAP_POPULATE,
        ioring->fd,
        IORING_OFF_SQES
    );
    if (ioring->sqring.sqes == (void*) -1) {
        abort();
    }

    hm_sqring_setup(ioring->vm, &ioring->params.sq_off, &ioring->sqring);
    hm_cqring_setup(ioring->vm, &ioring->params.cq_off, &ioring->cqring);

LABEL_OUT:
    return oe;
}

void
hm_ioring_teardown(hm_ioring_t *ioring) {
    if (munmap(ioring->sqring.sqes, hm_ioring_get_sqes_size(ioring)) != 0 ||
      munmap(ioring->vm, hm_ioring_get_vm_size(ioring)) != 0 ||
      close(ioring->fd) != 0) {
        abort();
    }

    memset(ioring, 0, sizeof(hm_ioring_t));
}

static hm_opt_error_t
hm_ioring_flush_cqes(uint32_t min_complete, hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;
    hm_cqring_t *cqring = &ioring->cqring;

    assert(min_complete <= *cqring->ring_entries);

    uint32_t tail = HM_ATOMIC_LOAD_ACQUIRE(cqring->tail);
    uint32_t head = *cqring->head;
    if (tail - head < min_complete) {
        uint32_t n_complete;
        HM_OE(oe, hm_ioring_enter(&n_complete, min_complete, ioring));
        tail = HM_ATOMIC_LOAD_ACQUIRE(cqring->tail);
    }
    for (; head < tail; head++) {
        struct io_uring_cqe *cqe = &cqring->cqes[head & *cqring->ring_mask];
        hm_user_data_t *user_data = (hm_user_data_t *)cqe->user_data;
        memcpy(&user_data->cqe, cqe, sizeof(struct io_uring_cqe));
        switch (user_data->opcode) {
        case IORING_OP_OPENAT:
            free(user_data->pathname);
            user_data->pathname = NULL;
            break;
        case IORING_OP_WRITE:
            free(user_data->buffer);
            user_data->buffer = NULL;
            break;
        default:
            break;
        };
        hm_user_data_decref(user_data);
    }
    HM_ATOMIC_STORE_RELEASE(cqring->head, head);

LABEL_OUT:
    return oe;
}

static hm_opt_error_t
hm_ioring_flush_sqes(hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;
    uint32_t n_complete;
    HM_OE(oe, hm_ioring_enter(&n_complete, 0, ioring));
    if (n_complete > 0) {
        // There are CQEs just sitting in the completion queue. Flush them.
        HM_OE(oe, hm_ioring_flush_cqes(0, ioring));
    }

LABEL_OUT:
    return oe;
}

static hm_opt_error_t
hm_ioring_get_sqe(struct io_uring_sqe **sqe, hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;

    hm_sqring_t *sqring = &ioring->sqring;
    if (*sqring->tail - HM_ATOMIC_LOAD_ACQUIRE(sqring->head) == *sqring->ring_entries) {
        HM_OE(oe, hm_ioring_flush_sqes(ioring));
    }

    *sqe = &sqring->sqes[*sqring->tail & *sqring->ring_mask];
    HM_ATOMIC_STORE_RELEASE(sqring->tail, *sqring->tail + 1);
    memset(*sqe, 0, sizeof(struct io_uring_sqe));

LABEL_OUT:
    return oe;
}

hm_opt_error_t
hm_ioring_enter(uint32_t *n_complete, uint32_t min_complete, hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;
LABEL_OUT:
    switch (oe) {
    case EBUSY:
        // In Hemlock, this is the point at which the actor suspends and requires the executor to
        // reap CQEs. If the final SQE has IOSQE_IO_LINK set, no other actors may submit I/O on this
        // ioring until the current actor finishes submitting its chain of SQEs.
        HM_OE(oe, hm_ioring_flush_cqes(1, ioring));
    case HM_OE_NONE:
        HM_OE_ERRNO_RESULT(
          oe,
          *n_complete,
          io_uring_enter(ioring->fd, *ioring->sqring.tail -
            HM_ATOMIC_LOAD_ACQUIRE(ioring->sqring.head), min_complete, IORING_ENTER_GETEVENTS)
        );
        break;
    default:
        break;
    }

    return oe;
}

int
hm_ioring_user_data_complete(hm_user_data_t *user_data, hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;

    while (!hm_user_data_is_complete(user_data)) {
        HM_OE(oe, hm_ioring_flush_cqes(1, ioring));
    }

LABEL_OUT:
    if (oe == HM_OE_NONE) {
        // io_uring_cqe res returns -errno in the res field if there are errors. We do the same.
        return user_data->cqe.res;
    } else if (oe == HM_OE_ERROR) {
        // Generic error, but this function returns -errno on failure. Just use the most appropriate
        // system error.
        return -EIO;
    } else {
        // oe was initialized from errno.
        return -oe;
    }
}

hm_opt_error_t
hm_ioring_nop_submit(hm_user_data_t **user_data, hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;
    struct io_uring_sqe *sqe;
    HM_OE(oe, hm_ioring_get_sqe(&sqe, ioring));

    *user_data = hm_user_data_create();
    (*user_data)->opcode = IORING_OP_NOP;

    sqe->user_data = (uint64_t)(*user_data);
    sqe->opcode = IORING_OP_NOP;

LABEL_OUT:
    return oe;
}

hm_opt_error_t
hm_ioring_open_submit(hm_user_data_t **user_data, uint8_t *pathname, int flags, mode_t mode,
  hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;
    struct io_uring_sqe *sqe;
    HM_OE(oe, hm_ioring_get_sqe(&sqe, ioring));

    *user_data = hm_user_data_create();
    (*user_data)->opcode = IORING_OP_OPENAT;
    (*user_data)->pathname = pathname;

    sqe->user_data = (uint64_t)(*user_data);
    sqe->opcode = IORING_OP_OPENAT;
    sqe->fd = AT_FDCWD;
    sqe->addr = (uint64_t)pathname;
    sqe->open_flags = flags;
    sqe->len = mode;

LABEL_OUT:
    return oe;
}

hm_opt_error_t
hm_ioring_close_submit(hm_user_data_t **user_data, int fd, hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;
    struct io_uring_sqe *sqe;
    HM_OE(oe, hm_ioring_get_sqe(&sqe, ioring));

    *user_data = hm_user_data_create();
    (*user_data)->opcode = IORING_OP_CLOSE;

    sqe->user_data = (uint64_t)(*user_data);
    sqe->opcode = IORING_OP_CLOSE;
    sqe->fd = fd;

LABEL_OUT:
    return oe;
}

hm_opt_error_t
hm_ioring_read_submit(hm_user_data_t **user_data, int fd, uint8_t *buffer, uint64_t n,
  hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;
    struct io_uring_sqe *sqe;
    HM_OE(oe, hm_ioring_get_sqe(&sqe, ioring));

    *user_data = hm_user_data_create();
    (*user_data)->opcode = IORING_OP_READ;
    (*user_data)->buffer = buffer;

    sqe->user_data = (uint64_t)(*user_data);
    sqe->opcode = IORING_OP_READ;
    sqe->off = -1;  // Use current file position.
    sqe->fd = fd;
    sqe->addr = (uint64_t)buffer;
    sqe->len = n;

LABEL_OUT:
    return oe;
}

hm_opt_error_t
hm_ioring_write_submit(hm_user_data_t **user_data, int fd, uint8_t *buffer, uint64_t n,
  hm_ioring_t *ioring) {
    hm_opt_error_t oe = HM_OE_NONE;
    struct io_uring_sqe *sqe;
    HM_OE(oe, hm_ioring_get_sqe(&sqe, ioring));

    *user_data = hm_user_data_create();
    (*user_data)->opcode = IORING_OP_WRITE;
    (*user_data)->buffer = buffer;

    sqe->user_data = (uint64_t)(*user_data);
    sqe->opcode = IORING_OP_WRITE;
    sqe->off = -1;  // Use current file position.
    sqe->fd = fd;
    sqe->addr = (uint64_t)buffer;
    sqe->len = n;

LABEL_OUT:
    return oe;
}
