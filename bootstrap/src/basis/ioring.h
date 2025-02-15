#pragma once
#include <linux/io_uring.h>
#include <sys/types.h>

// User_data structure to be used in the `user_data` field of a `struct io_uring_sqe` submission and
// returned in the `user_data` field of the associated `struct io_uring_cqe`.
typedef struct {
    // We aggressively copy CQEs from the kernel to here to avoid blocking completions.
    struct io_uring_cqe cqe;
    union {
        // For `read` or `write` operations.
        uint8_t *buffer;

        // For `open` operation.
        uint8_t *pathname;
    };

    // Keeping track of the associated opcode preserves enough information to safely free buffers
    // upon completion of some operations.
    uint8_t opcode;

    // Maximum of two refs. One from OCaml and one from the kernel.
    uint8_t refcount;
} hemlock_user_data_t;
void hemlock_user_data_pp(int fd, int indent, hemlock_user_data_t *user_data);
void hemlock_user_data_decref(hemlock_user_data_t *user_data);

// Utility type for tracking submission queue mmapped data structure fields.
typedef struct {
    unsigned *head;
    unsigned *tail;
    unsigned *ring_entries;
    unsigned *ring_mask;
    unsigned *flags;
    unsigned *array;
    struct io_uring_sqe *sqes;
} hemlock_sqring_t;
void hemlock_sqring_pp(int fd, int indent, hemlock_sqring_t *sqring);

// Utility type for tracking completion queue mmapped data structure fields.
typedef struct {
    unsigned *head;
    unsigned *tail;
    unsigned *ring_entries;
    unsigned *ring_mask;
    struct io_uring_cqe *cqes;
} hemlock_cqring_t;
void hemlock_cqring_pp(int fd, int indent, hemlock_cqring_t *cqring);

// Utility type for tracking io_uring fd and mmapped data structure fields.
typedef struct {
    // Parameters for constructing io_uring instance.
    struct io_uring_params params;

    // File descriptor associated with io_uring instance.
    int fd;

    // mmapped memory for io_uring instance.
    void *vm;

    hemlock_sqring_t sqring;
    hemlock_cqring_t cqring;
} hemlock_ioring_t;
void hemlock_ioring_pp(int fd, int indent, hemlock_ioring_t *ioring);
hemlock_opt_error_t hemlock_ioring_setup(hemlock_ioring_t *ioring);
void hemlock_ioring_teardown(hemlock_ioring_t *ioring);
hemlock_opt_error_t hemlock_ioring_enter(
    uint32_t *n_complete,
    uint32_t min_complete,
    hemlock_ioring_t *ioring
);
int hemlock_ioring_user_data_complete(hemlock_user_data_t *user_data, hemlock_ioring_t *ioring);
hemlock_opt_error_t hemlock_ioring_nop_submit(
    hemlock_user_data_t **user_data,
    hemlock_ioring_t *ioring
);
hemlock_opt_error_t hemlock_ioring_open_submit(
    hemlock_user_data_t **user_data,
    uint8_t *pathname,
    int flags,
    mode_t mode,
    hemlock_ioring_t *ioring
);
hemlock_opt_error_t hemlock_ioring_close_submit(
    hemlock_user_data_t **user_data,
    int fd,
    hemlock_ioring_t *ioring
);
hemlock_opt_error_t hemlock_ioring_read_submit(
    hemlock_user_data_t **user_data,
    int fd,
    uint8_t *buffer,
    uint64_t n,
    hemlock_ioring_t *ioring
);
hemlock_opt_error_t hemlock_ioring_write_submit(
    hemlock_user_data_t **user_data,
    int fd,
    uint8_t *buffer,
    uint64_t n,
    hemlock_ioring_t *ioring
);
