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
} hm_user_data_t;
void hm_user_data_pp(int fd, int indent, hm_user_data_t *user_data);
void hm_user_data_decref(hm_user_data_t *user_data);

// Utility type for tracking submission queue mmapped data structure fields.
typedef struct {
    unsigned *head;
    unsigned *tail;
    unsigned *ring_entries;
    unsigned *ring_mask;
    unsigned *flags;
    unsigned *array;
    struct io_uring_sqe *sqes;
} hm_sqring_t;
void hm_sqring_pp(int fd, int indent, hm_sqring_t *sqring);

// Utility type for tracking completion queue mmapped data structure fields.
typedef struct {
    unsigned *head;
    unsigned *tail;
    unsigned *ring_entries;
    unsigned *ring_mask;
    struct io_uring_cqe *cqes;
} hm_cqring_t;
void hm_cqring_pp(int fd, int indent, hm_cqring_t *cqring);

// Utility type for tracking io_uring fd and mmapped data structure fields.
typedef struct {
    // Parameters for constructing io_uring instance.
    struct io_uring_params params;

    // File descriptor associated with io_uring instance.
    int fd;

    // mmapped memory for io_uring instance.
    void *vm;

    hm_sqring_t sqring;
    hm_cqring_t cqring;
} hm_ioring_t;
void hm_ioring_pp(int fd, int indent, hm_ioring_t *ioring);
hm_opt_error_t hm_ioring_setup(hm_ioring_t *ioring);
void hm_ioring_teardown(hm_ioring_t *ioring);
hm_opt_error_t hm_ioring_enter(uint32_t *n_complete, uint32_t min_complete, hm_ioring_t *ioring);
int hm_ioring_user_data_complete(hm_user_data_t *user_data, hm_ioring_t *ioring);
hm_opt_error_t hm_ioring_nop_submit(hm_user_data_t **user_data, hm_ioring_t *ioring);
