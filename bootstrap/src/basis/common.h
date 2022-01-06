#pragma once
#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef enum {
    HM_OE_ERROR = -1,
    HM_OE_NONE
    // All values > 0 are specific linux errno.
} hm_opt_error_t;


#define HM_OE(oe, statement) do { \
    oe = statement; \
    if (oe != HM_OE_NONE) { \
        fprintf(stderr, "Error %s:%i\n", __FILE__, __LINE__); \
        goto LABEL_OUT; \
    }; \
} while (0)

#define HM_OE_ERRNO_RESULT(oe, result, statement) do { \
    result = statement; \
    if ((int64_t)result == -1) { \
        HM_OE(oe, errno); \
    }; \
} while (0)
