#include <threads.h>

#include "common.h"
#include "ioring.h"

#include "executor.h"

thread_local hm_executor_t executor = {0};

hm_opt_error_t
hm_executor_setup(hm_executor_t *executor) {
    return hm_ioring_setup(&executor->ioring);
}

void
hm_executor_teardown(hm_executor_t *executor) {
    hm_ioring_teardown(&executor->ioring);
}

hm_executor_t *
hm_executor_get() {
    return &executor;
}
