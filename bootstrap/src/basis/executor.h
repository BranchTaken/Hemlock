#pragma once
#include "ioring.h"

typedef struct {
    hm_ioring_t ioring;
} hm_executor_t;

hm_opt_error_t hm_executor_setup(hm_executor_t *executor);
void hm_executor_teardown(hm_executor_t *executor);
hm_executor_t *hm_executor_get();
