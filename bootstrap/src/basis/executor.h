#pragma once
#include "ioring.h"

typedef struct {
    hm_ioring_t ioring;
} hm_executor_t;

hm_opt_error_t hm_executor_setup(hm_executor_t *executor);
void hm_executor_teardown(hm_executor_t *executor);
hm_executor_t *hm_executor_get();

CAMLprim value hm_basis_executor_finalize_result(int result);
CAMLprim value hm_basis_executor_user_data_decref(value a_user_data);
CAMLprim value hm_basis_executor_user_data_pp(value a_fd, value a_user_data);
CAMLprim value hm_basis_executor_complete_inner(value a_user_data);
CAMLprim value hm_basis_executor_submit_out(hm_opt_error_t oe, hm_user_data_t *user_data);
CAMLprim value hm_basis_executor_nop_submit_inner(value a_unit);
CAMLprim value hm_basis_executor_setup_inner(value a_unit);
CAMLprim value hm_basis_executor_teardown_inner(value a_unit);
CAMLprim value hm_basis_executor_cqring_pp(value a_fd);
CAMLprim value hm_basis_executor_sqring_pp(value a_fd);
CAMLprim value hm_basis_executor_ioring_pp(value a_fd);
