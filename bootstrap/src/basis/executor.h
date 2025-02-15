#pragma once
#include "ioring.h"

typedef struct {
    hemlock_ioring_t ioring;
} hemlock_executor_t;

hemlock_opt_error_t hemlock_executor_setup(hemlock_executor_t *executor);
void hemlock_executor_teardown(hemlock_executor_t *executor);
hemlock_executor_t *hemlock_executor_get();

CAMLprim value hemlock_basis_executor_finalize_result(int result);
CAMLprim value hemlock_basis_executor_user_data_decref(value a_user_data);
CAMLprim value hemlock_basis_executor_user_data_pp(value a_fd, value a_user_data);
CAMLprim value hemlock_basis_executor_complete_inner(value a_user_data);
CAMLprim value hemlock_basis_executor_submit_out(
    hemlock_opt_error_t oe, hemlock_user_data_t *user_data
);
CAMLprim value hemlock_basis_executor_nop_submit_inner(value a_unit);
CAMLprim value hemlock_basis_executor_setup_inner(value a_unit);
CAMLprim value hemlock_basis_executor_teardown_inner(value a_unit);
CAMLprim value hemlock_basis_executor_cqring_pp(value a_fd);
CAMLprim value hemlock_basis_executor_sqring_pp(value a_fd);
CAMLprim value hemlock_basis_executor_ioring_pp(value a_fd);
