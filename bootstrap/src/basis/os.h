#pragma once

CAMLprim value hm_basis_os_error_to_string_get_length(value a_error);
CAMLprim value hm_basis_os_error_to_string_inner(value a_n, value a_bytes, value a_error);
