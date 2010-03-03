#include <stdlib.h>
#include <stdio.h>
#include <check.h>
#include <assert.h>

Suite * heman_suite(void) {
	Suite *s = suite_create("Heman");
	TCase *tc_core = tcase_create("Core");
	// tcase_add_test(tc_core, test_pools_empty);
	suite_add_tcase(s, tc_core);
	return s;
}

int main (void) {
	int number_failed;
	Suite *s = heman_suite();
	SRunner *sr = srunner_create(s);
	srunner_run_all(sr, CK_VERBOSE);
	number_failed = srunner_ntests_failed(sr);
	srunner_free(sr);
	return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
