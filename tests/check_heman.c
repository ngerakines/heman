#include <stdlib.h>
#include <stdio.h>
#include <check.h>
#include <assert.h>
#include "../src/hashtable.h"
#include "../src/store.h"

START_TEST (test_empty_stats) {
	struct hashtable *a = NULL;
	fail_unless(a == NULL, "empty hashtables are NULL.");
	fail_unless(last_for_nsk(a, "foo", "bar") == NULL, "empty hts can't return keys.");
} END_TEST

START_TEST (test_single_stat) {
	struct hashtable *b = NULL;
	time_t now;
	time(&now);
	fail_unless((b = add_stat(b, "foo", "bar", 1, now)) == NULL, "creating a stat");
	StatNode *last;
	last = last_for_nsk(b, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 1, "value should be one");

	fail_unless((b = add_stat(b, "foo", "bar", 1, now)) == NULL, "hitting the stat");
	last = last_for_nsk(b, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 2, "value should be one");
} END_TEST

Suite * heman_suite(void) {
	Suite *s = suite_create("Heman");
	TCase *tc_core = tcase_create("Core");
//	tcase_add_test(tc_core, test_empty_stats);
	tcase_add_test(tc_core, test_single_stat);
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
