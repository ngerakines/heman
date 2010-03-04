#include <stdlib.h>
#include <stdio.h>
#include <check.h>
#include <assert.h>
#include <sys/time.h>
#include "../src/hashtable.h"
#include "../src/store.h"

struct ll_stat {
	int value;
	time_t when;
	StatNode next;
};

START_TEST (test_empty_stats) {
	struct hashtable *a = NULL;
	fail_unless(a == NULL, "empty hashtables are NULL.");
	fail_unless(last_for_nsk(a, "foo", "bar") == NULL, "empty hts can't return keys.");
} END_TEST

START_TEST (test_single_stat) {
	struct hashtable *b = NULL;
	time_t now;
	time(&now);

	b = add_stat(b, "foo", "bar", 1);
	fail_if(b == NULL, "creating a stat shouldn't return null");
	StatNode last = NULL;
	last = last_for_nsk(b, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 1, "value should be one");

	b = add_stat(b, "foo", "bar", 1);
	fail_if(b == NULL, "creating a stat shouldn't return null");
	last = last_for_nsk(b, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 2, "value should be one");
} END_TEST

START_TEST (test_several_stat) {
	struct hashtable *b = NULL;
	b = add_stat(b, "foo", "bar", 1);
	b = add_stat(b, "foo", "bar", 1);
	b = add_stat(b, "foo", "baz", 1);
	b = add_stat(b, "foo", "baz", 1);
	b = add_stat(b, "foo", "baz", 1);
	StatNode last = NULL;
	last = last_for_nsk(b, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 2, "foo:bar should be two");
	last = last_for_nsk(b, "foo", "baz");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 3, "foo:baz should be three");
} END_TEST

START_TEST (test_stats_over_time) {
	struct hashtable *c = NULL;
	c = add_stat(c, "foo", "bar", 5);
	sleep(1);
	c = add_stat(c, "foo", "bar", 3);
	StatNode last = NULL;
	last = last_for_nsk(c, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 3, "foo:bar should be three");
	sleep(1);
	c = add_stat(c, "foo", "bar", 7);
	last = last_for_nsk(c, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 7, "foo:bar should be seven");
} END_TEST

Suite * heman_suite(void) {
	Suite *s = suite_create("Heman");
	TCase *tc_core = tcase_create("Core");
	tcase_add_test(tc_core, test_empty_stats);
	tcase_add_test(tc_core, test_single_stat);
	tcase_add_test(tc_core, test_several_stat);
	tcase_add_test(tc_core, test_stats_over_time);
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
