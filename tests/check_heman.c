#include <stdlib.h>
#include <stdio.h>
#include <check.h>
#include <assert.h>
#include <sys/time.h>
#include "../src/hashtable.h"
#include "../src/stats.h"
#include "../src/rules.h"

START_TEST (stats_empty) {
	struct hashtable *a = NULL;
	fail_unless(a == NULL, "empty hashtables are NULL.");
	fail_unless(last_for_nsk(a, "foo", "bar") == NULL, "empty hts can't return keys.");
} END_TEST

START_TEST (stats_single) {
	struct hashtable *b = NULL;
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

START_TEST (stats_several) {
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

START_TEST (stats_over_time) {
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

START_TEST (rules_empty) {
	struct hashtable *a = NULL;
	fail_unless(a == NULL, "empty hashtables are NULL.");
} END_TEST

START_TEST (rules_basic) {
	struct hashtable *b = NULL;
	b = create_rule(b, "foo", "bar", 1);
	fail_if(b == NULL, "Creating a rule should populate the hashtable.");
	printf("Rule created\n");
	fail_unless(rule_op(b, "foo", "bar") == 1, "rule op is 1");
	b = create_rule(b, "foo", "baz", 2);
	fail_unless(rule_op(b, "foo", "baz") == 2, "rule op for foo:baz is 2");
} END_TEST

Suite *heman_suite(void) {
	Suite *s = suite_create("Heman");
	TCase *tc_stats = tcase_create("Stats");
	tcase_add_test(tc_stats, stats_empty);
	tcase_add_test(tc_stats, stats_single);
	tcase_add_test(tc_stats, stats_several);
	tcase_add_test(tc_stats, stats_over_time);
	TCase *tc_rules = tcase_create("Rules");
	tcase_add_test(tc_rules, rules_empty);
	tcase_add_test(tc_rules, rules_basic);
	suite_add_tcase(s, tc_stats);
	suite_add_tcase(s, tc_rules);
	return s;
}

int main (void) {
	int number_failed;
	Suite *s = heman_suite();
	SRunner *sr = srunner_create(s);
	srunner_run_all(sr, CK_NORMAL);
	number_failed = srunner_ntests_failed(sr);
	srunner_free(sr);
	return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
