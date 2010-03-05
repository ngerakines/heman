#include <stdlib.h>
#include <stdio.h>
#include <check.h>
#include <assert.h>
#include <sys/time.h>
#include <unistd.h>
#include "../src/hashtable.h"
#include "../src/heman_types.h"
#include "../src/stats.h"
#include "../src/rules.h"
#include "../src/health.h"

START_TEST (stats_empty) {
	struct hashtable *a = NULL;
	fail_unless(a == NULL, "empty hashtables are NULL.");
	fail_unless(last_for_nsk(a, "foo", "bar") == NULL, "empty hts can't return keys.");
} END_TEST

START_TEST (stats_single) {
	Stats b = NULL;
	b = add_stat(b, "foo", "bar", 1, 1);
	fail_if(b == NULL, "creating a stat shouldn't return null");
	StatNode last = NULL;
	last = last_for_nsk(b, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 1, "value should be one");
	b = add_stat(b, "foo", "bar", 1, 1);
	fail_if(b == NULL, "creating a stat shouldn't return null");
	last = last_for_nsk(b, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 2, "value should be one");
} END_TEST

START_TEST (stats_several) {
	Stats b = NULL;
	b = add_stat(b, "foo", "bar", 1, 1);
	b = add_stat(b, "foo", "bar", 1, 1);
	b = add_stat(b, "foo", "baz", 1, 1);
	b = add_stat(b, "foo", "baz", 1, 1);
	b = add_stat(b, "foo", "baz", 1, 1);
	StatNode last = NULL;
	last = last_for_nsk(b, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 2, "foo:bar should be two");
	last = last_for_nsk(b, "foo", "baz");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 3, "foo:baz should be three");
} END_TEST

START_TEST (stats_over_time) {
	Stats c = NULL;
	c = add_stat(c, "foo", "bar", 5, 1);
	sleep(1);
	c = add_stat(c, "foo", "bar", 3, 1);
	StatNode last = NULL;
	last = last_for_nsk(c, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 3, "foo:bar should be three");
	sleep(1);
	c = add_stat(c, "foo", "bar", 7, 1);
	last = last_for_nsk(c, "foo", "bar");
	fail_if(last == NULL, "ns/key should be not NULL");
	fail_unless(last->value == 7, "foo:bar should be seven");
} END_TEST

START_TEST (rules_empty) {
	Rules a = NULL;
	fail_unless(a == NULL, "empty hashtables are NULL.");
} END_TEST

START_TEST (rules_basic) {
	Rules b = NULL;
	b = create_rule(b, "foo", "bar", 1);
	fail_if(b == NULL, "Creating a rule should populate the hashtable.");
	fail_unless(rule_op(b, "foo", "bar") == 1, "rule op is 1");
	b = create_rule(b, "foo", "baz", 2);
	fail_unless(rule_op(b, "foo", "baz") == 2, "rule op for foo:baz is 2");
} END_TEST

START_TEST (health_empty) {
	HRules a = NULL;
	fail_unless(a == NULL, "empty hashtables are NULL.");
} END_TEST

START_TEST (health_basic) {
	HRules c = NULL;
	fail_unless(hrule_exists(c, "has_activity") == 0, "no rules should exist");
	c = add_hrule(c, "has_activity", "foo", "bar", "none", "none");
	fail_if(c == NULL, "Creating a rule should populate the hashtable.");
	fail_unless(hrule_exists(c, "has_activity") == 1, "rule exists");
	fail_unless(hrule_exists(c, "has_activity2") == 0, "rule does not exist");
	fail_unless(hrules_for_namespace(c, "foo") == 1, "health rule count of 1 is returned");
	c = add_hrule(c, "moar_activity_pls", "foo", "bar", "none", "none");
	fail_unless(hrule_exists(c, "moar_activity_pls") == 1, "rule does exist");
	fail_unless(hrules_for_namespace(c, "foo") == 2, "health rule count of 2 is returned");
} END_TEST

START_TEST (health_calc) {
	HRules health_rules = NULL;
	Stats stats = NULL;
	stats = add_stat(stats, "foo", "bar", 1, 1);
	stats = add_stat(stats, "foo", "bar", 1, 1);
	stats = add_stat(stats, "foo", "baz", 1, 1);
	stats = add_stat(stats, "foo", "baz", 1, 1);
	stats = add_stat(stats, "foo", "baz", 1, 1);
	health_rules = add_hrule(health_rules, "has_bar", "foo", "bar", "test_1.lua", "none");
	fail_if(stats == NULL, "stats should have values");
	fail_if(health_rules == NULL, "health_rules should have values");
	Health health;
	health = health_for_namespace(health_rules, "foo", stats);
	fail_unless(health->good == 1, "good is 0");
	fail_unless(health->ok == 0, "ok is 0");
	fail_unless(health->bad == 0, "bad is 0");
	free(health); health = NULL;
	sleep(1);
	stats = add_stat(stats, "foo", "bar", 1, 1);
	stats = add_stat(stats, "foo", "bar", 1, 1);
	stats = add_stat(stats, "foo", "bar", 1, 1);
	stats = add_stat(stats, "foo", "bar", 1, 1);
	health = health_for_namespace(health_rules, "foo", stats);
	fail_unless(health->good == 0, "good is 0");
	fail_unless(health->ok == 1, "ok is 0");
	fail_unless(health->bad == 0, "bad is 0");
	free(health); health = NULL;
	sleep(1);
	stats = add_stat(stats, "foo", "bar", 5, 1);
	health = health_for_namespace(health_rules, "foo", stats);
	fail_unless(health->good == 0, "good is 0");
	fail_unless(health->ok == 0, "ok is 0");
	fail_unless(health->bad == 1, "bad is 0");
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
	TCase *tc_health = tcase_create("Health");
	tcase_add_test(tc_health, health_empty);
	tcase_add_test(tc_health, health_basic);
	tcase_add_test(tc_health, health_calc);
	suite_add_tcase(s, tc_stats);
	suite_add_tcase(s, tc_rules);
	suite_add_tcase(s, tc_health);
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
