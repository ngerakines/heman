/*
Copyright (c) 2010 Nick Gerakines <nick at gerakines dot net>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include "hashtable.h"
#include "rules.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

DEFINE_HASHTABLE_INSERT(insert_rule, struct rule_key, struct rule_value);
DEFINE_HASHTABLE_SEARCH(search_rules, struct rule_key, struct rule_value);

unsigned int hash_rule(void *ky) {
	RuleKey k = (RuleKey)ky;
	unsigned int hash = 0;
	char *s = k->namespace;
	for(; *s; ++s) {
		hash += *s;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}
	s = k->key;
	for(; *s; ++s) {
		hash += *s;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}
	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);
	return hash;
}

int equal_rules(void *k1, void *k2) {
	return (0 == memcmp(k1, k2, sizeof(struct rule_key)));
}

Rules create_rule(Rules h, char *namespace, char *key, int op) {
	if (h == NULL) {
		h = create_hashtable(16, hash_rule, equal_rules);
		if (NULL == h) {
			// NKG: Should I be calling `exit(-1)` here?
			exit(-1);
		}
	}

	RuleKey new_rule_key;
	new_rule_key = (RuleKey)malloc(sizeof(struct rule_key));
	if (NULL == new_rule_key) {
		// NKG: Should I just call `exit(1)` here?
		return NULL;
	}
	new_rule_key->namespace = namespace;
	new_rule_key->key = key;

	Rule rule;

	if (NULL == (rule = search_rules(h, new_rule_key))) {
		rule = (Rule)malloc(sizeof(struct rule_value));
		rule->namespace = namespace;
		rule->key = key;
		rule->op = op;
		if (hashtable_insert(h, new_rule_key, rule) != -1) {
			// NKG: Should I be calling `exit(-1)` here?
			exit(-1);
		}
	}

	return h;
}

int rule_op(Rules h, char *namespace, char *key) {
	if (h == NULL) {
		return 1;
	}

	RuleKey search_key;
	search_key = (RuleKey)malloc(sizeof(struct rule_key));
	if (search_key == NULL) {
		return 1;
	}
	search_key->namespace = namespace;
	search_key->key = key;

	Rule rule;
	rule = search_rules(h, search_key);
	free(search_key);
	if (rule == NULL) {
		return 1;
	}

	return rule->op;
}
