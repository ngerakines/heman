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
#include "stats.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "hashtable_itr.h"

DEFINE_HASHTABLE_SEARCH(search_health, struct hrule_key, struct hrule_value);

// NKG: Yeah, so, this is the best I could do.
unsigned int hash_hrule(void *ky) {
	struct hrule_key *k = (struct hrule_key *)ky;
	const char * s = k->name;
	unsigned int hash = 0;
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

int equal_hrules(void *k1, void *k2) {
	return (0 == memcmp(k1, k2, sizeof(struct hrule_key)));
}

HRules add_hrule(HRules h, char *name, char *namespace, char *key, char *lua, char *transforms) {
	if (h == NULL) {
		h = create_hashtable(16, hash_hrule, equal_hrules);
		if (NULL == h) {
			// NKG: Should I be calling `exit(-1)` here?
			exit(-1);
		}
	}

	HRuleKey hkey;
	hkey = (HRuleKey)malloc(sizeof(struct hrule_key));
	if (NULL == hkey) {
		// NKG: Should I just call `exit(1)` here?
		return NULL;
	}
	hkey->name = name;

	HRule value;

	if (NULL == (value = search_health(h, hkey))) {
		value = (HRule)malloc(sizeof(struct hrule_value));
		value->namespace = namespace;
		value->key = key;
		value->lua = lua;
		value->transforms = transforms;
		if (hashtable_insert(h, hkey, value) != -1) {
			// NKG: Should I be calling `exit(-1)` here?
			exit(-1);
		}
	}
	return h;
}

int hrules_for_namespace(HRules h, char *namespace) {
	struct hashtable_itr *iter;
	HRule rule;
	iter = hashtable_iterator(h);
	int i = 0;
	if (hashtable_count(h) > 0) {
		do {
			rule = hashtable_iterator_value(iter);
			if (rule->namespace == namespace) {
				i++;
			}
		} while (hashtable_iterator_advance(iter));
	}
	return i;
}