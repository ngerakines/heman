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
#include "store.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct ll_stat {
	int value;
	time_t when;
	StatNode next;
};

DEFINE_HASHTABLE_INSERT(insert_some, struct store_key, struct store_value);
DEFINE_HASHTABLE_SEARCH(search_some, struct store_key, struct store_value);
DEFINE_HASHTABLE_REMOVE(remove_some, struct store_key, struct store_value);

// NKG: Yeah, so, this is the best I could do.
unsigned int hash_from_key(void *ky) {
	struct store_key *k = (struct store_key *)ky;
	const char * s = k->key; // Need to account for namespace too.
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

static int equal_keys(void *k1, void *k2) {
	return (0 == memcmp(k1, k2, sizeof(struct store_key)));
}

struct hashtable *add_stat(struct hashtable *h, char *namespace, char *key, int value) {
	if (h == NULL) {
		h = create_hashtable(16, hash_from_key, equal_keys);
		if (NULL == h) {
			// NKG: Should I be calling `exit(-1)` here?
			exit(-1);
		}
	}

	struct store_key *new_key;
	new_key = (struct store_key *)malloc(sizeof(struct store_key));
	if (NULL == new_key) {
		// NKG: Should I just call `exit(1)` here?
		return NULL;
	}
	new_key->namespace = namespace;
	new_key->key = key;

	struct store_value *key_value;

	if (NULL == (key_value = search_some(h, new_key))) {
		key_value = (struct store_value *)malloc(sizeof(struct store_value));
		key_value->namespace = namespace;
		key_value->key = key;
		key_value->count = 1;
		key_value->stats = NULL;
		if (hashtable_insert(h, new_key, key_value) != -1) {
			// NKG: Should I be calling `exit(-1)` here?
			exit(-1);
		}
	}

	// We've got a valid key_value ref, now add the new stat. For now assume
	// all ops are 'INCR'.
	time_t now;
	time(&now);

	if (key_value->stats == NULL) {
		StatNode first;
		first = create_stat(value, now);
		key_value->stats = first;
	} else {
		if (key_value->stats->when == now) {
			key_value->stats->value += value;
		} else {
			key_value->stats = push_stat(key_value->stats, value, now);
		}
	}

	return h;
}

StatNode create_stat(int value, time_t when) {
	StatNode node;
	if (! (node = malloc(sizeof(StatNode)))) {
		return NULL;
	}
	node->value = value;
	node->when = when;
	node->next = NULL;
	return node;
}

StatNode push_stat(StatNode head, int value, time_t when) {
	StatNode new_head;
	new_head = create_stat(value, when);
	new_head->next = head;
	return new_head;
}

StatNode last_for_nsk(struct hashtable *hash_table, char *namespace, char *key) {
	if (hash_table == NULL) {
		return NULL;
	}
	struct store_key *search_key;
	search_key = (struct store_key *)malloc(sizeof(struct store_key));
	if (search_key == NULL) {
		// NKG: Should I just call `exit(1)` here?
		return NULL;
	}
	search_key->namespace = namespace;
	search_key->key = key;

	struct store_value *key_value;
	key_value = search_some(hash_table, search_key);
	if (key_value == NULL) {
		free(search_key);
		return NULL;
	}
	free(search_key);

	return key_value->stats;
}
