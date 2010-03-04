
#include "hashtable.h"
#include "store.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

DEFINE_HASHTABLE_INSERT(insert_some, struct store_key, struct store_value);
DEFINE_HASHTABLE_SEARCH(search_some, struct store_key, struct store_value);
DEFINE_HASHTABLE_REMOVE(remove_some, struct store_key, struct store_value);

static unsigned int hash_from_key(void *ky) {
	struct store_key *k = (struct store_key *)ky;
	unsigned int hash = 0;
	int c;
	while (c = *k->namespace++) {
		hash = c + (hash << 6) + (hash << 16) - hash;
	}
	while (c = *k->key++) {
		hash = c + (hash << 6) + (hash << 16) - hash;
	}
	return hash;
}

static int equal_keys(void *k1, void *k2) {
	return (0 == memcmp(k1, k2, sizeof(struct store_key)));
}

struct hashtable *add_stat(struct hashtable *h, char *namespace, char *key, int value, time_t when) {

	if (h == NULL) {
		h = create_hashtable(16, hash_from_key, equal_keys);
		if (NULL == h) {
			exit(-1);
		}
	}

	struct store_key *new_key;
	new_key = (struct store_key *)malloc(sizeof(struct store_key));
	if (NULL == new_key) {
		printf("ran out of memory allocating a key\n");
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
		if (! insert_some(h, new_key, key_value)) {
			exit(-1);
		}
	}

	// We've got a valid key_value ref, now add the new stat. For now assume
	// all ops are 'INCR'.
	if (key_value->stats == NULL) {
		printf("key_value->stats is NULL\n");
		key_value->stats = create_stat(value, when);
	} else {
		// NKG: This is broken.
		time_t now;
		time(&now);
		if (key_value->stats->when == now) {
			printf("Incrementing value by %d\n", value);
			key_value->stats += value;
		} else {
			printf("push new stat node onto key_value\n");
			key_value->stats = push_stat(key_value->stats, value, when);
		}
	}

	return h;
}

StatNode *create_stat(int value, time_t when) {
	StatNode *node;
	if (! (node = malloc(sizeof(StatNode)))) {
		return NULL;
	}
	node->value = value;
	node->when = when;
	node->next = NULL;
	return node;
}

StatNode *push_stat(StatNode *head, int value, time_t when) {
	StatNode *new_head;
	new_head = create_stat(value, when);
	new_head->next = head;
	return new_head;
}

StatNode *last_for_nsk(struct hashtable *hash_table, char *namespace, char *key) {
	if (hash_table == NULL) {
		printf("hash_table is NULL");
		return NULL;
	}
	struct store_key *search_key;
	search_key = (struct store_key *)malloc(sizeof(struct store_key));
	if (search_key == NULL) {
		printf("ran out of memory allocating a key\n");
		return NULL;
	}
	search_key->namespace = namespace;
	search_key->key = key;

	struct store_value *key_value;

	if (NULL == (key_value = search_some(hash_table, search_key))) {
		free(search_key); // right?
		printf("search_key found nothing... %s %s\n", namespace, key);
		return NULL;
	}
	free(search_key); // right?

	return key_value->stats;
}