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

#ifndef __STATS_H__
#define __STATS_H__

#include <sys/time.h>
#include "hashtable.h"

struct ll_stat;
typedef struct ll_stat *StatNode;

struct store_key {
	char *namespace;
	char *key;
};

struct store_value {
	char *namespace;
	char *key;
	int count;
	StatNode stats;
};

unsigned int hash_stat(void *ky);
int equal_stats(void *k1, void *k2);
struct hashtable *add_stat(struct hashtable *h, char *namespace, char *key, int value);

StatNode create_stat(int value, time_t when);
StatNode push_stat(StatNode head, int value, time_t when);
StatNode last_for_nsk(struct hashtable *hash_table, char *namespace, char *key);

#endif
