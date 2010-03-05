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

#ifndef __HEMAN_TYPES_H__
#define __HEMAN_TYPES_H__

#include <sys/time.h>
#include "hashtable.h"

// -- stats

struct ll_stat;
typedef struct ll_stat *StatNode;

struct ll_stat {
	int value;
	time_t when;
	StatNode next;
};

struct stat_key {
	char *namespace;
	char *key;
};

struct stat_value {
	char *namespace;
	char *key;
	int count;
	StatNode stats;
};

typedef struct stat_key *StatKey;
typedef struct stat_value *Stat;

// -- rules

struct rule_key {
	char *namespace;
	char *key;
};

struct rule_value {
	char *namespace;
	char *key;
	int op;
};

typedef struct rule_key *RuleKey;
typedef struct rule_value *Rule;

// -- hrules

struct hrule_key {
	char *name;
};

struct hrule_value {
	char *lua;
	char *namespace;
	char *key;
	char *transforms;
};

typedef struct hrule_key *HRuleKey;
typedef struct hrule_value *HRule;

struct struct_health {
	int good;
	int ok;
	int bad;
};

typedef struct struct_health *Health;

// -- containers

typedef struct hashtable *Rules;
typedef struct hashtable *Stats;
typedef struct hashtable *HRules;

#endif
