-record(rule, {key, rule, display_name}).
-record(stat, {pkey, fordate, namespace, key, value}).
-record(health, {pkey, namespace, priority, key, rules}).
-record(log, {pkey, fordate, namespace, health, messages}).
