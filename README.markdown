# About

Heman is an application health monitoring system for application-level health.

# Goals

 * Provide short-term (1 hour, 3 hour, 12 hour, 24 hour, 48 hour) views of application specific usage.
 * Allow developers to create 'rules' that help discern overall application health.
 * Create a clean, easy to read interface to convey application health.

Heman isn't intended to be used as a nagios or cacti replacement. This project is inspired by the [Facebook Platform status page](http://developers.facebook.com/live_status.php).

# The Basics

A *rule* is a stat or data-point that is traced. It could be an incrementing rule that counts the number of users processed by a queue worker or the maximum size of a queue. There are different types of rules like increment, set, maximum and minimum that can be used. Rules are organized by namespace and have user-friendly descriptions.

    1> heman:rule_set({<<"cerlan_data">>, <<"users_processed">>}, increase, "Users Processed").
    ok
    1> heman:rule_set({<<"cerlan_data">>, <<"user_queue">>}, max, "User Queue Max").
    ok

A *stat* represents a given data-point that is applicable to a rule. All data collected by stat calls are aggregated into the minute that it was received. This gives us a measurable window of time in which we can apply health checks. By default, if a stat is set for a namespace and key that isn't know, it is recorded and the default 'increase' type is applied.

    1> heman:stat_set(<<"cerlan_data">>, <<"users_processed">>, 1).
    ok
    ...
    403> heman:stat_set(<<"cerlan_data">>, <<"users_processed">>, 1).
    ok

*Health* is a score between 0 and 100 that is derived from the rules that exist for a namespace and the data collected through stat calls within that namespace. A namespace is considered to be in "poor" health when the health score drops below 50, "ok" health at below 70 and "good" health above 70. This scale is subject to change.

    1> Rules = [{{hours, 1, sum}, {under, 100}, {decrease, 20}}, {{hours, 1, sum}, {over, 200}, {increase, 20}}].
    [{{hours, 1, sum}, {under, 100}, {decrease, 20}}, {{hours, 1, sum}, {over, 200}, {increase, 20}}]
    2> heman:health_set(<<"cerlan_data">>, 1, <<"users_processed">>, Rules).
    ok
    3> heman:health(<<"cerlan_data">>).
    70

Lastly, the web interface is provided to give a quick view into the health of the namespaces tracked. The web interface generates an event log every few minutes and can show recent trends.

# Non-Erlang Implementation

Heman provides a binary memcached service that can be accessed by most current Memcached clients.

# Change log and development plan

0.0.1: Released

* Base framework
* Memcached protocol
* Basic namespaces
* Basic rules and health rules
* Basic web pages

0.0.2: In Development

* Aggregate health rules: e.g. When sum of a over 2 hours is greater than the sum of b over 2 hours then execute on X.
* Regular tasks to survey health: Poll namespaces every n minutes and store health and health reasons.
* Health logs: When health is surveyed, create logs of the namespace, score and reasons.
* Data purging: Delete all data older than N hours. 