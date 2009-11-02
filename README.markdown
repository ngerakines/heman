# About

Heman is an application health monitoring system for application-level health. This project is alpha quality software and shouldn't be used yet.

# Goals

 * Provide short-term (1 hour, 3 hour, 12 hour, 24 hour, 48 hour) views of application specific usage.
 * Allow developers to create 'rules' that help discern overall application health.
 * Create a clean, easy to read interface to describe application health.

*What it is NOT*:

 * A throttling system or framework.
 * A nagios or cacti replacement.

# How does it work?

The key concept here are stats and how they affect the health score of an application. When heman is started, a number of rules are registered that tell heman how to process certain stats. An example rule could be "queuelength" under the namespace "queues" with a rule type of "replace".

As your application operates, it would call the heman:set/3 function to register important stats. For example, every few executions of a queue worker could make a call to `heman:set("queue", "queuelength", N)` to register the current queue length.

There is another set of rules called "health" that look at recent data and figure out how to assign a health score to a namespace. A health score is between 0 and 100, 0 being very unhealth or fatal and 100 being ideal. A health rule consists of a namespace, key, condition and a result. A condition may be something like "if over 100 return fatal" or "if under 10 add +10". By default, the health score for a namespace is 50 and the health rules can immediately return a score or increase/decrease the score.

Lastly, the web interface is provided to give a quick view into the health of the namespaces tracked. The web interface generates an event log every few minutes and can show recent trends.

# Tasks

 * Create process and code that periodically prunes old data.
 * Create event log and process health periodically into that log.

