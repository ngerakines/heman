Heman is a small, portable daemon that exposes an interface to help you expose and understand application statistics over time. The ultimate goal is to allow application developers and consumers to determine the health of an application.

Health is based on the number of rules that register bad, ok or good for a given namespace. The rules are registered Lua scripts for a given namespace key which allows developers and non-developers to easily create and manipulate rules.

# Usage

Heman stores rules on how to process incoming stats.

    rule<"app_data", "user_processed"> = <"integer", "The number of users processed by our batch processing system over time.">
    rule<"app_data", "users_per_batch"> = <"integer", "The number of users processed in a batch.">
    rule<"app_data", "batch_processing_time_spent"> = <"time", "The amount of time required to process a batch of users"

As your application does it's thing, periodically stats are sent to Heman.

    update<"app_data", "user_processed"> = +1
    update<"app_data", "user_processed"> = +1
    set<"app_data", "users_per_batch"> = 2
    set<"app_data", "batch_processing_time_spent"> = 15 minutes

Health is determined by a few things, but mainly the output of rules entered into the system. Rules are written in lua and look something like this:

    -- users_processed.lua
    function main(users_processed_today)
      if users_processed_today == 0 then
        return 1, "No users processed."
      else if users_processed_today < 50 then
        return 2, "Some users processed but not many."
      else if users_processed_today < 1000
        return 3, "Lots of users processed, all good."
      else
        return 2, "Too many users processed."
      end
    end
    main(input)

The output of the lua script directly impacts the health rule result. Two return values are expected by the application when executing a health script. The first is an integer representing the status where 1 is bad, 2 is ok and 3 is good. The second is a log message that is human readable.

When creating a health rule, some hints are given to let it know what data to pass in and what processing should be applied before so.

    health
      rule_name: "user_processed"
      lua: "users_processed.lua"
      requires: <"app_data", "user_processed">
      transform: sum value, range of 12 hours

    health
      rule_name: "users_per_batch"
      lua: "users_per_batch.lua"
      requires: <"app_data", "users_per_batch">
      transform: table

The above hints used are 'sum value', 'range of N' and 'table'. The 'sum value' hint tells Heman to sum all of the collected data for the app\_data:user_processed namespace key. The 'range of N' says that only the past 12 hours of data needs to be collected and transformed. The 'table' hint is the default rule whereby all of the data is put into a Lua table in the form of a dictionary whereby the keys are the timestamps and the values are the values.

Lastly, once the system is setup with rules, health rules and is collecting data then you'll want to see the results of that information in the form of health status and possibly graphs. When requesting the health of a namespace, all of the health rules are applied for that namespace and the output takes two forms. The first is a simple statistic of the number of good, ok and bad responses. The second is a breakdown of the log entries associated with those responses.

    status<"app_data"> ->
    "OK", [1, 1, 0]

    logs<"app_data"> ->
    [
      ["Not much time required to process all users."],
      ["Some users processed but not many."],
      []
    ]

When Heman is determining if application health is "GOOD", "OK" or "BAD", it uses the status group in which is highest with 0 rules below it. Hence "1,1,0" is "OK" whereby "5,0,1" is "BAD".

# TODO

Stats:

 * Add function to store to garbage collect old data.
 * When adding stats, support both 'incr' and 'set' operations.

Rules:

Health:

 * Add hashtable or binary search tree to represent health rules.
 * Add function to iterate over datasets for a given namespace.
 * Add lua support.
