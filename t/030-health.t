#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../heman ./ebin -boot start_sasl -config heman -sasl errlog_type error

-include_lib("heman/include/heman.hrl").

main(_) ->
    etap:plan(11),
    error_logger:tty(false),
    etap_application:start_ok(sasl, "application 'sasl' started ok"),
    etap_application:start_ok(mnesia, "application 'mnesia' started ok"),
    etap_application:start_ok(crypto, "application 'crypto' started ok"),
    etap_application:start_ok(heman, "application 'heman' started ok"),

    (fun() ->
        etap:is(heman:add_rule({<<"harvest">>, <<"grain_harvested">>}, increase), ok, "rule 1 created"),
        etap:is(heman:add_rule({<<"harvest">>, <<"rice_harvested">>}, increase), ok, "rule 2 created"),
        Rules = [
            {rule,{<<"harvest">>,<<"grain_harvested">>},increase},
            {rule,{<<"harvest">>,<<"rice_harvested">>},increase}
        ],
        etap:is(lists:sort(heman:rules()), lists:sort(Rules), "rules exist"),
        ok
    end)(),

    (fun() ->
        Rules1 = [
            {{hours, 24, sum}, {under, 30}, {decrease, 10}},
            {{hours, 24, sum}, {under, 10}, {decrease, 50}},
            {{hours, 24, sum}, {over, 80}, {increase, 30}},
            {{hours, 24, sum}, {over, 50}, {increase, 10}}
        ],
        etap:is(heman:add_health_rule(<<"harvest">>, 1, <<"grain_harvested">>, Rules1), ok, "health rule 1 created"),
        etap:is(heman:add_health_rule(<<"harvest">>, 2, <<"rice_harvested">>, Rules1), ok, "health rule 2 created"),        
        Rules = [
            {health,{<<"harvest">>,<<"grain_harvested">>}, <<"harvest">>,1,<<"grain_harvested">>,Rules1},
            {health,{<<"harvest">>,<<"rice_harvested">>}, <<"harvest">>,2,<<"rice_harvested">>,Rules1}
        ],
        etap:is(lists:sort(heman:health_rules(<<"harvest">>)), lists:sort(Rules), "rules exist"),
        ok
    end)(),

    (fun() ->
        Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
        Data = [
            {calendar:gregorian_seconds_to_datetime(Now), 29},
            {calendar:gregorian_seconds_to_datetime(Now - 60), 31},
            {calendar:gregorian_seconds_to_datetime(Now - 120), 30},
            {calendar:gregorian_seconds_to_datetime(Now - 180), 29},
            {calendar:gregorian_seconds_to_datetime(Now - 240), 28}
        ],
        mnesia:transaction(fun() -> [begin
            DBKey = {Date, Time, <<"harvest">>, <<"grain_harvested">>},
            mnesia:write(#stat{
                pkey = DBKey,
                fordate = {Date, Time},
                namespace = <<"harvest">>,
                key = <<"grain_harvested">>,
                value = Value
            })
        end || {{Date, Time}, Value} <- Data] end),
        ok
    end)(),

    (fun() ->
        Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
        Data = [
            {calendar:gregorian_seconds_to_datetime(Now), 5},
            {calendar:gregorian_seconds_to_datetime(Now - 60), 6},
            {calendar:gregorian_seconds_to_datetime(Now - 120), 9}
        ],
        mnesia:transaction(fun() -> [begin
            DBKey = {Date, Time, <<"harvest">>, <<"rice_harvested">>},
            mnesia:write(#stat{
                pkey = DBKey,
                fordate = {Date, Time},
                namespace = <<"harvest">>,
                key = <<"rice_harvested">>,
                value = Value
            })
        end || {{Date, Time}, Value} <- Data] end),
        ok
    end)(),

    (fun() ->
        etap:is(heman:health(<<"harvest">>), 70, "harvest health checks"),
        ok
    end)(),

    etap:end_tests().
