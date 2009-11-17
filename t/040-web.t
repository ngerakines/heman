#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../heman ./ebin -boot start_sasl -sasl errlog_type error

-include_lib("heman/include/heman.hrl").

main(_) ->
    etap:plan(8),
    error_logger:tty(false),
    
    application:load(heman),
    Rules = [
		{{<<"harvest">>, <<"grain_harvested">>}, increase, <<"Tons of grain harvested.">>},
		{{<<"harvest">>, <<"rice_harvested">>}, increase, <<"Tons of grain harvested.">>},
		{{<<"harvest">>, <<"breakmin">>}, increase, <<"Amount of time in minutes workers have taken breaks.">>},
		{{<<"harvest">>, <<"grain_price">>}, replace, <<"Current grain sell price.">>},
		{{<<"harvest">>, <<"rice_price">>}, replace, <<"Current rice sell price.">>}
	],
    application:set_env(heman, rules, Rules),
    
    etap_application:start_ok(sasl, "application 'sasl' started ok"),
    etap_application:start_ok(mnesia, "application 'mnesia' started ok"),
    etap_application:start_ok(crypto, "application 'crypto' started ok"),
    etap_application:start_ok(heman, "application 'heman' started ok"),

    (fun() ->
        HarvestRules = [
            {{hours, 24, sum}, {under, 30}, {decrease, 10}},
            {{hours, 24, sum}, {under, 10}, {decrease, 50}},
            {{hours, 24, sum}, {over, 80}, {increase, 30}},
            {{hours, 24, sum}, {over, 50}, {increase, 10}}
        ],
        BreakRules = [
            {{hours, 1, sum}, {under, 15}, {decrease, 20}},
            {{hours, 1, sum}, {over, 30}, {decrease, 20}}
        ],
        etap:is(heman:health_set(<<"harvest">>, 1, <<"grain_harvested">>, HarvestRules), ok, "health rule 1 created"),
        etap:is(heman:health_set(<<"harvest">>, 2, <<"rice_harvested">>, HarvestRules), ok, "health rule 2 created"),
        etap:is(heman:health_set(<<"harvest">>, 3, <<"breakmin">>, BreakRules), ok, "health rule 3 created"),
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
        Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
        Data = [
            {calendar:gregorian_seconds_to_datetime(Now - 60), 5},
            {calendar:gregorian_seconds_to_datetime(Now - 120), 6},
            {calendar:gregorian_seconds_to_datetime(Now - 180), 9}
        ],
        mnesia:transaction(fun() -> [begin
            DBKey = {Date, Time, <<"harvest">>, <<"breakmin">>},
            mnesia:write(#stat{
                pkey = DBKey,
                fordate = {Date, Time},
                namespace = <<"harvest">>,
                key = <<"breakmin">>,
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
