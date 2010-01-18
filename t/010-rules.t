#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -boot start_sasl -sasl errlog_type error

main(_) ->
    etap:plan(8),
    error_logger:tty(false),
    etap_application:start_ok(sasl, "application 'sasl' started ok"),
    etap_application:start_ok(mnesia, "application 'mnesia' started ok"),
    etap_application:start_ok(crypto, "application 'crypto' started ok"),
    etap_application:start_ok(heman, "application 'heman' started ok"),

    (fun() ->
        etap:is(heman:rule_set({<<"movies">>, <<"action">>}, increase, "Action Movies"), ok, "rule 1 created"),
        etap:is(heman:rule_set({<<"movies">>, <<"comedy">>}, increase, "Comedy Movies"), ok, "rule 2 created"),
        Rules = [
        	{rule,{<<"heman_meta">>,<<"stat_set">>}, increase, "Number of stat calls made."},
            {rule,{<<"movies">>,<<"action">>}, increase, "Action Movies"},
            {rule,{<<"movies">>,<<"comedy">>}, increase, "Comedy Movies"}
        ],
        etap:is(lists:sort(heman:rule_get()), lists:sort(Rules), "rules exist"),
        ok
    end)(),

    (fun() ->
        etap:is(heman:namespaces(), [<<"heman_meta">>, <<"movies">>], "namespaces returns movies"),
        ok
    end)(),

    etap:end_tests().
