#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -boot start_sasl -sasl errlog_type error -config heman

main(_) ->
    etap:plan(7),
    error_logger:tty(false),
    etap_application:start_ok(sasl, "application 'sasl' started ok"),
    etap_application:start_ok(mnesia, "application 'mnesia' started ok"),
    etap_application:start_ok(crypto, "application 'crypto' started ok"),
    etap_application:start_ok(heman, "application 'heman' started ok"),

    (fun() ->
        etap:is(heman:add_rule({<<"movies">>, <<"action">>}, increase), ok, "rule 1 created"),
        etap:is(heman:add_rule({<<"movies">>, <<"comedy">>}, increase), ok, "rule 2 created"),
        Rules = [
            {rule,{<<"movies">>,<<"action">>},increase},
            {rule,{<<"movies">>,<<"comedy">>},increase}
        ],
        etap:is(lists:sort(heman:rules()), lists:sort(Rules), "rules exist"),
        ok
    end)(),

    etap:end_tests().
