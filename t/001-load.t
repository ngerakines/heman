#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin -boot start_sasl -sasl errlog_type error

main(_) ->
    etap:plan(5),
    error_logger:tty(false),
    etap_application:start_ok(sasl, "application 'sasl' started"),
    etap_application:start_ok(mnesia, "application 'mnesia' started"),
    etap_application:start_ok(crypto, "application 'crypto' started"),
    etap_application:start_ok(heman, "application 'heman' started"),
    etap:is(application:stop(heman), ok, "application 'heman' stopped"),
    etap:end_tests().
