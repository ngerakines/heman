#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -boot start_sasl -config heman

main(_) ->
	etap:plan(1),
    %% error_logger:tty(false),
    etap_application:start_ok(heman, "application 'heman' started ok"),

   	etap:end_tests().

