-module(heman_web).

-export([start/0, dispatch/1]).

start() ->
    mochiweb_http:start([{loop, fun statmaster_web:dispatch/1}, {port, 7816}]).

dispatch(Req) ->
    Req:respond({200, [{"Content-Type", "text/html"}], <<"Got it!">>}).

