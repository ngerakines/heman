%% Copyright (c) 2009,2010 Nick Gerakiens <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(heman_health).
-include("heman.hrl").
-export([start/0, init/1, ploop/1]).

-record(health_state, {frequency, last_purge, keep_window}).

start() ->
    proc_lib:start(heman_health, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    heman_health:ploop(#health_state{
        frequency = heman:env_key('log_frequency', 180),
        last_purge = 0,
        keep_window = heman:env_key('purge_window', 172800)
    }).

ploop(State) ->
    timer:sleep(State#health_state.frequency * 1000),
    Namespaces = heman:namespaces(),
    Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
    NewState = if
        State#health_state.last_purge > (Now - (State#health_state.frequency * 3)) ->
            scrub_logs(State, Namespaces),
            State#health_state{ last_purge = Now };
        true -> State
    end,
    heman:stat_set_internal(<<"heman_meta">>, <<"health_loop">>, 1),
    process_namespace(Namespaces),
    ploop(NewState).

process_namespace([]) -> ok;
process_namespace([Namespace | Namespaces]) ->
    {Logs, Score} = heman:health_and_logs(Namespace),
    case heman:log_get(Namespace) of
        [#log{ messages = Logs } | _] -> ok;
        _ ->
            heman:log_set(Namespace, Score, Logs),
            heman:stat_set_internal(<<"heman_meta">>, <<"scores">>, Score),
            ok
    end,
    process_namespace(Namespaces).

scrub_logs(_, []) -> ok;
scrub_logs(State, [Namespace | Namespaces]) ->
    case heman:log_get(Namespace) of
        [] -> ok;
        [_] -> ok;
        Logs ->
            Now = calendar:datetime_to_gregorian_seconds({date(), time()}),
            LogsToDelete = lists:filter(fun(Log) ->
                Log#log.fordate > (Now - State#health_state.keep_window)
            end, Logs),
            [ mnesia:delete(Log) || Log <- LogsToDelete]
    end,
    scrub_logs(State, Namespaces).
