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
-export([start/0, init/1, ploop/0]).

start() ->
    proc_lib:start(heman_health, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    heman_health:ploop().

ploop() ->
    timer:sleep(180000),
    heman:stat_set_internal(<<"heman_meta">>, <<"health_loop">>, 1),
    process_namespace(heman:namespaces()),
    ploop().

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

