%% Copyright (c) 2009 Nick Gerakiens <nick@gerakines.net>
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
-module(heman_web).

-include("heman.hrl").

-export([start/0, dispatch/1]).

start() ->
    mochiweb_http:start([{loop, fun heman_web:dispatch/1}, {port, 7816}]).

dispatch(Req) ->
    UriParts = string:tokens(Req:get(path), "/"),
    handle([Req:get(method) | UriParts], Req).

handle(['GET'], Req) ->
    Namespaces = heman:namespaces(),
    Projects = [ {NS, heman:health(NS)} || NS <- Namespaces],
    Body = erlang:iolist_to_binary(heman_troot:main(Projects)),
    Req:respond({200, [{"Content-Type", "text/html"}], Body});

handle(['GET', Namespace], Req) ->
    Score = heman:health(Namespace),
    Log = heman:log_get(Namespace),
    Rules = heman:rule_get(),
    Keys = lists:usort([ Key || #rule{ key = {NS, Key}} <- Rules, NS == list_to_binary(Namespace) ]),
    Stats = [begin
        Dataset = lists:keysort(1, [begin
            {Stat#stat.fordate, Stat#stat.namespace, Stat#stat.key, Stat#stat.value}
        end || Stat <- heman:stat_get(Namespace, Key)]),
        case length(Dataset) > 10 of
            true -> {Key, lists:nthtail(10, Dataset)};
            _ -> {Key, Dataset}
        end
    end || Key <- Keys],
    Body = erlang:iolist_to_binary(heman_tnamespace:main({Namespace, Score}, Log, Keys, Stats)),
    Req:respond({200, [{"Content-Type", "text/html"}], Body});

handle(_, Req) ->
    Req:respond({404, [{"Content-Type", "text/html"}], <<"">>}).
