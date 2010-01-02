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

-export([start/0, dispatch/1, chart_url/1]).

-define(ENC, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-.").

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

handle(['GET', "favicon.ico"], Req) ->
    Req:respond({404, [{"Content-Type", "text/html"}], <<"">>});

handle(['GET', Namespace], Req) ->
    {Logs, Score} = heman:health_and_logs(Namespace),
    Rules = heman:rule_get(),
    Keys = lists:usort([ Key || #rule{ key = {NS, Key}} <- Rules, NS == list_to_binary(Namespace) ]),
    Stats = [begin
        Dataset = [begin
            {Stat#stat.fordate, Stat#stat.namespace, Stat#stat.key, Stat#stat.value}
        end || Stat <- lists:reverse(lists:keysort(2, heman:stat_get(Namespace, Key)))],
        {Key, Dataset}
    end || Key <- Keys],
    Body = erlang:iolist_to_binary(heman_tnamespace:main({Namespace, Score}, Logs, Keys, Stats)),
    Req:respond({200, [{"Content-Type", "text/html"}], Body});

handle(_, Req) ->
    Req:respond({404, [{"Content-Type", "text/html"}], <<"">>}).

chart_url(Stats) ->
    Values = case length(Stats) of
        N when N > 100 -> {X, _} = lists:split(100, Stats), lists:reverse([Value || {_ , _, _, Value} <- X]);
        _ -> lists:reverse([Value || {_ , _, _, Value} <- Stats])
    end,
    Max = case Values of [] -> 0; _ -> lists:max(Values) end,
    "http://chart.apis.google.com/chart?chs=200x125&cht=ls&chco=0077CC&chd=t:" ++ string:join([ integer_to_list(V) || V <- Values], ",") ++ "&chds=0," ++ integer_to_list(Max).
    %% {Max, Encoded} = encode_values(Values, 0, ""),
    %% "http://chart.apis.google.com/chart?chs=200x125&cht=ls&chco=0077CC&chd=e:" ++ Encoded ++ "&chds=0," ++ integer_to_list(Max + 1).

encode_values([], Max, Acc) -> {Max, Acc};
encode_values([Value | Values], Max, Acc) ->
    NewMax = case Max > Value of true -> Max; _ -> Value end,
    F = floor(Value / 64),
    S = Value rem 64,
    encode_values(Values, NewMax, Acc ++ [lists:nth(F + 1, ?ENC), lists:nth(S + 1, ?ENC)]).

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

