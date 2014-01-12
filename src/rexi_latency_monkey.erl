% Copyright 2012 Cloudant
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
-module(rexi_latency_monkey).

-export([maybe_go_bananas/3]).

maybe_go_bananas(Msg, Dest, SendFun) ->
    EndPoints = split(config:get("latency_monkey", "endpoints", "all")),
    case delay_request_by(Msg, Dest, EndPoints) of
        false ->
            SendFun();
        Wait ->
            timer:apply_after(Wait, erlang, apply, [SendFun, []])
    end.

delay_request_by({'$gen_cast', {doit, _, _, MFA}}, Dest, EndPoints) ->
    {Module, Function, _} = MFA,
    EndPoint = case lists:member("all", EndPoints) of
        true ->
            "all";
        false ->
            EndPointAsString = string:join(
                [atom_to_list(Module), atom_to_list(Function)],
                ":"
            ),
            case lists:member(EndPointAsString, EndPoints) of
                true -> EndPointAsString;
                false -> false
            end
    end,
    case EndPoint of
        false -> false;
        EndPoint -> maybe_get_waittime(EndPoint, Dest)
    end;
delay_request_by(_, _, _) ->
    false.

maybe_get_waittime(EndPoint, {rexi_server, Dest}) ->
    Section = string:join(["latency_monkey", EndPoint], "."),
    Sources = split(config:get(Section, "source", "")),
    Node = node(),
    SourceIsVictim = case lists:member("all", Sources) of
        true ->
            true;
        false ->
            lists:member(atom_to_list(Node), Sources)
    end,
    case SourceIsVictim of
        true ->
            Targets = split(config:get(Section, "target")),
            TargetIsVictim = case lists:member("all", Targets) of
                true ->
                    true;
                false ->
                    lists:member(atom_to_list(Dest), Targets)
            end,
            case TargetIsVictim of
                true ->
                    {Waittime, _} = string:to_integer(
                        config:get(Section, "delay")
                    ),
                    Waittime;
                false ->
                    false
            end;
        _ ->
            false
    end;
maybe_get_waittime(_, _) ->
    false.

% Stolen from smoosh_utils.erl
split(CSV) ->
    re:split(CSV, "\\s*,\\s*", [{return,list}, trim]).