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
-module(rexi_governor).

-behaviour(gen_server).

%% API
-export([start_link/0, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% max number of processes that can be spawned
-define(SPAWN_MAX, 2000).
%% timeout period after which a node is considered knocked out
-define(NODEOUT, 100).

-record(state, {node_pids = ets:new(pids, [bag]),
                node_pid_cnts = ets:new(pids, [set]),
                pid_nodes = ets:new(nodes, [set]),
                node_timers = ets:new(timers, [set])}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Dest, Msg) ->
    case erlang:send(Dest, Msg, [noconnect, nosuspend]) of
    ok -> ok;
    _ ->
        % treat nosuspend and noconnect the same
        gen_server:cast(?MODULE, {spawn_and_track, Dest, Msg})
    end.

init([]) ->
    net_kernel:monitor_nodes(true),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({spawn_and_track, {_, Node} = Dest, Msg},
            #state{node_pids = NodePids, node_pid_cnts = PidCnts,
                   pid_nodes = PidNodes}=State) ->
    case ets:lookup(PidCnts, Node) of
    [] ->
        % first time
        ets:insert(PidCnts, {Node, 0}),
        Spawned = 0;
    [{Node, Cnt}] ->
        Spawned = Cnt
    end,
    case Spawned < ?SPAWN_MAX of
    true ->
        {Pid, _Ref} = spawn_monitor(erlang, send, [Dest, Msg]),
        ets:insert(NodePids, {Node, Pid}),
        ets:update_counter(PidCnts, Node, 1),
        ets:insert(PidNodes, {Pid, Node});
    false ->
        % drop message on floor
        ok
    end,
    {noreply, State}.

handle_info({nodeup, Node}, #state{node_timers = Timers} = State) ->
    case ets:lookup(Timers, Node) of
    [{Node, TRef}] ->
        erlang:cancel_timer(TRef),
        {noreply, State#state{node_timers = ets:delete(Timers, Node)}};
    _ ->
        {noreply, State}
    end;

handle_info({nodedown, Node}, #state{node_timers = Timers } = State) ->
    case ets:lookup(Timers, Node) of
    [] ->
        TRef = erlang:send_after(?NODEOUT, self(), {nodeout, Node}),
        ets:insert(Timers, {Node, TRef}),
        {noreply, State};
    _ ->
        {noreply, State}
    end;

handle_info({nodeout, Node}, #state{node_pids = NodePids, node_pid_cnts = PidCnts,
                                    pid_nodes = PidNodes,
                                    node_timers = Timers} = State) ->
    % check for race with node up
    case ets:member(Timers, Node) of
    true ->
        ets:delete(Timers, Node),
        Pids = [Pid || {_, Pid} <- ets:lookup(NodePids, Node)],
        ets:update_counter(PidCnts, Node, -(length(Pids))),
        lists:map(fun(P) -> ets:delete(PidNodes, P) end, Pids),
        lists:map(fun(P) -> exit(P, kill) end, Pids),
        ets:delete(NodePids, Node);
    false ->
        ok
    end,
    {noreply, State};

handle_info({'DOWN', _, process, Pid, normal},
            #state{node_pids = NodePids,
                   node_pid_cnts = PidCnts,
                   pid_nodes = PidNodes} = State) ->
    case ets:lookup(PidNodes, Pid) of
    [{Pid, Node}] ->
        ets:delete_object(NodePids, {Node, Pid}),
        ets:update_counter(PidCnts, Node, -1),
        ets:delete(PidNodes, Pid);
    [] ->
        ok
    end,
    {noreply, State};

handle_info({'DOWN', _, process, _Pid, killed}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
