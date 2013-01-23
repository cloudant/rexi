%%%-------------------------------------------------------------------
%%% @author Bob Dionne <>
%%% @copyright (C) 2012, Cloudant, Inc.
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2012 by Bob Dionne <>
%%%-------------------------------------------------------------------
-module(rexi_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Returns list of tuples to set default properties
%%  for the suite.
%%
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%
%% @spec suite() -> Info
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%% @doc
%% Initialization before the whole suite
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = start_apps([rexi, twig]),
    case ct:get_config(controller) of
    true ->
        connect_nodes(),
        timer:apply_after(275, erlang, halt, []);
    _ ->
        ok
    end,
    Config.

start_apps([]) ->
    ok;
start_apps([App|Rest]) ->
    case application:start(App) of
    ok ->
       start_apps(Rest);
    {error, {already_started, App}} ->
       start_apps(Rest);
    {error, _Reason} when App =:= public_key ->
       % ignore on R12B5
       start_apps(Rest);
    {error, Reason} ->
        io:format("I cant start because of ~p ~n",[Reason]),
       {error, {app_would_not_start, App}}
    end.

connect_nodes() ->
    %% leave for possible use later, but no need for controlling node yet
    ct:pal("Connect nodes is a no-op here ~n",[]).

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the whole suite
%%
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case group.
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
init_per_testcase(rexi_simple, Config) ->
    Config;

init_per_testcase(rexi_stress, Config) ->
    Config.




%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase - atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% @end
%%--------------------------------------------------------------------
end_per_testcase(rexi_simple, Config) ->
    {save_config, Config};
end_per_testcase(rexi_stress, Config) ->
    {save_config, Config}.


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of test case group definitions.
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% @spec: groups() -> [Group]
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%  Returns the list of groups and test cases that
%%  are to be executed.
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
all() -> [rexi_stress].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Test case info function - returns list of tuples to set
%%  properties for the test case.
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Note: This function is only meant to be used to return a list of
%% values, not perform any other operations.
%%
%% @spec TestCase() -> Info
%% @end
%%--------------------------------------------------------------------
rexi_simple() ->
    [].

%%--------------------------------------------------------------------
%% @doc Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
%% @end
%%--------------------------------------------------------------------
rexi_simple(_Config) ->
    put(seed, random:seed()),
    Nodes = lists:filter(fun(N) ->
                             hd(string:tokens(atom_to_list(N), "@")) =/= "master"
                         end, nodes()),
    ct:pal("sending messages to nodes ~p ~n",[Nodes]),
    Total = send_times(Nodes, 10, 0),
    ct:pal("ok start was called, msgs sent 100 times and received ~p responses ~n",[Total]).

foo(_N, Seed) ->
    case random:uniform_s(10, Seed) of
    {5, NewSeed} ->
        ct:pal("Hit random 5 when seed was ~p ~n",[Seed]),
        timer:sleep(1000),
        rexi:reply({ok, NewSeed});
    {_, NewSeed} ->
        rexi:reply({ok, NewSeed})
    end.

send_times(_Nodes, 0, N) ->
    N;
send_times(Nodes, Times, Total) ->
    lists:map(fun(N) ->
                  Ref = rexi:cast(N, {rexi_SUITE, foo, [node(), get(seed)]}),
                  receive
                  {Ref, {ok, NewSeed}} ->
                      put(seed, NewSeed),
                      ok
                  after 1000 ->
                      ok
                  end
              end,
              Nodes),
    send_times(Nodes, Times - 1, Total + length(Nodes)).

rexi_stress(_Config) ->
    Nodes = lists:filter(fun(N) ->
                             hd(string:tokens(atom_to_list(N), "@")) =/= "master"
                         end, nodes()),
    ct:pal("sending messages to nodes ~p ~n",[Nodes]),
    Total = send_times1(Nodes, 1000000, 0),
    ct:pal("ok msgs sent and received ~p responses ~n",[Total]).

bar() ->
    timer:sleep(50),
    rexi:reply(ok).

send_times1(_Nodes, 0, N) ->
    N;
send_times1(Nodes, Times, Total) ->
    spawn(fun() ->
              lists:map(fun(N) ->
                            Ref = rexi:cast(N, {rexi_SUITE, bar, []}),
                            receive
                            {Ref, ok} ->
                                ok
                            after 500 ->
                                ok
                            end
                        end,
                        Nodes)
          end),
    send_times1(Nodes, Times - 1, Total + length(Nodes)).
