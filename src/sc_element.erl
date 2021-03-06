%%%-------------------------------------------------------------------
%%% @author sgrout
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2018 09:03
%%%-------------------------------------------------------------------
-module(sc_element).
-author("sgrout").

-behaviour(gen_server).

%% API
-export([start_link/2, create/2,create/1,fetch/1,replace/2,delete/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, lease_time, start_time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(Value, LeaseTime) ->
  gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
  sc_sup:start_child(Value, LeaseTime).

create(Value) ->
  create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
  gen_server:call(Pid, fetch).

replace(Pid, Value) ->
  gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
  gen_server:cast(Pid, delete).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([Value, LeaseTime]) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  {ok, #state{value = Value,
              lease_time = LeaseTime,
              start_time = StartTime},
    time_left(StartTime, LeaseTime)}.

time_left(_StartTime, infinity) ->
  infinity;
time_left(StartTime, LeaseTime) ->
  Now = calendar:local_time(),
  CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
  TimeElapsed = CurrentTime - StartTime,
  case LeaseTime - TimeElapsed of
    Time when Time =< 0 -> 0;
    Time  -> Time * 1000
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call(fetch, _From, State) ->
  #state{value = Value,
        lease_time = LeaseTime,
        start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {reply, {ok, Value}, State, TimeLeft}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({replace, Value}, State) ->
  #state{lease_time = LeaseTime,
    start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
  {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
  {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  sc_store:delete(self()),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
