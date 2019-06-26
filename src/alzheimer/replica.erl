-module (replica).
-export([start_link/0,init/1, repRead/2,repWrite/2, repUpdate/2]).
-export([handle_info/2,code_change/3, handle_call/3, 
        handle_cast/2, terminate/2]).
-behaviour(gen_server).

start_link() -> 
	gen_server:start_link(?MODULE, [], []).

repRead(Pid, {read,Req}) ->
  gen_server:call(Pid, {read,Req} ).

repWrite(Pid, {write, Req}) ->
  gen_server:call(Pid, {write, Req}).

% I tried doing the update but had problems with it
repUpdate(Pid, {update, Req}) ->
  gen_server:cast(Pid, {update, Req}).

init([]) -> {ok, []}.

handle_call({read,Req}, From, Requests) ->
  case Requests == [] of
        true -> {reply, 'ABORTED', exception, Requests};
        false ->
          case lists:member(Req,Requests) of
            true ->
              {reply,{ok,Req},Requests};
            false ->
              {reply,'ABORTED', exception, Requests}
          end
  end;

%% check if message is already in Requests => {noupdate, Reply}
%% else {updated, Reply, NewState}
%% or stop
handle_call({write, Req}, From, Requests) ->
  % check if request has been sent
  try lists:member(Req,Requests) of
    true -> {reply, {noupdate,Req}, Req};
    false -> 
      NewRequests = [Req|Requests],
     {reply,{updated,NewRequests}, NewRequests}
     % gen_replicated:updateOthers(From, Req)
  catch
    _ : _ -> stop
  end.

handle_info({'ABORTED', server_stopped}, Requests) ->
    {stop,server_stopped,Requests}.

%% Default catch-all
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Default catch-all
handle_cast(_Req, State) ->
    {noreply, State}.

%%% FAILED TRIALS
% handle_cast({update, Req}, Requests) ->
%   % check if request has been sent
%   try lists:member(Req,Requests) of
%     true -> {noreply, Requests};
%     false -> 
%       NewRequests = [Req|Requests],
%       {{noreply,NewRequests}, NewRequests}
%   catch
%     _ : _ -> stop
%   end.