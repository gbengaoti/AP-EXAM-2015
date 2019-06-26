-module(gen_replicated).
-export([start/2, stop/1, read/2, write/2]).
-export([updateOthers/2]).
%%%=========================================================================
%%%  API
%%%=========================================================================

start(NumReplica, _Mod) ->
    % start replicated servers here
    Reps = (startReplicas(NumReplica,[])),
    ServerRef = spawn_link(fun() -> loop(NumReplica,[],Reps)end),
	case  Reps == [] of
		true -> {'ABORTED', exception,[]};
		false -> {ok, ServerRef}
    end.

%for stopping the coordinator and all the replica.
stop(Server) ->
   	rpc(Server, exit).

%% for sending a read request to a replicated server. The
%% coordinator will forward the request to one of the replica.
%% The return value is {ok, Result} where 
%% Result is the result from calling the Mod:handle_read function

read(Server, Req) ->
    rpc(Server, {toread, Req}).

write(Server, Req) ->
    rpc(Server, {towrite, Req}).

%% loop 
loop(NumReplica, MsgList, Reps) ->
	receive
		{From, exit} ->
			%% this line is meant to make all replicas exit
			lists:foreach(fun(Rep) ->(Rep ! {'ABORTED',server_stopped}) end, Reps),
			async(From,{'ABORTED',server_stopped});

		{From, {toread, Req}} ->
			async(From, (replica:repRead(lists:last(Reps), {read, Req}))),
      loop(NumReplica, MsgList, Reps);

    {From, {towrite, Req}} ->
      async(From, (replica:repWrite(lists:last(Reps), {write, Req}))),
      loop(NumReplica, MsgList, Reps);
    %% TRIED updating here
    {_From, {toupdate, Req}} ->
      lists:foreach(fun(Rep) ->(replica:repUpdate(Rep, {update, Req})) end, Reps)
	end.

%% message sending
%% For synchronous requests send a message and receives a response
rpc(Pid, Request) ->
  % sends a message(Request) to the process Id
  Pid ! {self(), Request},
  receive
    % waits until Pid responds
    {Pid, Response} -> Response
  end.

%% Send a message, don't wait for a response
async(Pid,Request) ->
  Pid ! {self(), Request}.

%% private functions

startReplicas(0, List) -> List;
startReplicas(NumReplica, List) ->
{ok, Z} = replica:start_link(),
startReplicas(NumReplica-1, [Z|List]).

%%% FAILED TRIALS
%% tried updating
updateOthers(Server, Req) ->
    async(Server, {toupdate, Req}).