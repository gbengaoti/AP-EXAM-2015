-module(alzheimer).
-export([start/0, upsert/3, query/2,f/1,p/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-behaviour(gen_server).
%%%=========================================================================
%%%  API
%%%=========================================================================

start() ->
    gen_server:start_link(?MODULE, [], []).

%% carry out Pred on every data in the database
query(Aid, Pred) ->
    gen_server:call(Aid, {doP,Pred}).

upsert(Aid, Id, F) ->
    gen_server:call(Aid, {update,Id}).


init([]) -> {ok,[]}.

%% for each id and corresponding data in the database which 
%% corresponds to a row, call the function P on it
%% DB is the database being maintained here
handle_call({doP,Pred}, From, DB ) ->
	case DB == [] of
		false ->lists:foreach(fun({Id, Data}) -> ({reply, p(Pred,{Id,Data}), DB}) end, DB);
		true -> {reply,'ABORTED', exception, DB}
	end;

%% use F to determine whether DB shoud be modified
handle_call({update,Id}, From, DB) ->
% If there is no row with identifier Id in the database, then F is called with the argument
% {new, Id}; otherwise F is called with the argument {existing, {Id, Data}},
% where Data is the row data for Id
	% searches in DB for element with the ID, returns false or the Tuple
	case lists:keyfind(Id, 1, DB) of
		false -> {reply, f({new, Id}), DB};
		{_,Data} ->{reply, f({existing, {Id, Data}}), DB}
	end.

%% should run pred on each element in Data,
%% save true values and for an insatnce f false raise exception
p(Pred, {Id,Data}) ->
	%% neeed to return row where exception occured
	{True, False} = lists:partition(fun(D) -> Pred(D) ==  true end, Data),
		case False == [] of
			false -> {ok,True};
			true -> {error,False}
		end.

% If F returns {modify, NewData}, then the row for
% Id is set to NewData; otherwise if F returns ignore then the database
% is not updated.Returns the return value of the F call, and if F raises a 
% normal (throw) exception,
% then this function should raise the same exception.
f(Res) ->
	try Res of
		{new,_} -> {modify, newData};
		{existing,_} -> {ignore}
	catch
		_ : _ -> {error, exception}
	end.

%% Default catch-all
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Default catch-all
handle_info(_Reason, State) ->
    {noreply, State}.

%% Default catch-all
terminate(_Reason, _State) ->
    ok.

%% Default catch-all
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
	