-module(alzheimerTest).
-include_lib("eunit/include/eunit.hrl").

start1_test() ->
	{ok, Cord} = alzheimer:start().

% query test
%% If database is empty - exception
query1_test() ->
	{ok, Cord} = alzheimer:start(),
	alzheimer:query(Cord,p).

% upsert test