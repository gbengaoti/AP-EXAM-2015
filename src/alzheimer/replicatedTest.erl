-module(replicatedTest).
-include_lib("eunit/include/eunit.hrl").

%% start the gen_replicated server and the replicas
%% should return {'ABORTED', exception,[]} or {ok, ServerRef}

% zero replicas - error
start1_test() ->
	?assertEqual((gen_replicated:start(0, mod)),{'ABORTED',exception,[]}).

% correct-{ok, ServerRef}
start2_test() ->
	{ok, Cord} = gen_replicated:start(3, mod).

%%%%%%%%%%%%%%% READ AND WRITE REQUESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%5%%%%%%%%%%

write1_test() ->
	{ok, Cord} = gen_replicated:start(3, mod),
	?assertEqual((gen_replicated:write(Cord, "Pie")),{updated,["Pie"]}).

% Reqest already in Request box
write2_test() ->
	{ok, Cord} = gen_replicated:start(3, mod),
	gen_replicated:write(Cord, "Pie"),
	?assertEqual((gen_replicated:write(Cord, "Pie")),{noupdate,"Pie"}).

% reads Request
readWrite1_test() ->
	{ok, Cord} = gen_replicated:start(3, mod),
	gen_replicated:write(Cord, "Pie"),
	gen_replicated:write(Cord, "Pies"),
	gen_replicated:write(Cord, "Pie3"),
	?assertEqual((gen_replicated:read(Cord, "Pie")),{ok,"Pie"}).

% Requests box empty - exception
readWrite2_test() ->
	{ok, Cord} = gen_replicated:start(3, mod),
	gen_replicated:read(Cord, "Pie").

% Request not in request box - exception
readWrite3_test() ->
	{ok, Cord} = gen_replicated:start(3, mod),
	gen_replicated:write(Cord, "Pie"),
	gen_replicated:write(Cord, "Pies"),
	gen_replicated:write(Cord, "Pie3"),
	gen_replicated:read(Cord, "Pls").

% Stop : stops all replicas and the coordiator
stop1_test() ->
	{ok, Cord} = gen_replicated:start(3, mod),
	gen_replicated:stop(Cord).