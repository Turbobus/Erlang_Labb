-module(server).
-export([start/1,stop/1]).




% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    
    ServerID = genserver:start(ServerAtom, [] ,fun handle/2),
    genserver:request(ServerID, {"HEJ", "DÅ"}),
    genserver:request(ServerID, {"PÅ", "DÅ"}),
    genserver:request(ServerID, {"DIG", "DÅ"})

    .

    %Test = spawn(fun() -> loop(ServerAtom) end),
    %self().
    %not_implemented.


     

% Join channel
%handle(St, {_, Channel}) ->
handle(St, {First, _}) ->
    % TODO: Implement this function
    {reply, First, St}. %;
    %{reply, {error, not_implemented, "Test"}, St}.

 
% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).
    %not_implemented.
