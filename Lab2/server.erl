-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    
    %genserver:start(ServerAtom, [] ,fun handle/2).

    Test = spawn(fun() -> loop(ServerAtom) end),
    self().
    %not_implemented.



loop(ServerAtom) ->
    receive {request, From, Ref, Data} -> From ! {result, Ref, "HEJ"}.


     

% Join
%handle(St, {join, Channel}) -> 

 
% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
