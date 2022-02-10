-module(server).
-export([start/1,stop/1]).

-record(inState,{
    channels
}).

initial_state() ->
    #inState{
        channels = [] % Pid to underlying channel procesors 
    }.


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    
    genserver:start(ServerAtom, initial_state() ,fun handle/2).
    %genserver:request(ServerID, {"HEJ", "DÅ"});


    %Test = spawn(fun() -> loop(ServerAtom) end),
    %self().
    %not_implemented.




% Join channel

handle(St, {join, Channel, User}) ->
    % TODO: Implement this function
    %io:fwrite("In join handle"),
    ChannelInList = lists:member(Channel, St#inState.channels),   
  
    io:fwrite("~p~n", [St#inState.channels]),
    io:fwrite("~p~n", [Channel]),
    io:fwrite("~p~n", [ChannelInList]),
    

    if ChannelInList ->
        % If channel exists, add client to channel
        %genserver:start(list_to_atom(Channel),[User], fun channelHandler/2),

        Response = genserver:request(list_to_atom(Channel), {join, User}),
        io:fwrite("In Wrong if\n"),
        io:fwrite("~p~n", [Response]),
        case Response of
            sucess -> {reply, sucess, St};   
            failed -> {reply, failed, St}
        end;
        true ->
            io:fwrite("In if\n"),
        % If channel does not exist, create one and add client to channel
            genserver:start(list_to_atom(Channel), [User], fun channelHandler/2),

            % Hur vi hade skrivit
            %{reply, sucess, St#inState{channels = lists:append(Channel, St#inState.channels)}},
            
            % Båda dessa verkar fungera
            %{reply, sucess, St#inState{channels = lists:append([Channel], St#inState.channels)}}, % append() == ++
            {reply, sucess, St#inState{channels = [Channel | St#inState.channels]}}               % använder "cons" => |
           
    end;
   

    
    %{reply, {error, not_implemented, "Test"}, St}.


handle(St, _) ->
    {reply, {error, not_implemented, "Not Working"}, St}.
 
channelHandler(Users, {join, User}) ->
    UserInChannel = lists:member(User, Users),
    io:fwrite("~p~n", [Users]),

    if UserInChannel ->
           io:fwrite("User in channel"),
           {reply, failed, Users};
        true ->
           io:fwrite("User not in channel"),
           %{reply, sucess, lists:append(User,Users)}
           {reply, sucess, [User | Users]}
    end.
    
    
    
    %{reply, {error, not_implemented, "Working"}, Users}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).
    %not_implemented.
