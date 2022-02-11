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




% Join a channel

handle(St, {join, Channel, User}) ->

    ChannelInList = lists:member(Channel, St#inState.channels),   

    if ChannelInList ->
        % If channel exists, add client to channel

        Response = genserver:request(list_to_atom(Channel), {join, User}),
        case Response of
            sucess -> {reply, sucess, St};   
            failed -> {reply, failed, St}
        end;
        true ->
        % If channel does not exist, create one and add client to channel
            genserver:start(list_to_atom(Channel), [User], fun channelHandler/2),
            {reply, sucess, St#inState{channels = [Channel | St#inState.channels]}}     
    end;



handle(St, _) ->
    {reply, {error, not_implemented, "Not Working"}, St}.
 
% Join a channel
channelHandler(Users, {join, User}) ->
    UserInChannel = lists:member(User, Users),

    if UserInChannel ->
           io:fwrite("User in channel"),
           {reply, failed, Users};
        true ->
           io:fwrite("User not in channel"),
           %{reply, sucess, lists:append(User,Users)}
           {reply, sucess, [User | Users]}
    end;
    
% Leave a channel
channelHandler(Users, {leave, User}) ->
    io:fwrite("In Channel handler\n"),
    io:fwrite("~p~n", [Users]),

    UserInChannel = lists:member(User, Users),

    if UserInChannel ->
           io:fwrite("User in channel"),
           {reply, sucess, lists:delete(User, Users)};
           
        true ->
           io:fwrite("User not in channel"),
           {reply, failed, Users}
    end;
    
channelHandler(Users, {message_send, Channel, Nick ,Msg, User}) ->
    io:fwrite("In Channel handler for message\n"),
    io:fwrite("~p~n", [registered()]),
    io:fwrite("~p~n", [Users]),

    spawn(fun() -> lists:foreach(fun(To) ->
                     io:fwrite("~p~n", [To]),
                     
                     %To:handle({message_receive, self(), User, Msg})
                     
                    genserver:request(To, {message_receive, Channel, Nick, Msg})
                  
                 end, Users) end).
    %lists:map(fun(To)-> sendMessageToClient(Msg, User) end, Users);
    %genserver:request(list_to_atom(To),{message_receive,self(),User,Msg}).

%sendMessageToClient(Msg, User, To) ->
    

    %{reply, {error, not_implemented, "Working"}, Users}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).
    %not_implemented.
