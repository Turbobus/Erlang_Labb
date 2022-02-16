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


% Join a channel
handle(St, {join, Channel, User}) ->
    io:fwrite("~p~n", [registered()]),
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

handle(St, stopAllChannels) ->
    lists:foreach(fun(Channel) ->
                    genserver:stop(list_to_atom(Channel))
                    end, St#inState.channels), 
    {reply, ok, []};

handle(St, {changeNick, NewNick}) ->
    NickAvailable = lists:all(fun(Channel) ->
                        genserver:request(list_to_atom(Channel), {checkUserNick, NewNick})
                    end, St#inState.channels), 
    
    %io:fwrite("~p~n", [NickAvailable]),
    case NickAvailable of
        true  -> {reply, ok, St};
        false -> {reply, failed, St}
    end;
    

handle(St, _) ->
    {reply, {error, not_implemented, "Not Working"}, St}.
 

channelHandler(Users, {checkUserNick, NewNick}) ->
    %List = lists:map(fun(OneUser) ->
    %                genserver:request(OneUser, {whoami})
    %                end, Users), 

    %NickInChannel = lists:member(NewNick, List),
     io:fwrite("~p~n", ["In Channel handler for changing name\n"]),
    case true of
        true  -> {reply, false, Users};
        false -> {reply, true, Users}
    end;



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

    UserInChannel = lists:member(User, Users),

    if UserInChannel ->
           io:fwrite("User in channel"),
           {reply, sucess, lists:delete(User, Users)};
           
        true ->
           io:fwrite("User not in channel"),
           {reply, failed, Users}
    end;
    
% Send a message
channelHandler(Users, {message_send, Channel, Nick ,Msg, User}) ->

    case lists:member(User,Users) of
        true -> spawn(fun() -> lists:foreach(fun(To) -> % Maybe remove spawn? Or move it into the foreach
                                if To /= User ->
                                    genserver:request(To, {message_receive, Channel, Nick, Msg});
                                    true -> ok
                                end
                            
                            end, Users) end),
                            {reply, ok, Users};

        false -> {reply, failed, Users}
    end;
    %lists:map(fun(To)-> sendMessageToClient(Msg, User) end, Users);
    %genserver:request(list_to_atom(To),{message_receive,self(),User,Msg}).

channelHandler(Users, _) ->
    {reply, {error, not_implemented, "Not Working"}, Users}.
%sendMessageToClient(Msg, User, To) ->
    


    %{reply, {error, not_implemented, "Working"}, Users}.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    %lists:foreach(fun(Channel) ->
     %               genserver:stop(list_to_atom(Channel))
      %              end, [ServerAtom#inState.channels]), 

    genserver:request(ServerAtom, stopAllChannels),
    genserver:stop(ServerAtom).
    %not_implemented.
