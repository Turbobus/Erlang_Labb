-module(server).
-export([start/1,stop/1]).

-record(inState,{
    channels,
    nicks
}).

% The initial state of the server
initial_state() ->
    #inState{
        channels = [], % Pid to underlying channel procesors 
        nicks = []     % All taken nicks for the whole server
    }.


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_state() ,fun handle/2).


% Join a channel
handle(St, {join, Channel, User, Nick}) ->

    % See if channel already exists
    ChannelInList = lists:member(Channel, St#inState.channels),   

    if ChannelInList ->
        % If channel exists, send request to channelserver to join user
        Response = genserver:request(list_to_atom(Channel), {join, User}),
        case Response of
            sucess -> {reply, sucess, St#inState{nicks = [Nick | St#inState.nicks]}};   
            failed -> {reply, failed, St}
        end;
        true ->
        % If channel does not exist, create one and add client to channel
            genserver:start(list_to_atom(Channel), [User], fun channelHandler/2),
            {reply, sucess, St#inState{channels = [Channel | St#inState.channels], nicks = [Nick | St#inState.nicks]}}     
    end;


% Change nickname
handle(St, {changeNick, NewNick, OldNick}) ->

    % See if the new nick is already taken
    AlreadyExist = lists:member(NewNick, St#inState.nicks),

    % If it existed it fails, otherwise it deletes the old nick from the list and adds the new nick instead
    case AlreadyExist of
        true  -> {reply, failed, St};
        false -> {reply, ok, St#inState{nicks = lists:delete(OldNick, [NewNick | St#inState.nicks])}}
    end;

% Stops all channel servers (If the masterserver quits)
handle(St, stopAllChannels) ->

    % Send a stop command to each channelserver
    lists:foreach(fun(Channel) ->
                    genserver:stop(list_to_atom(Channel))
                    end, St#inState.channels), 
    {reply, ok, []};


% Client quits the user interface
handle(St, {quitClient, User, UserNick})->
    
    % Remove user from all channels (Will make client leave channels that it were in)
    lists:foreach(fun(Channel) ->
                    genserver:request(list_to_atom(Channel), {leave, User})
                    end, St#inState.channels), 
    
    % Frees up the nick that the leaving user had so other users can take it
    NewNickList = [Nick || Nick <- St#inState.nicks, Nick /= UserNick],
    {reply, ok, St#inState{nicks = NewNickList}};
    
% Failsafe to catch all other commands       
handle(St, _) ->
    {reply, {error, not_implemented, "Not Working"}, St}.





% Channel server functions
%-----------------------------------------------------------------------------------------

% Join a channel
channelHandler(Users, {join, User}) ->

    % See if user is already in the channel
    UserInChannel = lists:member(User, Users),

    if UserInChannel ->   
            % If the user were in the channel alredy, they cannot be entered again
           {reply, failed, Users};
        true ->
           % If the user where not in the channel, add them to the channel
           {reply, sucess, [User | Users]}
    end;
    
% Leave a channel
channelHandler(Users, {leave, User}) ->
    % Check if user is a member of this channel
    UserInChannel = lists:member(User, Users),
    
    % if they are a member, let them leave. Otherwise we reply with failed
    if UserInChannel ->
           {reply, sucess, lists:delete(User, Users)};
           
        true ->
           {reply, failed, Users}
    end;
    
% Send a message
channelHandler(Users, {message_send, Channel, Nick ,Msg, User}) ->
    % Check if the user is a member of this channel
    case lists:member(User,Users) of
        % If the user is a member, send a request to all other users in the channel to receive the message
        true -> lists:foreach(fun(To) -> % Maybe remove spawn? Or move it into the foreach
                                spawn(fun() -> 
                                if To /= User ->
                                    genserver:request(To, {message_receive, Channel, Nick, Msg});
                                    true -> ok
                                end
                                end)
                            end, Users),
                            {reply, ok, Users};

        % If not a member. reply with failed
        false -> {reply, failed, Users}
    end;


% Handle any requests that can not be pattern matched 
channelHandler(Users, _) ->
    {reply, {error, not_implemented, "Not Working"}, Users}.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, stopAllChannels),
    genserver:stop(ServerAtom).
