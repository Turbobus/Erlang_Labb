-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    joinedChannels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        joinedChannels = []
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % TODO: Implement this function
    %io:fwrite(St#client_st.server),
    %io:fwrite(Channel),
    %io:fwrite(self()),
    
    % Check if server is registered 
    case lists:member(St#client_st.server, registered()) of
        % If server is registered, send a request to join it
        true -> Result = (catch(genserver:request(St#client_st.server, {join, Channel, self()}))),
                case Result of 
                    sucess -> {reply, ok, St#client_st{joinedChannels = [Channel | St#client_st.joinedChannels]}};
                    failed -> {reply, {error, user_already_joined, "User already in channel"}, St}
                end;
        % If server is not registered, reply an error
        false -> {reply, {error, server_not_reached, "Server does not respond"}, St}
    end;
    


% Leave channel
handle(St, {leave, Channel}) ->             % Look over and see if we can remove joinedChannel dependancy
    % TODO: Implement this function
    % {reply, ok, St} ;
    ChannelExists = lists:member(list_to_atom(Channel), registered()),
    %io:fwrite("~p~n", [registered()]),
    %io:fwrite("~p~n", [ChannelExists]),
    %io:fwrite("~p~n", [list_to_atom(Channel)]),

    %ChannelExists = lists:member(Channel, St#client_st.joinedChannels),
    if ChannelExists ->
        Result = (catch(genserver:request(list_to_atom(Channel), {leave, self()}))),

        case Result of 
            sucess -> {reply, ok, St#client_st{joinedChannels = lists:delete(Channel, St#client_st.joinedChannels)}};
            failed -> {reply, {error, user_not_joined, "User not in channel"}, St}
        end;
        true ->
            {reply, {error, user_not_joined, "User not in channel"}, St}
    end;


    %{reply, {error, not_implemented, "leave not implemented"}, St} ;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
   
    Result = genserver:request(list_to_atom(Channel), {message_send, Channel, St#client_st.nick, Msg, self()}),

    case Result of
        ok -> {reply,ok,St};
        failed -> {reply, {error, user_not_joined, "User not in Channel"}, St};
        exit -> {reply, {error, server_not_reached, "Server does not respond"}, St}
        
    end;


% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    io:fwrite("~p~n", [St]),
    io:fwrite("~p~n", ["In message handler"]),
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
