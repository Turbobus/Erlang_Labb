-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join a channel
handle(St, {join, Channel}) ->

    % Check if server is registered 
    %case lists:member(St#client_st.server, registered()) of

    case true of
        % If server is registered, send a request to join it
        true -> Result = (catch(genserver:request(St#client_st.server, {join, Channel, self(),St#client_st.nick}))),
                case Result of 
                    sucess -> {reply, ok, St};
                    failed -> {reply, {error, user_already_joined, "User already in channel"}, St};
                    {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
                end;
        % If server is not registered, reply with an error
        false -> {reply, {error, server_not_reached, "Server does not respond"}, St}
    end;
    


% Leave a channel
handle(St, {leave, Channel}) ->     
    % Check if channel is registered/exists
    ChannelExists = lists:member(list_to_atom(Channel), registered()),

    % If channel is registered, send a request to leave it
    if ChannelExists ->
        Result = genserver:request(list_to_atom(Channel), {leave, self()}),

        case Result of 
            % Reply with ok if user can leave
            sucess -> {reply, ok, St};
            % Reply with error if user is not a member of the channel
            failed -> {reply, {error, user_not_joined, "User not in channel"}, St}
        end;
        true ->
            {reply, {error, user_not_joined, "User not in channel"}, St}
    end;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->

    % Check if channel is registered/exists
    case lists:member(list_to_atom(Channel), registered()) of
    
        % If channel is registered, send a request to it with a message
        true -> Result = genserver:request(list_to_atom(Channel), {message_send, Channel, St#client_st.nick, Msg, self()}),
                case Result of
                    ok -> {reply, ok, St};
                    failed -> {reply, {error, user_not_joined, "User not in Channel"}, St};
                    {'EXIT',_} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
                end;

         % If channel is not registered, reply with error
        false -> {reply, {error, server_not_reached, "Server does not respond"}, St}
    end;



% Change nick
handle(St, {nick, NewNick}) ->
    % Send a request to the server to see if client can change to the new nick
    Result = (catch(genserver:request(St#client_st.server, {changeNick, NewNick, St#client_st.nick}))),

    % If ok, update the nick. Otherise, reply with error
    case Result of
        ok     -> {reply, ok, St#client_st{nick = NewNick}}; 
        failed -> {reply,{error, nick_taken, "Nick is already taken"},St}
    end;
    

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Send a request to the server to remove the client from all processes
    genserver:request(St#client_st.server, {quitClient, self(), St#client_st.nick}),
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
