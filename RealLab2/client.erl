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
    
    % Send a request to server to join a given channel
    Result = (catch(genserver:request(St#client_st.server, {join, Channel, self(),St#client_st.nick}))),
    case Result of 
        % Reply with ok if user could join
        sucess -> {reply, ok, St};
        % Reply with error if user is already a member in channel
        failed -> {reply, {error, user_already_joined, "User already in channel"}, St};
        % If server did not repond, reply with error
        {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
    end;
        

% Leave a channel
handle(St, {leave, Channel}) ->     

    % Send a request to server to leave given channel
    Result = (catch(genserver:request(list_to_atom(Channel), {leave, self()}))),
    case Result of 
        % Reply with ok if user could leave
        sucess -> {reply, ok, St};
        % Reply with error if user is not a member of the channel
        failed -> {reply, {error, user_not_joined, "User not in channel"}, St};
        % If server did not repond, reply with error
        {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
    end;


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
   
    % Send request to send a message
     Result = (catch (genserver:request(list_to_atom(Channel), {message_send, Channel, St#client_st.nick, Msg, self()}))),
        case Result of
            % Reply with ok if the server could send the message 
            ok -> {reply, ok, St}; 
            % Reply with failed if it could not
            failed -> {reply, {error, user_not_joined, "User not in Channel"}, St}; 
            % Catch exit if server does not respond to client
            {'EXIT',_} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
        end;


% Change nick
handle(St, {nick, NewNick}) ->
    % Send a request to the server to see if client can change to the new nick
    Result = (catch(genserver:request(St#client_st.server, {changeNick, NewNick, St#client_st.nick}))),

    case Result of
        % Reply with ok if nick could be changed
        ok     -> {reply, ok, St#client_st{nick = NewNick}}; 
        % Reply with failed if nick is already taken
        failed -> {reply,{error, nick_taken, "Nick is already taken"},St};
        % If server did not repond, reply with error
        {'EXIT',_} -> {reply, {error, server_not_reached, "Server does not respond"}, St}
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
