%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : Data structures 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(msg_lib_if).
 


%% --------------------------------------------------------------------
%% Data type: signal message
%% Purpose: Unified way to send and receive signal from processes
%% where
%%     from: erlang process pid (senders Pid) 
%%     tag_msg: atom or tuple  (signal identifier)
%%     msg: term 
%% --------------------------------------------------------------------
record(signal_message,{from,tag_msg,ms}).
