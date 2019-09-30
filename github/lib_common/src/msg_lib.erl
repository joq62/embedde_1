%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(msg_lib).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("/home/pi/erlang/erlang_embedded_system_1/github/lib_msg/src/msg_lib.if").
%% --------------------------------------------------------------------



%% External exports
%-compile(export_all).

%% intermodule exports
-export([call/5,call/6
	]).

%% user interface help , info , start stop 
-export([
	 ]).

%% exports for use within the module only
-export([
	]).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: call
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
call(PidDestination,From,TagMsg,Msg,TagReply)->
    Signal=#signal_message{from=From,tag_msg=TagMsg,msg=Msg},
    PidDestination!{self(),Signal},
    Result=receive
	       {PidDestination,#signal_message{tag_msg=TagReply}=Reply}->
		   Reply;
	       UnmachedSignal->
		   {error,[unmatched_signal,UnmachedSignal,?MODULE,?LINE]}
	   end,
    Result.

call(PidDestination,From,TagMsg,Msg,TagReply,Timeout)->
    Signal=#signal_message{from=From,tag_msg=TagMsg,msg=Msg},
    PidDestination!{self(),Signal},
    Result=receive
	       {PidDestination,#signal_message{tag_msg=TagReply}=Reply}->
		   Reply;
	       UnmachedSignal->
		   {error,[unmatched_signal,UnmachedSignal,?MODULE,?LINE]}
	   after Timeout->
		   {error,[timeout,PidDestination,TagMsg,Msg,TagReply,?MODULE,?LINE]}
	   end,
    Result.
    
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
