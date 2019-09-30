%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_controller_service).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
-define(W1,'worker_1@asus').
-define(W2,'worker_2@asus').
%% External exports

-export([]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function:init 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
init_test()->
    rpc:call(list_to_atom("service_x@asus"),init,stop,[]),
    rpc:call(?W1,os,cmd,["rm -rf "++"service_x"]),
    pong=net_adm:ping(?W1),
    pong=net_adm:ping(?W2),
    {ok,_Pid}=controller_service:start(),
    ok.

t1_get_nodes()->
    [worker_2@asus,worker_1@asus]=controller_service:get_nodes(),
ok.    

t2_create_pod_test()->
    {ok,'service_x@asus'}=controller_service:create_pod(?W1,"service_x"),
    pong=net_adm:ping('service_x@asus'),
    {error,[pod_already_loaded,"service_x",controller,_]}=controller_service:create_pod(?W1,"service_x"),
    {error,[badrpc,nodedown,controller,_]}=controller_service:create_pod('glurk@asus',"service_x"),
    ok.

t3_get_pods_test()->
    [service_x@asus]=controller_service:get_pods(),
    ok.

t4_delete_pod_test()->
    {ok,stopped}=controller_service:delete_pod(?W1,"service_x"),
    {ok,stopped}=controller_service:delete_pod(?W1,"service_x"),
    pang=net_adm:ping('service_x@asus'),
    {error,[badrpc,nodedown,controller,_LINE]}=controller_service:delete_pod('glurk@asus',"service_x"),
    ok.
t5_get_pods_test()->
    []=controller_service:get_pods(),
    ok.

t6_create_container_test()->
    {ok,'service_x@asus'}=controller_service:create_pod(?W1,"service_x"),
    pong=net_adm:ping('service_x@asus'),
    PodId="service_x",
    CpCmd="cp -r "++"brd_ctrl "++PodId,
    rpc:call('service_x@asus',os,cmd,[CpCmd],5000),
    true=rpc:call('service_x@asus',filelib,is_dir,[filename:join(PodId,"brd_ctrl")],5000),
    ok.

t7_delete_container_test()->
    
    {ok,stopped}=controller_service:delete_pod(?W1,"service_x"),
    ok.


stop_test()->
    controller_service:stop(),
    do_kill().
do_kill()->
    init:stop().
