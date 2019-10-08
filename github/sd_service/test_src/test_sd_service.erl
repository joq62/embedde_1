%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : eunit for service discovery  
%%% Nodes -> used for manage Pods (erlang vm) 
%%% Pods -> used for manage containers (erlang vm)
%%% Container -> manage services (erlang applications)
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_sd_service). 
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------

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
    ok=application:start(sd_service),
    ok.

add_info_test()->
    {ok,Host}=inet:gethostname(),

    WorkerNodeId="node_worker_1",
    WorkerNodeStr=WorkerNodeId++"@"++Host,
    WorkerNode=list_to_atom(WorkerNodeStr),
    MasterNodeId="node_master_1",
    MasterNodeStr=MasterNodeId++"@"++Host,
    MasterNode=list_to_atom(MasterNodeStr),

    WorkerPodId="pod_worker_1",
    WorkerPodStr=WorkerPodId++"@"++Host,
    WorkerPod=list_to_atom(WorkerPodStr),

    WorkerPodId_2="pod_worker_2",
    WorkerPodStr_2=WorkerPodId_2++"@"++Host,
    WorkerPod_2=list_to_atom(WorkerPodStr_2),

    MasterPodId="pod_master_1",
    MasterPodStr=MasterPodId++"@"++Host,
    MasterPod=list_to_atom(MasterPodStr),
       
    ok=sd_service:add_worker_node(WorkerNodeId,WorkerNode),
    ok=sd_service:add_master_node(MasterNodeId,MasterNode),

    ok=sd_service:add_worker_pod(WorkerPodId,WorkerPod),
    ok=sd_service:add_master_pod(MasterPodId,MasterPod),

    ok=sd_service:register_service("ServiceId_1",WorkerPod),
    ok=sd_service:register_service("ServiceId_1",WorkerPod_2),
    ok=sd_service:register_service("Master_1",MasterPod),

    ok.

get_info_test()->    
    {ok,Host}=inet:gethostname(),

    WorkerNodeId="node_worker_1",
    WorkerNodeStr=WorkerNodeId++"@"++Host,
    WorkerNode=list_to_atom(WorkerNodeStr),
    MasterNodeId="node_master_1",
    MasterNodeStr=MasterNodeId++"@"++Host,
    MasterNode=list_to_atom(MasterNodeStr),

    WorkerPodId="pod_worker_1",
    WorkerPodStr=WorkerPodId++"@"++Host,
    WorkerPod=list_to_atom(WorkerPodStr),

    WorkerPodId_2="pod_worker_2",
    WorkerPodStr_2=WorkerPodId_2++"@"++Host,
    WorkerPod_2=list_to_atom(WorkerPodStr_2),

    MasterPodId="pod_master_1",
    MasterPodStr=MasterPodId++"@"++Host,
    MasterPod=list_to_atom(MasterPodStr),

    [{"node_worker_1",node_worker_1@asus}]=sd_service:worker_nodes(), % node_worker_id@Host All workers in the system(clustere)
    [{"node_master_1",node_master_1@asus}]=sd_service:master_nodes(), % node_master_id@Host All master in the system(clustere)
    [{"pod_worker_1",pod_worker_1@asus}]=sd_service:worker_pods(), % pod_worker_id@Host wrokerAll pods on node Node
    [{"pod_master_1",pod_master_1@asus}]=sd_service:master_pods(),

    [{{"node_worker_1",node_worker_1@asus},_T100}]=sd_service:worker_nodes_time(), % node_worker_id@Host All workers in the system(clustere)
    [{{"node_master_1",node_master_1@asus},_T101}]=sd_service:master_nodes_time(), % node_master_id@Host All master in the system(clustere)
    [{{"pod_worker_1",pod_worker_1@asus},_T102}]=sd_service:worker_pods_time(), % pod_worker_id@Host wrokerAll pods on node Node
    [{{"pod_master_1",pod_master_1@asus},_T103}]=sd_service:master_pods_time(),

    [{"Master_1",pod_master_1@asus},
     {"ServiceId_1",pod_worker_2@asus},
     {"ServiceId_1",pod_worker_1@asus}]=sd_service:services(), 

    [{{"Master_1",pod_master_1@asus},_T1},
           {{"ServiceId_1",pod_worker_2@asus},_T2},
           {{"ServiceId_1",pod_worker_1@asus},_T3}]=sd_service:services_time(), 

    [{"ServiceId_1",pod_worker_2@asus},
     {"ServiceId_1",pod_worker_1@asus}]=sd_service:service("ServiceId_1"),
    [{"Master_1",pod_master_1@asus}]=sd_service:service("Master_1"),
    []=sd_service:service("glurk"),

    [{{"ServiceId_1",pod_worker_2@asus},_T10},
     {{"ServiceId_1",pod_worker_1@asus},_T11}]=sd_service:service_time("ServiceId_1"),
    [{{"Master_1",pod_master_1@asus},_T12}]=sd_service:service_time("Master_1"),
    []=sd_service:service_time("glurk"),
    ok.

delete_info_test()->
    {ok,Host}=inet:gethostname(),

    WorkerNodeId="node_worker_1",
    WorkerNodeStr=WorkerNodeId++"@"++Host,
    WorkerNode=list_to_atom(WorkerNodeStr),
    MasterNodeId="node_master_1",
    MasterNodeStr=MasterNodeId++"@"++Host,
    MasterNode=list_to_atom(MasterNodeStr),

    WorkerPodId="pod_worker_1",
    WorkerPodStr=WorkerPodId++"@"++Host,
    WorkerPod=list_to_atom(WorkerPodStr),

    WorkerPodId_2="pod_worker_2",
    WorkerPodStr_2=WorkerPodId_2++"@"++Host,
    WorkerPod_2=list_to_atom(WorkerPodStr_2),

    MasterPodId="pod_master_1",
    MasterPodStr=MasterPodId++"@"++Host,
    MasterPod=list_to_atom(MasterPodStr),

    ok=sd_service:delete_worker_node(WorkerNodeId,WorkerNode),
    ok=sd_service:delete_master_node(MasterNodeId,MasterNode),
    []=sd_service:worker_nodes(),
    []=sd_service:master_nodes(),

    ok=sd_service:delete_worker_pod(WorkerPodId,WorkerPod),
    ok=sd_service:delete_master_pod(MasterPodId,MasterPod),
    []=sd_service:worker_pods(),
    []=sd_service:master_pods(),

    ok=sd_service:delete_service("ServiceId_1",WorkerPod),
    ok=sd_service:delete_service("ServiceId_1",WorkerPod_2),
    ok=sd_service:delete_service("Master_1",MasterPod),
    []=sd_service:services(), 
    
    ok.


absents_test()->
    {ok,Host}=inet:gethostname(),

    WorkerNodeId="node_worker_1",
    WorkerNodeStr=WorkerNodeId++"@"++Host,
    WorkerNode=list_to_atom(WorkerNodeStr),
    MasterNodeId="node_master_1",
    MasterNodeStr=MasterNodeId++"@"++Host,
    MasterNode=list_to_atom(MasterNodeStr),

    WorkerPodId="pod_worker_1",
    WorkerPodStr=WorkerPodId++"@"++Host,
    WorkerPod=list_to_atom(WorkerPodStr),

    WorkerPodId_2="pod_worker_2",
    WorkerPodStr_2=WorkerPodId_2++"@"++Host,
    WorkerPod_2=list_to_atom(WorkerPodStr_2),

    MasterPodId="pod_master_1",
    MasterPodStr=MasterPodId++"@"++Host,
    MasterPod=list_to_atom(MasterPodStr),
       
    ok=sd_service:add_worker_node(WorkerNodeId,WorkerNode),
    ok=sd_service:add_master_node(MasterNodeId,MasterNode),

    ok=sd_service:add_worker_pod(WorkerPodId,WorkerPod),
    ok=sd_service:add_master_pod(MasterPodId,MasterPod),

    ok=sd_service:register_service("ServiceId_1",WorkerPod),
    ok=sd_service:register_service("ServiceId_1",WorkerPod_2),
    ok=sd_service:register_service("Master_1",MasterPod),

     [{"node_worker_1",node_worker_1@asus}]=sd_service:worker_nodes(), % node_worker_id@Host All workers in the system(clustere)
    [{"node_master_1",node_master_1@asus}]=sd_service:master_nodes(), % node_master_id@Host All master in the system(clustere)
    [{"pod_worker_1",pod_worker_1@asus}]=sd_service:worker_pods(), % pod_worker_id@Host wrokerAll pods on node Node
    [{"pod_master_1",pod_master_1@asus}]=sd_service:master_pods(),

    [{{"node_worker_1",node_worker_1@asus},_T100}]=sd_service:worker_nodes_time(), % node_worker_id@Host All workers in the system(clustere)
    [{{"node_master_1",node_master_1@asus},_T101}]=sd_service:master_nodes_time(), % node_master_id@Host All master in the system(clustere)
    [{{"pod_worker_1",pod_worker_1@asus},_T102}]=sd_service:worker_pods_time(), % pod_worker_id@Host wrokerAll pods on node Node
    [{{"pod_master_1",pod_master_1@asus},_T103}]=sd_service:master_pods_time(),

    [{"Master_1",pod_master_1@asus},
     {"ServiceId_1",pod_worker_2@asus},
     {"ServiceId_1",pod_worker_1@asus}]=sd_service:services(), 

    [{{"Master_1",pod_master_1@asus},_T1},
           {{"ServiceId_1",pod_worker_2@asus},_T2},
           {{"ServiceId_1",pod_worker_1@asus},_T3}]=sd_service:services_time(), 

    [{"ServiceId_1",pod_worker_2@asus},
     {"ServiceId_1",pod_worker_1@asus}]=sd_service:service("ServiceId_1"),
    [{"Master_1",pod_master_1@asus}]=sd_service:service("Master_1"),
    []=sd_service:service("glurk"),

    [{{"ServiceId_1",pod_worker_2@asus},_T10},
     {{"ServiceId_1",pod_worker_1@asus},_T11}]=sd_service:service_time("ServiceId_1"),
    [{{"Master_1",pod_master_1@asus},_T12}]=sd_service:service_time("Master_1"),
    []=sd_service:service_time("glurk"),


    %%----- absensts test

    timer:sleep(1000),
    []=sd_service:worker_nodes(),
    []=sd_service:master_nodes(), 
    []=sd_service:worker_pods(), 
    []=sd_service:master_pods(),

    []=sd_service:worker_nodes_time(), 
    []=sd_service:master_nodes_time(), 
    []=sd_service:worker_pods_time(), 
    []=sd_service:master_pods_time(),

    []=sd_service:services(), 

    []=sd_service:services_time(), 

    []=sd_service:service("Master_1"),
    []=sd_service:service("glurk"),

    []=sd_service:service_time("ServiceId_1"),
    []=sd_service:service_time("Master_1"),


    ok.    


delete_info_2_test()->
    {ok,Host}=inet:gethostname(),

    WorkerNodeId="node_worker_1",
    WorkerNodeStr=WorkerNodeId++"@"++Host,
    WorkerNode=list_to_atom(WorkerNodeStr),
    MasterNodeId="node_master_1",
    MasterNodeStr=MasterNodeId++"@"++Host,
    MasterNode=list_to_atom(MasterNodeStr),

    WorkerPodId="pod_worker_1",
    WorkerPodStr=WorkerPodId++"@"++Host,
    WorkerPod=list_to_atom(WorkerPodStr),

    WorkerPodId_2="pod_worker_2",
    WorkerPodStr_2=WorkerPodId_2++"@"++Host,
    WorkerPod_2=list_to_atom(WorkerPodStr_2),

    MasterPodId="pod_master_1",
    MasterPodStr=MasterPodId++"@"++Host,
    MasterPod=list_to_atom(MasterPodStr),

    ok=sd_service:delete_worker_node(WorkerNodeId,WorkerNode),
    ok=sd_service:delete_master_node(MasterNodeId,MasterNode),
    []=sd_service:worker_nodes(),
    []=sd_service:master_nodes(),

    ok=sd_service:delete_worker_pod(WorkerPodId,WorkerPod),
    ok=sd_service:delete_master_pod(MasterPodId,MasterPod),
    []=sd_service:worker_pods(),
    []=sd_service:master_pods(),

    ok=sd_service:delete_service("ServiceId_1",WorkerPod),
    ok=sd_service:delete_service("ServiceId_1",WorkerPod_2),
    ok=sd_service:delete_service("Master_1",MasterPod),
    []=sd_service:services(), 
    
    ok.

stop_test()->
    ok=application:stop(sd_service),
    ok=application:unload(sd_service),
    kill(),
    ok.
kill()->
    init:stop().
