%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sd).
  


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-define(HEARTBEAT_INTERVAL,1*20*1000).
-define(INACITIVITY_TIMEOUT,1*92*1000).
%-define(INACITIVITY_TIMEOUT,1**100).
%% --------------------------------------------------------------------

%% External exports


-export([add_worker_node/3,delete_worker_node/3,
	 add_master_node/3,delete_master_node/3,
	 worker_nodes/1,master_nodes/1,

	 add_worker_pod/3,delete_worker_pod/3,
	 add_master_pod/3,delete_master_pod/3,
	 worker_pods/1,master_pods/1,

	 register_service/3,delete_service/3,
	 service/2,service_time/2,services/1,services_time/1,
	 update_sd_list/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
update_sd_list(List)->
    Now=erlang:system_time(),
    UpdatedList=[{{Id,ErlVm},Now}||{{Id,ErlVm},TimeStamp}<-List,
			 TimeStamp/1000>?INACITIVITY_TIMEOUT,
				  false==(Id=:="sd_service")],
    UpdatedList.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
master_pods(MasterPods)->
    AvailablePods=[{PodId,Pod}||{{PodId,Pod},_Time}<-MasterPods],
    {ok,AvailablePods}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
master_nodes(MasterNodes)->
    AvailableNodes=[{NodeId,Node}||{{NodeId,Node},_Time}<-MasterNodes],
    {ok,AvailableNodes}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
worker_pods(WorkerPods)->
    AvailablePods=[{PodId,Pod}||{{PodId,Pod},_Time}<-WorkerPods],
    {ok,AvailablePods}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
worker_nodes(WorkerNodes)->
    AvailableNodes=[{NodeId,Node}||{{NodeId,Node},_Time}<-WorkerNodes],
    {ok,AvailableNodes}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

services(Services)->
    AvailableServices=[{ServiceId,Pod}||{{ServiceId,Pod},_Time}<-Services],
    {ok,AvailableServices}.

services_time(Services)->
    AvailableServices=[{{ServiceId,Pod},Time}||{{ServiceId,Pod},Time}<-Services],
    {ok,AvailableServices}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
service(WantedServiceId,Services)->
    AvailableServices=[{ServiceId,Pod}||{{ServiceId,Pod},_Time}<-Services,
					ServiceId==WantedServiceId],
    {ok,AvailableServices}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
service_time(WantedServiceId,Services)->
    AvailableServices=[{{ServiceId,Pod},Time}||{{ServiceId,Pod},Time}<-Services,
					ServiceId==WantedServiceId],
    {ok,AvailableServices}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
register_service(ServiceId,Pod,Services)->
    Now=erlang:system_time(),
    Result=case lists:keymember({ServiceId,Pod},1,Services) of
	      false->
		  {ok,[{{ServiceId,Pod},Now}|Services]};
	      true ->
		   NewServices=lists:keyreplace({ServiceId,Pod}, 1, Services, {{ServiceId,Pod},Now}),
		   {ok,NewServices}
	  end,
    Result.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_service(ServiceId,Pod,Services)->
    NewServices=lists:keydelete({ServiceId,Pod},1,Services),
    {ok,NewServices}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_worker_node(WorkerNodeId,WorkerNode,WorkerNodes)->
    NewWorkerNodes=lists:keydelete({WorkerNodeId,WorkerNode},1,WorkerNodes),
    {ok,NewWorkerNodes}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_master_node(MasterNodeId,MasterNode,MasterNodes)->
    NewMasterNodes=lists:keydelete({MasterNodeId,MasterNode},1,MasterNodes),
    {ok,NewMasterNodes}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_worker_node(WorkerNodeId,WorkerNode,WorkerNodes)->
    Now=erlang:system_time(),
    Result=case lists:keymember({WorkerNodeId,WorkerNode},1,WorkerNodes) of
	      false->
		  {ok,[{{WorkerNodeId,WorkerNode},Now}|WorkerNodes]};
	      true ->
		   NewWorkerNodes=lists:keyreplace({WorkerNodeId,WorkerNode}, 1, WorkerNodes, {{WorkerNodeId,WorkerNode},Now}),
		   {ok,NewWorkerNodes}
	  end,
    Result.
 %   Result=case lists:member({WorkerNodeId,WorkerNode},WorkerNodes) of
%	      false->
%		  {ok,[{WorkerNodeId,WorkerNode}|WorkerNodes]};
%	      true ->
%		  {ok,WorkerNodes}
%	  end,
%    Result.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_master_node(MasterNodeId,MasterNode,MasterNodes)->
%    Result=case lists:member({MasterNodeId,MasterNode},MasterNodes) of
%	      false->
%		  {ok,[{MasterNodeId,MasterNode}|MasterNodes]};
%	      true ->
%		  {ok,MasterNodes}
%	  end,
 %   Result.

    Now=erlang:system_time(),
    Result=case lists:keymember({MasterNodeId,MasterNode},1,MasterNodes) of
	      false->
		  {ok,[{{MasterNodeId,MasterNode},Now}|MasterNodes]};
	      true ->
		   NewMasterNodes=lists:keyreplace({MasterNodeId,MasterNode}, 1, MasterNodes, {{MasterNodeId,MasterNode},Now}),
		   {ok,NewMasterNodes}
	  end,
    Result.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_worker_pod(WorkerPodId,WorkerPod,WorkerPods)->
    NewWorkerPods=lists:keydelete({WorkerPodId,WorkerPod},1,WorkerPods),
    {ok,NewWorkerPods}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_master_pod(MasterPodId,MasterPod,MasterPods)->
    NewMasterPods=lists:keydelete({MasterPodId,MasterPod},1,MasterPods),
    {ok,NewMasterPods}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_worker_pod(WorkerPodId,WorkerPod,WorkerPods)->
 %   Result=case lists:member({WorkerPodId,WorkerPod},WorkerPods) of
%	      false->
%		  {ok,[{WorkerPodId,WorkerPod}|WorkerPods]};
%	      true ->
%		  {ok,WorkerPods}
%	  end,
 %   Result.

    Now=erlang:system_time(),
    Result=case lists:keymember({WorkerPodId,WorkerPod},1,WorkerPods) of
	      false->
		  {ok,[{{WorkerPodId,WorkerPod},Now}|WorkerPods]};
	      true ->
		   NewWorkerPods=lists:keyreplace({WorkerPodId,WorkerPod}, 1, WorkerPods, {{WorkerPodId,WorkerPod},Now}),
		   {ok,NewWorkerPods}
	  end,
    Result.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_master_pod(MasterPodId,MasterPod,MasterPods)->
%    Result=case lists:member({MasterPodId,MasterPod},MasterPods) of
%	      false->
%		  {ok,[{MasterPodId,MasterPod}|MasterPods]};
%	      true ->
%		  {ok,MasterPods}
%	  end,
 %   Result.

    Now=erlang:system_time(),
    Result=case lists:keymember({MasterPodId,MasterPod},1,MasterPods) of
	      false->
		  {ok,[{{MasterPodId,MasterPod},Now}|MasterPods]};
	      true ->
		   NewMasterPods=lists:keyreplace({MasterPodId,MasterPod}, 1, MasterPods, {{MasterPodId,MasterPod},Now}),
		   {ok,NewMasterPods}
	  end,
    Result.
    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
