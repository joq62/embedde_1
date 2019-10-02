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

%% --------------------------------------------------------------------

%% External exports


-export([add_worker_node/3,delete_worker_node/3,
	 add_master_node/3,delete_master_node/3,
	 add_worker_pod/3,delete_worker_pod/3,
	 add_master_pod/3,delete_master_pod/3,
	 add_service/3,delete_service/3,service/2,service_time/2,services/1,services_time/1
	]).


%% ====================================================================
%% External functions
%% ====================================================================

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
add_service(ServiceId,Pod,Services)->
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
    NewWorkerNodes=lists:delete({WorkerNodeId,WorkerNode},WorkerNodes),
    {ok,NewWorkerNodes}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_master_node(MasterNodeId,MasterNode,MasterNodes)->
    NewMasterNodes=lists:delete({MasterNodeId,MasterNode},MasterNodes),
    {ok,NewMasterNodes}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_worker_node(WorkerNodeId,WorkerNode,WorkerNodes)->
    Result=case lists:member({WorkerNodeId,WorkerNode},WorkerNodes) of
	      false->
		  {ok,[{WorkerNodeId,WorkerNode}|WorkerNodes]};
	      true ->
		  {ok,WorkerNodes}
	  end,
    Result.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_master_node(MasterNodeId,MasterNode,MasterNodes)->
    Result=case lists:member({MasterNodeId,MasterNode},MasterNodes) of
	      false->
		  {ok,[{MasterNodeId,MasterNode}|MasterNodes]};
	      true ->
		  {ok,MasterNodes}
	  end,
    Result.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_worker_pod(WorkerPodId,WorkerPod,WorkerPods)->
    NewWorkerPods=lists:delete({WorkerPodId,WorkerPod},WorkerPods),
    {ok,NewWorkerPods}.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_master_pod(MasterPodId,MasterPod,MasterPods)->
    NewMasterPods=lists:delete({MasterPodId,MasterPod},MasterPods),
    {ok,NewMasterPods}.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_worker_pod(WorkerPodId,WorkerPod,WorkerPods)->
    Result=case lists:member({WorkerPodId,WorkerPod},WorkerPods) of
	      false->
		  {ok,[{WorkerPodId,WorkerPod}|WorkerPods]};
	      true ->
		  {ok,WorkerPods}
	  end,
    Result.
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add_master_pod(MasterPodId,MasterPod,MasterPods)->
    Result=case lists:member({MasterPodId,MasterPod},MasterPods) of
	      false->
		  {ok,[{MasterPodId,MasterPod}|MasterPods]};
	      true ->
		  {ok,MasterPods}
	  end,
    Result.


    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%filter_events(Key
