%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sd_service). 

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%-define(HB_INTERVAL,1*60*1000).

-define(HB_INTERVAL,1*30*1000).
%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{worker_nodes,master_nodes,
	       worker_pods,master_pods,
	       services
	      }).

%% --------------------------------------------------------------------




-export([add_worker_node/2,delete_worker_node/2,
	 add_master_node/2,delete_master_node/2,
	 worker_nodes/0,master_nodes/0,
	 worker_nodes_time/0,master_nodes_time/0,

	 add_worker_pod/2,delete_worker_pod/2,
	 add_master_pod/2,delete_master_pod/2,
	 worker_pods/0,master_pods/0,
	 worker_pods_time/0,master_pods_time/0,

	 register_service/2,delete_service/2,
	 services/0,service/1,services_time/0,service_time/1,
	 
	 heart_beat/1
	]).

-export([start/0,
	 stop/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals



%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).



%%-----------------------------------------------------------------------
services()->
     gen_server:call(?MODULE, {services},infinity).
service(ServiceId)->
    gen_server:call(?MODULE, {service,ServiceId},infinity).
services_time()->
     gen_server:call(?MODULE, {services_time},infinity).
service_time(ServiceId)->
    gen_server:call(?MODULE, {service_time,ServiceId},infinity).

worker_nodes()->
    gen_server:call(?MODULE, {worker_nodes},infinity).
master_nodes()->
    gen_server:call(?MODULE, {master_nodes},infinity).
worker_nodes_time()->
    gen_server:call(?MODULE, {worker_nodes_time},infinity).
master_nodes_time()->
    gen_server:call(?MODULE, {master_nodes_time},infinity).

worker_pods()->
    gen_server:call(?MODULE, {worker_pods},infinity).
master_pods()->
    gen_server:call(?MODULE, {master_pods},infinity).
worker_pods_time()->
    gen_server:call(?MODULE, {worker_pods_time},infinity).
master_pods_time()->
    gen_server:call(?MODULE, {master_pods_time},infinity).



%%-----------------------------------------------------------------------
register_service(ServiceId,Pod)->
    gen_server:cast(?MODULE, {register_service,ServiceId,Pod}).
delete_service(ServiceId,Pod)->
    gen_server:cast(?MODULE, {delete_service,ServiceId,Pod}).


add_worker_node(WorkerNodeId,WorkerNode)->
    gen_server:cast(?MODULE, {add_worker_node,WorkerNodeId,WorkerNode}).
delete_worker_node(WorkerNodeId,WorkerNode)->
    gen_server:cast(?MODULE, {delete_worker_node,WorkerNodeId,WorkerNode}).

add_master_node(MasterNodeId,MasterNode)->
    gen_server:cast(?MODULE, {add_master_node,MasterNodeId,MasterNode}).
delete_master_node(MasterNodeId,MasterNode)->
    gen_server:cast(?MODULE, {delete_master_node,MasterNodeId,MasterNode}).

add_worker_pod(WorkerPodId,WorkerPod)->
    gen_server:cast(?MODULE, {add_worker_pod,WorkerPodId,WorkerPod}).
delete_worker_pod(WorkerPodId,WorkerPod)->
    gen_server:cast(?MODULE, {delete_worker_pod,WorkerPodId,WorkerPod}).

add_master_pod(MasterPodId,MasterPod)->
    gen_server:cast(?MODULE, {add_master_pod,MasterPodId,MasterPod}).
delete_master_pod(MasterPodId,MasterPod)->
    gen_server:cast(?MODULE, {delete_master_pod,MasterPodId,MasterPod}).

heart_beat(Interval)->
    gen_server:cast(?MODULE, {heart_beat,Interval}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    spawn(fun()->h_beat(?HB_INTERVAL) end),
    io:format("Dbg ~p~n",[{?MODULE, application_started}]),
    {ok, #state{worker_nodes=[],master_nodes=[],
		worker_pods=[],master_pods=[],
		services=[]
	       }}.   
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({services}, _From, State) ->
    Reply=case rpc:call(node(),sd,services,[State#state.services],5000) of
	      {ok,AvailableServices}->
		  AvailableServices;
	      {badrpc,Err}->
						% call logger error
		  {error,[badrpc,Err,?MODULE,?LINE]};
	      Err->
						% call logger error
		  {error,[unknown_error,Err,?MODULE,?LINE]}
	  end,   
    {reply, Reply, State};

handle_call({service,ServiceId}, _From, State) ->
    Reply=case rpc:call(node(),sd,service,[ServiceId,State#state.services],5000) of
	      {ok,AvailableServices}->
		  AvailableServices;
	      {badrpc,Err}->
		% call logger error
		  {error,[badrpc,Err,?MODULE,?LINE]};
	      Err->
		% call logger error
		  {error,[unknown_error,Err,?MODULE,?LINE]}
	  end,   
    {reply, Reply, State};


handle_call({services_time}, _From, State) ->
    Reply=case rpc:call(node(),sd,services_time,[State#state.services],5000) of
	      {ok,AvailableServices}->
		  AvailableServices;
	      {badrpc,Err}->
						% call logger error
		  {error,[badrpc,Err,?MODULE,?LINE]};
	      Err->
						% call logger error
		  {error,[unknown_error,Err,?MODULE,?LINE]}
	  end,   
    {reply, Reply, State};

handle_call({service_time,ServiceId}, _From, State) ->
    Reply=case rpc:call(node(),sd,service_time,[ServiceId,State#state.services],5000) of
	      {ok,AvailableServices}->
		  AvailableServices;
	      {badrpc,Err}->
		% call logger error
		  {error,[badrpc,Err,?MODULE,?LINE]};
	      Err->
		% call logger error
		  {error,[unknown_error,Err,?MODULE,?LINE]}
	  end,   
    {reply, Reply, State};

%%----------------------------------- 
handle_call({worker_nodes}, _From, State) ->
    Reply=case rpc:call(node(),sd,worker_nodes,[State#state.worker_nodes],5000) of
	      {ok,AvailableNodes}->
		  AvailableNodes;
	      {badrpc,Err}->
						% call logger error
		  {error,[badrpc,Err,?MODULE,?LINE]};
	      Err->
						% call logger error
		  {error,[unknown_error,Err,?MODULE,?LINE]}
	  end,   
    {reply, Reply, State};

handle_call({worker_nodes_time}, _From, State) ->
    Reply=State#state.worker_nodes,
    {reply, Reply, State};

handle_call({master_nodes}, _From, State) ->
    Reply=case rpc:call(node(),sd,master_nodes,[State#state.master_nodes],5000) of
	      {ok,AvailableNodes}->
		  AvailableNodes;
	      {badrpc,Err}->
						% call logger error
		  {error,[badrpc,Err,?MODULE,?LINE]};
	      Err->
						% call logger error
		  {error,[unknown_error,Err,?MODULE,?LINE]}
	  end,   
    {reply, Reply, State};

handle_call({master_nodes_time}, _From, State) ->
    Reply=State#state.master_nodes,
    {reply, Reply, State};

%-------------------------------------------------------
handle_call({worker_pods}, _From, State) ->
 Reply=case rpc:call(node(),sd,worker_pods,[State#state.worker_pods],5000) of
	      {ok,AvailablePods}->
		  AvailablePods;
	      {badrpc,Err}->
						% call logger error
		  {error,[badrpc,Err,?MODULE,?LINE]};
	      Err->
						% call logger error
		  {error,[unknown_error,Err,?MODULE,?LINE]}
	  end,   
    {reply, Reply, State};

handle_call({worker_pods_time}, _From, State) ->
    Reply=State#state.worker_pods,
    {reply, Reply, State};

handle_call({master_pods}, _From, State) ->
 Reply=case rpc:call(node(),sd,master_pods,[State#state.master_pods],5000) of
	      {ok,AvailablePods}->
		  AvailablePods;
	      {badrpc,Err}->
						% call logger error
		  {error,[badrpc,Err,?MODULE,?LINE]};
	      Err->
						% call logger error
		  {error,[unknown_error,Err,?MODULE,?LINE]}
	  end,   
    {reply, Reply, State};

handle_call({master_pods_time}, _From, State) ->
    Reply=State#state.master_pods,
    {reply, Reply, State};



handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {error,[unmatched_signal,Request,From,?MODULE,?LINE]},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( {register_service,ServiceId,Pod}, State) ->
    case rpc:call(node(),sd,register_service,[ServiceId,Pod,State#state.services],5000) of
	{ok,NewServices}->
	    NewState=State#state{services=NewServices};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};

handle_cast( {delete_service,ServiceId,Pod}, State) ->
    case rpc:call(node(),sd,delete_service,[ServiceId,Pod,State#state.services],5000) of
	{ok,NewServices}->
	    NewState=State#state{services=NewServices};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};



%%--------------------------------- Node --------------------


handle_cast( {add_worker_node,WorkerNodeId,WorkerNode}, State) ->
    case rpc:call(node(),sd,add_worker_node,[WorkerNodeId,WorkerNode,State#state.worker_nodes],5000) of
	{ok,NewWorkerNodes}->
	    NewState=State#state{worker_nodes=NewWorkerNodes};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};

handle_cast( {add_master_node,MasterNodeId,MasterNode}, State) ->
    case rpc:call(node(),sd,add_master_node,[MasterNodeId,MasterNode,State#state.master_nodes],5000) of
	{ok,NewMasterNodes}->
	    NewState=State#state{master_nodes=NewMasterNodes};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};

handle_cast( {delete_worker_node,WorkerNodeId,WorkerNode}, State) ->
    case rpc:call(node(),sd,delete_worker_node,[WorkerNodeId,WorkerNode,State#state.worker_nodes],5000) of
	{ok,NewWorkerNodes}->
	    NewState=State#state{worker_nodes=NewWorkerNodes};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};

handle_cast( {delete_master_node,MasterNodeId,MasterNode}, State) ->
    case rpc:call(node(),sd,delete_master_node,[MasterNodeId,MasterNode,State#state.master_nodes],5000) of
	{ok,NewMasterNodes}->
	    NewState=State#state{master_nodes=NewMasterNodes};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};
%----------------------------------- Pod -----------------

handle_cast( {add_worker_pod,WorkerPodId,WorkerPod}, State) ->
    case rpc:call(node(),sd,add_worker_pod,[WorkerPodId,WorkerPod,State#state.worker_pods],5000) of
	{ok,NewWorkerPods}->
	    NewState=State#state{worker_pods=NewWorkerPods};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};

handle_cast( {add_master_pod,MasterPodId,MasterPod}, State) ->
    case rpc:call(node(),sd,add_master_pod,[MasterPodId,MasterPod,State#state.master_pods],5000) of
	{ok,NewMasterPods}->
	    NewState=State#state{master_pods=NewMasterPods};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};

handle_cast( {delete_worker_pod,WorkerPodId,WorkerPod}, State) ->
    case rpc:call(node(),sd,delete_worker_pod,[WorkerPodId,WorkerPod,State#state.worker_pods],5000) of
	{ok,NewWorkerPods}->
	    NewState=State#state{worker_pods=NewWorkerPods};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};

handle_cast( {delete_master_pod,MasterPodId,MasterPod}, State) ->
    case rpc:call(node(),sd,delete_master_pod,[MasterPodId,MasterPod,State#state.master_pods],5000) of
	{ok,NewMasterPods}->
	    NewState=State#state{master_pods=NewMasterPods};
	{badrpc,_Err}->
	    % call logger error
	    NewState=State;
	_Err->
	    % call logger error
	    NewState=State
    end,    
    {noreply, NewState};

%%-----------------------------------------------------------------------

handle_cast({heart_beat,Interval}, State) ->
    UpdatedWorkerNodes=rpc:call(node(),sd,update_sd_list,[State#state.worker_nodes],5000),
    UpdatedMasterNodes=rpc:call(node(),sd,update_sd_list,[State#state.master_nodes],5000),

    UpdatedWorkerPods=rpc:call(node(),sd,update_sd_list,[State#state.worker_pods],5000),
    UpdatedMasterPods=rpc:call(node(),sd,update_sd_list,[State#state.master_pods],5000),

    UpdatedServices=rpc:call(node(),sd,update_sd_list,[State#state.services],5000),
    NewState=State#state{worker_nodes=UpdatedWorkerNodes,master_nodes=UpdatedMasterNodes,
			 worker_pods=UpdatedWorkerPods,master_pods=UpdatedMasterPods,
			 services=UpdatedServices},
    spawn(fun()->h_beat(Interval) end),    
    {noreply, NewState};


handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
h_beat(Interval)->
    timer:sleep(Interval),
    rpc:cast(node(),?MODULE,heart_beat,[Interval]).
	     

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

