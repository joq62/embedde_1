%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(controller).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(OS_CMD_1000,1000).
-define(START_POD_INTERVAL,50).
-define(START_POD_TRIES,50).
-define(STOP_POD_INTERVAL,50).
-define(STOP_POD_TRIES,50).

-define(GITHUB,"/home/pi/erlang/erlang_embedded_system_1/github").
-define(UPDATE_SERVICE(Service,ServiceInfoRecord),controller_lib:update({service,Service},ServiceInfoRecord)).




%% intermodule 
-export([get_nodes/0,
	create_pod/2, delete_pod/2,get_pods/0
	]).
%% External exports

%-compile(export_all).
%% ====================================================================
%% External functions
%% ===================================================================
delete_pod(Node,PodId)->
    % Pod='PodId@Host'
    Result=case rpc:call(Node,inet,gethostname,[],5000) of
	       {ok,Host}->
		   PodStr=PodId++"@"++Host,
		   Pod=list_to_atom(PodStr),
		   rpc:call(Pod,init,stop,[],5000),
		    case check_if_pod_stopped(Pod,?STOP_POD_INTERVAL,?STOP_POD_TRIES,error) of
			error->
			    {error,[couldnt_stop_pod,PodId,?MODULE,?LINE]};
			ok->
			    RmCmd="rm -rf "++PodId,
			    case rpc:call(Node,os,cmd,[RmCmd],5000) of
				[]->
				    {ok,stopped};
				Err ->
				    {error,[unknown_error,Err,?MODULE,?LINE]}
			    end
		    end;
	       {badrpc,Err}->
		   {error,[badrpc,Err,?MODULE,?LINE]};
	       Err ->
		   {error,[unknown_error,Err,?MODULE,?LINE]}
	   end,
    Result.
		       


check_if_pod_stopped(_Pod,_Interval,0,ok)->
    ok;
check_if_pod_stopped(_Pod,_Interval,0,error)->
    error;
check_if_pod_stopped(_Pod,_Interval,_N,ok) ->
    ok;
check_if_pod_stopped(Pod,Interval,N,error) ->
    timer:sleep(Interval),
    case net_adm:ping(Pod) of
	pong->
	    NewResult=error;
	pang->
	    NewResult=ok
    end,
    check_if_pod_stopped(Pod,Interval,N-1,NewResult).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_pod(Node,PodId)->
    Result= case create_pod_dir(Node,PodId) of
		{ok,PodStr}->
		    case start_pod(Node,PodId,PodStr) of
			{ok,Pod}->
			    {ok,Pod};
			{error,Err}->
			    {error,Err}
		    end;
		{error,Err}->
		    {error,Err}
	    end,
    Result.

start_pod(Node,PodId,PodStr)->
    ErlCmd="erl -pa "++PodId++"/* "++"-sname "++PodStr++" -detached",
    Result= case rpc:call(Node,os,cmd,[ErlCmd],5000) of
		[]->
		    case check_if_pod_started(list_to_atom(PodStr),?START_POD_INTERVAL,?START_POD_TRIES,error) of
			error->
			    {error,[couldnt_start_pod,PodId,?MODULE,?LINE]};
			ok->
			    {ok,list_to_atom(PodStr)}
		    end;
	        {badrpc,Err}->
		    {error,[badrpc,Err,?MODULE,?LINE]};
		Err ->
		    {error,[unknown_error,Err,?MODULE,?LINE]}
	    end,
    Result.			
create_pod_dir(Node,PodId)->
    % Pod='PodId@Host'
    Result=case rpc:call(Node,inet,gethostname,[],5000) of
	       {ok,Host}->
		   PodStr=PodId++"@"++Host,
		   %Pod=list_to_atom(PodStr),
		   case rpc:call(Node,filelib,is_dir,[PodId],5000) of
		       true->
			   rpc:call(Node,os,cmd,["rm -rf "++PodId],5000),
			   {error,[pod_already_loaded,PodId,?MODULE,?LINE]};
		       false-> 
			   case rpc:call(Node,file,make_dir,[PodId],5000) of
			       ok->
				   {ok,PodStr};
			       {badrpc,Err}->
				   {error,[badrpc,Err,?MODULE,?LINE]};
			       Err ->
				   {error,[unknown_error,Err,?MODULE,?LINE]}
			   end;
		       {badrpc,Err}->
			   {error,[badrpc,Err,?MODULE,?LINE]};
		       Err ->
			   {error,[unknown_error,Err,?MODULE,?LINE]}
		   end;
	       {badrpc,Err}->
		   {error,[badrpc,Err,?MODULE,?LINE]};
	       Err ->
		   {error,[unknown_error,Err,?MODULE,?LINE]}
	   end,
    Result.

check_if_pod_started(_Pod,_Interval,0,ok)->
    ok;
check_if_pod_started(_Pod,_Interval,0,error)->
    error;
check_if_pod_started(_Pod,_Interval,_N,ok) ->
    ok;
check_if_pod_started(Pod,Interval,N,error) ->
    timer:sleep(Interval),
    case net_adm:ping(Pod) of
	pang->
	    NewResult=error;
	pong ->
	    NewResult=ok
    end,
    check_if_pod_started(Pod,Interval,N-1,NewResult).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

get_pods()->
    AllNodes=nodes(),
    Result=get_pods(AllNodes,[]),
    Result.
    
get_pods([],Pods)->	   
    Pods;
get_pods([Node|T],Acc) ->	
    [W|_]=string:tokens(atom_to_list(Node),"_"),
    case W of
	"worker"->
	    NewAcc=Acc;
	"controller"->
	    NewAcc=Acc;
	_ ->
	    NewAcc=[Node|Acc]
    end,
    get_pods(T,NewAcc).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

get_nodes()->
    AllNodes=nodes(),
    Result=get_nodes(AllNodes,[]),
    Result.
    
get_nodes([],Nodes)->	   
    Nodes;
get_nodes([Node|T],Acc) ->	
    [W|_]=string:tokens(atom_to_list(Node),"_"),
    case W of
	"worker"->
	    NewAcc=[Node|Acc];
	"controller"->
	    NewAcc=[Node|Acc];
	_ ->
	    NewAcc=Acc
    end,
    get_nodes(T,NewAcc).



%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: unload_service(Service,BoardNode)
%% Description:
%% Returns:ok|{error,Err}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:stop_service_node(Service,WorkerNode)
%% Description:
%% Returns:ok|{error,Err}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:create_worker_node(Service,BoardNode)
%% Description:
%% Returns:{ok,PidService}|{error,Err}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
check_del_service_dir(Service,BoardNode)->
    Result=case rpc:call(BoardNode,filelib,is_dir,[Service],5000) of
	       true->
		   case rpc:call(BoardNode,os,cmd,["rm -rf "++Service]) of
		       []->
			   timer:sleep(?OS_CMD_1000),
			   ok;
		       {badrpc,nodedown} ->
			   {error,nodedown};
		       Err->
			   {error,Err}
		   end;
	       false->
		   ok;
	       {badrpc,nodedown} ->
		   {error,nodedown};
	       Err->
		   {error,Err}
	   end,
    Result.

%% --------------------------------------------------------------------
%% Function:clone_compile(Service,BoardNode)
%% Description:
%% Returns: ok|{erro,compile_info}|{error,nodedown}
%% --------------------------------------------------------------------
clone_compile(Pod,Service)->
    Result=case clone(Service,Pod) of
	       ok->
		   compile_erl(filename:join(Service,"src"),filename:join(Service,"ebin"),Pod);
	       {badrpc,nodedown}->
		   {error,nodedown};
	       Err->
		   io:format(" ~p~n",[{?MODULE,?LINE,Err}]),
		   {error,[compiler_error,Err,?MODULE,?LINE]}
	   end,
    Result.
    

clone(Service,BoardNode)->
    Path=filename:join(?GITHUB,Service),
    Result=case rpc:call(BoardNode,os,cmd,["cp -r "++Path++" ."]) of
	       []->
		   ok;
	       {badrpc,nodedown}->
		   {error,nodedown};
	       Err->
		   {error,Err}
	   end,
    Result.

compile_erl(Src,Dest,BoardNode)->
  %  io:format("~p~n",[{?MODULE,?LINE,Src,Dest,BoardNode}]),
    Result=case rpc:call(BoardNode,file,list_dir,[Src]) of
	       {ok,Files}->
		   FilesToCompile=[filename:join(Src,File)||File<-Files,filename:extension(File)==".erl"],
		   case rpc:call(BoardNode,os,cmd,["rm  "++Dest++"/*"]) of
		       []->
			   CompileResult=[{rpc:call(BoardNode,c,c,[ErlFile,[{outdir,Dest}]],5000),ErlFile}||ErlFile<-FilesToCompile],
			   case [{R,File}||{R,File}<-CompileResult,error==R] of
			       []->
				   ok;
			       CompilerErrors->
				   {error,[compiler_error,CompilerErrors,?MODULE,?LINE]}
			   end;
		       {badrpc,nodedown}->
			   {error,nodedown};
		       Err->
			   {error,Err}
		   end;
	       {badrpc,nodedown}->
		   {error,nodedown};
	       Err->
		   {error,Err}
	   end,
    Result.
