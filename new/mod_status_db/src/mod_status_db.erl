%%%----------------------------------------------------------------------
%%% File    : mod_status_db.erl
%%% Author  : Aleksey Lavrov
%%% Purpose :
%%% Id      : 
%%%----------------------------------------------------------------------

-module(mod_status_db).
-author('LavrovAP@gmail.com').

-behaviour(gen_mod).

-export([
        start/2,
	stop/1,
	clear_db/1,
	on_register_connection/3,
	on_remove_connection/3,
	set_presence_hook/4,
	mod_opt_type/1
	]).

-ifndef(LAGER).
-define(LAGER, 1).
-endif.

-include("ejabberd.hrl").
-include("logger.hrl").

start(Host, Opts) ->
	?INFO_MSG("Start mod_status_db:Host: ~p Opts: ~p", [Host, Opts]),
	clear_db(Host),
	ejabberd_hooks:add(sm_register_connection_hook,		Host, ?MODULE, on_register_connection, 55),
	ejabberd_hooks:add(sm_remove_connection_hook,		Host, ?MODULE, on_remove_connection, 55),
	ejabberd_hooks:add(set_presence_hook,			Host, ?MODULE, set_presence_hook, 1),
	ok.

stop(Host) ->
	?INFO_MSG("Stop mod_status_db ~p", [Host]),
	ejabberd_hooks:delete(sm_register_connection_hook,	Host, ?MODULE, on_register_connection, 55),
	ejabberd_hooks:delete(sm_remove_connection_hook,	Host, ?MODULE, on_remove_connection, 55),
	ejabberd_hooks:delete(set_presence_hook,		Host, ?MODULE, set_presence_hook, 1),
	clear_db(Host),
	ok.

clear_db(Server) ->
	?INFO_MSG("mod_status_db:clear_db ~p", [Server]),
	catch ejabberd_sql:sql_query(Server, [<<"UPDATE chatoperator SET status='Disconnected', showe=NULL, Resource=NULL, pid=NULL \
	WHERE node='">>, atom_to_binary(node(), utf8), <<"'">>]),
	ok.

on_register_connection(_SID, _JID, _Info) ->
	{_A,User,Server,_C,_D,_E,_F} = _JID,
        {{_C1,_C2,_C3},_C4} = _SID,
        _PID = list_to_binary( pid_to_list( _C4 ) ),
	?INFO_MSG("mod_status_db Connect:~p", [User]),
	LServer =  jid:nameprep(Server),
	catch ejabberd_sql:sql_query(LServer, [<<"UPDATE chatoperator SET status='Connect', node='">>, atom_to_binary(node(), utf8), <<"', pid='">>, _PID, <<"' \
	WHERE vclogin = '">>, User, <<"'">>]),
	ok.

on_remove_connection(_SID, _JID, _SessionInfo) ->
	{_A,User,Server,_C,_D,_E,_F} = _JID,
        {{_C1,_C2,_C3},_C4} = _SID,
        _PID = list_to_binary( pid_to_list( _C4 ) ),
	?INFO_MSG("mod_status_db Disonnect:~p", [User]),
	LServer =  jid:nameprep(Server),
	catch ejabberd_sql:sql_query(LServer, [<<"UPDATE chatoperator SET status='Disconnected', showe=NULL, Resource=NULL, node=NULL, pid=NULL \
	WHERE ( vclogin = '">>, User, <<"' and pid = '">>, _PID, <<"' )">>]),
	ok.

set_presence_hook(User, Server, Resource, Presence) ->
%%	?INFO_MSG("mod_status_db Change Presence: ~p Presence: ~p", [User,Presence]),
%%	LServer = jid:nameprep(Server),
        LUser = jlib:nodeprep(User),
        LServer = jlib:nodeprep(Server),
        _C4 = ejabberd_sm:get_session_pid(LUser, LServer, Resource),
        _PID = list_to_binary( pid_to_list( _C4 ) ),
        ?INFO_MSG("mod_status_db Change Presence:User:~p Resource:~p Presence:~p Pid:~p", [User, Resource, Presence, _PID]),

	case fxml:get_subtag(Presence, <<"show">>) of
		false ->
			LL = fxml:get_tag_attr_s(<<"show">>, Presence),
			catch ejabberd_sql:sql_query(LServer, [<<"UPDATE chatoperator SET showe='">>, LL, <<"', Resource='">>, Resource, <<"', node='">>, atom_to_binary(node(), utf8), <<"' WHERE vclogin = '">>, User, <<"'">>]);
			%%catch ejabberd_sql:sql_query(LServer, [<<"UPDATE chatoperator SET showe='">>, LL, <<"', Resource='">>, Resource, <<"', node='">>, atom_to_binary(node(), utf8), <<"' WHERE pid = '">>, _PID, <<"'">>]);
		_ ->
			LL = fxml:get_tag_cdata(fxml:get_subtag(Presence, <<"show">>)),
			catch ejabberd_sql:sql_query(LServer, [<<"UPDATE chatoperator SET showe='">>, LL, <<"', Resource='">>, Resource, <<"', node='">>, atom_to_binary(node(), utf8), <<"' WHERE vclogin = '">>, User, <<"'">>])
			%%catch ejabberd_sql:sql_query(LServer, [<<"UPDATE chatoperator SET showe='">>, LL, <<"', Resource='">>, Resource, <<"', node='">>, atom_to_binary(node(), utf8), <<"' WHERE pid = '">>, _PID, <<"'">>])
	end,
	ok.

mod_opt_type(db_type) ->
	fun (A) when is_atom(A) -> A end;
mod_opt_type(_) ->
	[db_type].

