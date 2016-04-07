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
	?INFO_MSG("Start mod_status_db:Host: ~p~n Opts: ~p~n", [Host, Opts]),
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
	catch ejabberd_odbc:sql_query(Server, [<<"UPDATE chatoperator SET status='Disconnected', showe=NULL, Resource=NULL WHERE node='">>, atom_to_binary(node(), utf8), <<"'">>]),
	ok.

on_register_connection(_SID, _JID, _Info) ->
	{_A,User,Server,_C,_D,_E,_F} = _JID,
	?INFO_MSG("mod_status_db Connect:~p", [User]),
	LServer =  jid:nameprep(Server),
	catch ejabberd_odbc:sql_query(LServer, [<<"UPDATE chatoperator SET status='Connect', node='">>, atom_to_binary(node(), utf8), <<"' WHERE vclogin = '">>, User, <<"'">>]),
	ok.

on_remove_connection(_SID, _JID, _SessionInfo) ->
	{_A,User,Server,_C,_D,_E,_F} = _JID,
	?INFO_MSG("mod_status_db Disonnect:~p", [User]),
	LServer =  jid:nameprep(Server),
	catch ejabberd_odbc:sql_query(LServer, [<<"UPDATE chatoperator SET status='Disconnected', showe=NULL, Resource=NULL, node=NULL WHERE vclogin = '">>, User, <<"'">>]),
	ok.

set_presence_hook(User, Server, Resource, Presence) ->
	?INFO_MSG("mod_status_db Change Presence: ~p~n Presence: ~p~n", [User,Presence]),
	LServer = jid:nameprep(Server),
	case fxml:get_subtag(Presence, <<"show">>) of
		false ->
			LL = fxml:get_tag_attr_s(<<"show">>, Presence),
			catch ejabberd_odbc:sql_query(LServer, [<<"UPDATE chatoperator SET showe='">>, LL, <<"', Resource='">>, Resource, <<"', node='">>, atom_to_binary(node(), utf8), <<"' WHERE vclogin = '">>, User, <<"'">>]);
		_ ->
			LL = fxml:get_tag_cdata(fxml:get_subtag(Presence, <<"show">>)),
			catch ejabberd_odbc:sql_query(LServer, [<<"UPDATE chatoperator SET showe='">>, LL, <<"', Resource='">>, Resource, <<"', node='">>, atom_to_binary(node(), utf8), <<"' WHERE vclogin = '">>, User, <<"'">>])
	end,
	ok.

mod_opt_type(db_type) ->
	fun (A) when is_atom(A) -> A end;
mod_opt_type(_) ->
	[db_type].

