%%%----------------------------------------------------------------------
%%% File    : mod_status_log_db.erl
%%% Author  : Aleksey Lavrov
%%% Purpose :
%%% Id      : 
%%%----------------------------------------------------------------------

-module(mod_status_log_db).
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
	?INFO_MSG("Start mod_status_log_db:Host: ~p~n Opts: ~p~n", [Host, Opts]),
	clear_db(Host),
	ejabberd_hooks:add(sm_register_connection_hook,		Host, ?MODULE, on_register_connection, 55),
	ejabberd_hooks:add(sm_remove_connection_hook,		Host, ?MODULE, on_remove_connection, 55),
	ejabberd_hooks:add(set_presence_hook,			Host, ?MODULE, set_presence_hook, 1),
	ok.

stop(Host) ->
	?INFO_MSG("Stop mod_status_log_db ~p", [Host]),
	ejabberd_hooks:delete(sm_register_connection_hook,	Host, ?MODULE, on_register_connection, 55),
	ejabberd_hooks:delete(sm_remove_connection_hook,	Host, ?MODULE, on_remove_connection, 55),
	ejabberd_hooks:delete(set_presence_hook,		Host, ?MODULE, set_presence_hook, 1),
	clear_db(Host),
	ok.

clear_db(Server) ->
	?INFO_MSG("mod_status_log_db:clear_db ~p", [Server]),
	catch ejabberd_odbc:sql_query(Server, [<<"INSERT INTO chat_status_list (user, status, showe, node, resource) VALUES \
	('ALL', 'Disconnected', NULL, '">>, atom_to_binary(node(), utf8), <<"', 'ALL')">>]),
	ok.

on_register_connection(_SID, _JID, _Info) ->
	?INFO_MSG("mod_status_log_db:SID: ~p", [_SID]),
	{{_C1,_C2,_C3},_C4} = _SID,
	_PID = list_to_binary( pid_to_list( _C4 ) ),
	{_A,User,Server,Resource,_D,_E,_F} = _JID,
	%%[{_B1,{_IP,_Port}},{_B4,_B5},{_B6,_B7}] = _Info,
	[{_B1,{{_IP1,_IP2,_IP3,_IP4},_Port}},{_B4,_B5},{_B6,_B7}] = _Info,
	_IP = list_to_binary([ integer_to_binary(_IP1), ".", integer_to_binary(_IP2), ".", integer_to_binary(_IP3), ".", integer_to_binary(_IP4), ":", integer_to_binary(_Port) ]),
	?INFO_MSG("mod_status_log_db Connect:~p", [User]),
	LServer =  jid:nameprep(Server),
	catch ejabberd_odbc:sql_query(LServer, [<<"INSERT INTO chat_status_list (user, status, showe, node, resource, ip, pid) VALUES \
	('">>, User, <<"', 'Connect', NULL, '">>, atom_to_binary(node(), utf8), <<"', '">>, Resource, <<"', '">>, _IP, <<"', '">>, _PID, <<"' )">>]),
	ok.

on_remove_connection(_SID, _JID, _Info) ->
	?INFO_MSG("mod_status_log_db:SID: ~p", [_SID]),
        {{_C1,_C2,_C3},_C4} = _SID,
	_PID = list_to_binary( pid_to_list( _C4 ) ),
	{_A,User,Server,Resource,_D,_E,_F} = _JID,
	[{_B1,{{_IP1,_IP2,_IP3,_IP4},_Port}},{_B4,_B5},{_B6,_B7}] = _Info,
	_IP = list_to_binary([ integer_to_binary(_IP1), ".", integer_to_binary(_IP2), ".", integer_to_binary(_IP3), ".", integer_to_binary(_IP4), ":", integer_to_binary(_Port) ]),
	?INFO_MSG("mod_status_log_db Disonnect:~p", [User]),
	LServer =  jid:nameprep(Server),
	catch ejabberd_odbc:sql_query(LServer, [<<"INSERT INTO chat_status_list (user, status, showe, node, resource, ip, pid) VALUES \
	('">>, User, <<"', 'Disconnected', NULL, '">>, atom_to_binary(node(), utf8), <<"', '">>, Resource, <<"', '">>, _IP, <<"', '">>, _PID, <<"' )">>]),
	ok.

set_presence_hook(User, Server, Resource, Presence) ->
%%	?INFO_MSG("mod_status_log_db:SID1: ~p", [_SID]),
	?INFO_MSG("mod_status_log_db Change Presence:User:~p~n Resource:~p~n Presence:~p~n", [User, Resource, Presence]),
	LServer = jid:nameprep(Server),
	case fxml:get_subtag(Presence, <<"show">>) of
		false ->
			LL = fxml:get_tag_attr_s(<<"show">>, Presence),
			catch ejabberd_odbc:sql_query(LServer, [<<"INSERT INTO chat_status_list (user, status, showe, node, resource) VALUES \
			('">>, User, <<"', 'Connect', '">>, LL, <<"', '">>, atom_to_binary(node(), utf8), <<"', '">>, Resource, <<"')">>]);
		_ ->
			LL = fxml:get_tag_cdata(fxml:get_subtag(Presence, <<"show">>)),
			catch ejabberd_odbc:sql_query(LServer, [<<"INSERT INTO chat_status_list (user, status, showe, node, resource) VALUES \
			('">>, User, <<"', 'Connect', '">>, LL, <<"', '">>, atom_to_binary(node(), utf8), <<"', '">>, Resource, <<"')">>])
	end,
	ok.



mod_opt_type(db_type) -> 
	fun (A) when is_atom(A) -> A end;
mod_opt_type(db_table) ->
        fun (B) when is_atom(B) -> B end;
mod_opt_type(_) ->
	[db_type, db_table].

