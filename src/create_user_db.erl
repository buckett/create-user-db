-module(create_user_db).

-export([handle_req/1]).

-include_lib("couch/include/couch_db.hrl").

handle_req(#httpd{method='GET'}=Req) ->
    case Req#httpd.user_ctx#user_ctx.name of
        null ->
            couch_httpd:send_error(Req, {unauthorized, "You must be authenticated to create DB"});
	User ->
	    ?LOG_DEBUG("User ~s", [User]),
	    case ensure_user_db(User) of
	        {ok, Db} ->
		    try
                        ensure_security(User, Db)
                    after
                        couch_db:close(Db)
                    end,
		    couch_httpd:send_json(Req, {[{<<"db">>, Db#db.name}]});
		_default ->
		    couch_httpd:send_error(Req, {"Failed to create DB"})
            end
    end;

handle_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").

admin_ctx() ->
    {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

ensure_user_db(User) ->
    User_Db = user_db_name(User),
    case couch_db:open_int(User_Db, [admin_ctx(), nologifmissing]) of
        Ok={ok, _Db} ->
            Ok;
        _Err ->
            couch_db:create(User_Db, [admin_ctx()])
    end.

add_user(User, Prop, {Modified, SecProps}) ->
    {PropValue} = couch_util:get_value(Prop, SecProps, {[]}),
    Names = couch_util:get_value(<<"names">>, PropValue, []),
    case lists:member(User, Names) of
        true ->
            {Modified, SecProps};
        false ->
            {true,
             lists:keystore(
               Prop, 1, SecProps,
               {Prop,
                {lists:keystore(
                   <<"names">>, 1, PropValue,
                   {<<"names">>, [User | Names]})}})}
    end.

ensure_security(User, Db) ->
    {SecProps} = couch_db:get_security(Db),
    case lists:foldl(
           fun (Prop, SAcc) -> add_user(User, Prop, SAcc) end,
           {false, SecProps},
           [<<"admins">>, <<"members">>]) of
        {false, _} ->
            ok;
        {true, SecProps1} ->
            couch_db:set_security(Db, {SecProps1})
    end.

user_db_name(User) ->
    <<"userdb-", (iolist_to_binary(mochihex:to_hex(User)))/binary>>.


