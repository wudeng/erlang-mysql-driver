-module(mysql_api).

-export([
    fetch/1,
    fetch/2,
    transaction/1,
    transaction/2
]).

-define(POOL, pool).
-define(TIMEOUT, 5000).

fetch(Query) ->
    fetch(Query, ?TIMEOUT).

fetch(Query, Timeout) ->
    case mysql:fetch(?POOL, Query, Timeout) of
        {data, MysqlRes} -> 
            mysql:get_result_rows(MysqlRes);
        {updated, MysqlRes} -> 
            mysql:get_result_affected_rows(MysqlRes);
        {error, MysqlRes} ->  
            Reason = mysql:get_result_reason(MysqlRes),
            ErrCode = mysql:get_result_err_code(MysqlRes),
            ErrSqlState = mysql:get_result_err_sql_state(MysqlRes),
            erlang:error({db_error, ErrCode, ErrSqlState, Reason})
    end.

transaction(Fun) ->
    transaction(Fun, ?TIMEOUT).

transaction(Fun, Timeout) ->
    case mysql:transaction(?POOL, Fun, Timeout) of
      {atomic, Result} -> 
          {ok, Result};
      {aborted, {Reason, {rollback_result, _Result}}} ->
          {error, Reason}
    end.
