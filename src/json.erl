-module(json).
-export([encode/1, decode/1, get_value/2, get_value/3]).

encode(PropList) ->
    jiffy:encode({PropList}).

decode(Json) ->
    unpack_object(jiffy:decode(Json)).

get_value(Key, DecodedResult) ->
    get_value(Key, DecodedResult, undefined).
get_value(Key, DecodedResult, Default) ->
    Value = proplists:get_value(Key, DecodedResult, Default),
    % jiffy packs object proplists in a one-element tuple,
    % so reverse that operation for decoded values
    case Value of
        List when is_list(List) ->
            lists:map(fun unpack_object/1, List);
        _ ->
            unpack_object(Value)
    end.

unpack_object({PropList}) when is_list(PropList) ->
    PropList;
unpack_object(Value) ->
    Value.
