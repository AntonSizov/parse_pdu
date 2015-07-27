-module(parse_pdu).

-export([
    main/1
]).
-export([parse/1]).

-include_lib("oserl/include/oserl.hrl").

-define(log(Fmt, Args),
    (fun(F, A) ->
        io:format(F ++ "~n", A)
    end)(Fmt, Args)
).

main([Log]) ->
    parse(Log).

parse(Log) ->
    Hex = strip_for_each_line(Log, length("2015-07-16 16:39:02.992 > [info] ")),
    ?log("Hex: ~p", [Hex]),
    BinPdu = decode_hex(list_to_binary(Hex)),
    {ok, {CmdId, _Status, _SeqNum, _Body} = Pdu} = smpp_operation:unpack(BinPdu),
    Params0 = smpp_operation:to_list(Pdu),
    Params = get_short_message_and_udh_hex(CmdId, Params0),

    ?log("Params: ~p", [Params]),
    BinPdu.

strip_for_each_line(Input0, N) ->
    Input = list_to_binary(Input0),
    Lines = binary:split(Input, <<"\n">>, [global]),
    Strip = fun(Line) ->
        L0 = lists:sublist(binary_to_list(Line), N+1, 50),
        binary_to_list(list_to_binary(binary:split(list_to_binary(L0), <<":">>, [global])))
    end,
    StrippedLines = lists:map(Strip, Lines),
    Flatten = lists:flatten(StrippedLines),
    Flatten.

get_short_message_and_udh_hex(CmdId, Params0) when
        CmdId =:= ?COMMAND_ID_SUBMIT_SM orelse CmdId =:= ?COMMAND_ID_DELIVER_SM ->

   {value, {short_message, ShortMsg}, _} =
       lists:keytake(short_message, 1, Params0),

    EsmClass = proplists:get_value(esm_class, Params0),
    HasUDH = (EsmClass band ?ESM_CLASS_GSM_UDHI) =:= ?ESM_CLASS_GSM_UDHI,
    Params =
    case HasUDH of
        true ->
            {UDH, _Rest} = smpp_sm:chop_udh(ShortMsg),
            [{udh, UDH} | Params0];
        false -> Params0
    end,

    Params;
get_short_message_and_udh_hex(_, Params) -> {Params, ""}.


%% ===================================================================
%% HEX
%% ===================================================================

decode_hex(HexBin) ->
  decode_hex(HexBin, <<>>).
decode_hex(<<>>, Acc) ->
  Acc;
decode_hex(<<M, L, T/binary>>, Acc) ->
  Byte = hexlist_to_integer([M, L]),
  decode_hex(T, <<Acc/binary, Byte>>).


hexlist_to_integer([M, L]) ->
  hexdigit_to_integer(M) * 16 + hexdigit_to_integer(L).


hexdigit_to_integer(D) when D =< $9 ->
  D - $0;

hexdigit_to_integer(D) when D =< $F ->
  D - $A + 10;

hexdigit_to_integer(D) ->
  D - $a + 10.
