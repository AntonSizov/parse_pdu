-module(parse_pdu).

-export([
    main/1
]).
-export([parse/1]).

-include_lib("oserl/include/oserl.hrl").

-define(IEI_16_BIT_REFNUM_CONCAT, 8).

-define(log(Fmt, Args),
    (fun(F, A) ->
        io:format(F ++ "~n", A)
    end)(Fmt, Args)
).

main([Log]) ->
    Hex = strip_for_each_line(Log, length("2015-07-16 16:39:02.992 > [info] ")),
    parse(Hex);
main([_DoNotStrip, Log]) ->
    parse(Log);
main([]) ->
    HelpMsg =
    "1. To parse previous format hex with date preffix and \":\" delimiters:\n"
    "\tparse_pdu \"<HexWithDelimeters>\"\n"
    "2. To parse plain hex WITHOUT date preffix and \":\" delimiters:\n"
    "\tparse_pdu plain \"<Hex>\"\n",
    io:format(HelpMsg).

parse(Hex) ->
    ?log("Hex: ~p", [Hex]),
    BinPdu = decode_hex(Hex),
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
    Params1 = [{'short_message.hex', list_to_hex(ShortMsg)} | Params0],

    EsmClass = proplists:get_value(esm_class, Params1),
    HasUDH = (EsmClass band ?ESM_CLASS_GSM_UDHI) =:= ?ESM_CLASS_GSM_UDHI,
    case HasUDH of
        true ->
            {UDH, _Rest} = smpp_sm:chop_udh(ShortMsg),

            {_, ShortMsgWithoutUDH} = lists:split(length(UDH), ShortMsg),

            Params2 = Params1 ++
            [{'short_message.no_udh', ShortMsgWithoutUDH},
            {'short_message.no_udh.hex', list_to_hex(ShortMsgWithoutUDH)}],

            Params2 ++
            [{udh, UDH},
            {udhHex, list_to_hex(UDH)}  | parse_udh_to_params(UDH, [])];
        false -> Params1
    end;
get_short_message_and_udh_hex(_, Params) -> {Params, ""}.


parse_ie([?IEI_CONCAT|_] = Ie) ->
	[_, _, RefNum, TotalSegments, SeqNum] = Ie,
    [
		{'udh.concat8.sar_msg_ref_num',    RefNum},
    	{'udh.concat8.sar_total_segments', TotalSegments},
	    {'udh.concat8.sar_segment_seqnum', SeqNum}
	];
parse_ie([?IEI_16_BIT_REFNUM_CONCAT|_] = Ie) ->
	[_, _, RefNum1, RefNum2, TotalSegments, SeqNum] = Ie,
	<<RefNum:16>> = <<RefNum1:8, RefNum2:8>>,
    [
		{'udh.concat16.sar_msg_ref_num',    RefNum},
    	{'udh.concat16.sar_total_segments', TotalSegments},
	    {'udh.concat16.sar_segment_seqnum', SeqNum}
	];
parse_ie([?IEI_PORT_16|_] = Ie) ->
	[_, _, DestPort1, DestPort2, OrigPort1, OrigPort2] = Ie,
	<<DestPort:16>> = <<DestPort1:8, DestPort2:8>>,
	<<OrigPort:16>> = <<OrigPort1:8, OrigPort2:8>>,
	[
		{'udh.portAddr16.destination_port',	DestPort},
		{'udh.portAddr16.source_port',		OrigPort}
	].

parse_udh_to_params(Udh, ParamsList) ->
	[_|Ies] = Udh,
	Fun = fun(Ie, Params) -> parse_ie(Ie) ++ Params end,
	smpp_sm:ies_foldl(Fun, ParamsList, Ies).

%% ===================================================================
%% HEX
%% ===================================================================


decode_hex(HexBin) ->
  decode_hex(HexBin, []).
decode_hex([], Acc) ->
  list_to_binary(lists:reverse(Acc));
decode_hex([Hex1, Hex2 | Tail], Acc) ->
  Int = list_to_integer([Hex1, Hex2], 16),
  decode_hex(Tail, [Int | Acc]).

list_to_hex(ListOfInt) ->
    lists:flatten([integer_to_list(Int, 16) || Int <- ListOfInt]).
