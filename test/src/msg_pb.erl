-module(msg_pb).

-export([encode_nested_msg1_pb/1,
	 decode_nested_msg1_pb/1, encode_nested_msg_pb/1,
	 decode_nested_msg_pb/1, encode_bigmessage_pb/1,
	 decode_bigmessage_pb/1, encode_long_pb/1,
	 decode_long_pb/1, encode_short_pb/1, decode_short_pb/1,
	 encode_attri_pb/1, decode_attri_pb/1]).

-record(attri_pb, {index, value}).

-record(short_pb, {not_used}).

-record(long_pb,
	{dbID, sceneID, sceneLine, x, y, z, attriList = [],
	 name, career, sex, signature, vip_level, telphone}).

-record(bigmessage_pb, {list = []}).

-record(nested_msg_pb, {data = #bigmessage_pb{}}).

-record(nested_msg1_pb,
	{data1 = #nested_msg_pb{}, data2 = #bigmessage_pb{}}).

encode_nested_msg1_pb(Record)
    when is_record(Record, nested_msg1_pb) ->
    encode(nested_msg1_pb, Record).

encode_nested_msg_pb(Record)
    when is_record(Record, nested_msg_pb) ->
    encode(nested_msg_pb, Record).

encode_bigmessage_pb(Record)
    when is_record(Record, bigmessage_pb) ->
    encode(bigmessage_pb, Record).

encode_long_pb(Record)
    when is_record(Record, long_pb) ->
    encode(long_pb, Record).

encode_short_pb(Record)
    when is_record(Record, short_pb) ->
    encode(short_pb, Record).

encode_attri_pb(Record)
    when is_record(Record, attri_pb) ->
    encode(attri_pb, Record).

encode(attri_pb, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#attri_pb.index, none), int32,
			   []),
		      pack(2, optional,
			   with_default(Record#attri_pb.value, none), int32,
			   [])]);
encode(short_pb, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#short_pb.not_used, none), int32,
			   [])]);
encode(long_pb, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#long_pb.dbID, none), int64, []),
		      pack(2, optional,
			   with_default(Record#long_pb.sceneID, none), int32,
			   []),
		      pack(3, optional,
			   with_default(Record#long_pb.sceneLine, none), int32,
			   []),
		      pack(4, optional, with_default(Record#long_pb.x, none),
			   int32, []),
		      pack(5, optional, with_default(Record#long_pb.y, none),
			   int32, []),
		      pack(6, optional, with_default(Record#long_pb.z, none),
			   int32, []),
		      pack(7, repeated,
			   with_default(Record#long_pb.attriList, none),
			   attri_pb, []),
		      pack(8, optional,
			   with_default(Record#long_pb.name, none), string, []),
		      pack(9, optional,
			   with_default(Record#long_pb.career, none), int32,
			   []),
		      pack(10, optional,
			   with_default(Record#long_pb.sex, none), bool, []),
		      pack(11, optional,
			   with_default(Record#long_pb.signature, none), string,
			   []),
		      pack(12, optional,
			   with_default(Record#long_pb.vip_level, none), int32,
			   []),
		      pack(13, optional,
			   with_default(Record#long_pb.telphone, none), string,
			   [])]);
encode(bigmessage_pb, Record) ->
    iolist_to_binary([pack(1, repeated,
			   with_default(Record#bigmessage_pb.list, none),
			   long_pb, [])]);
encode(nested_msg_pb, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#nested_msg_pb.data, none),
			   bigmessage_pb, [])]);
encode(nested_msg1_pb, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#nested_msg1_pb.data1, none),
			   nested_msg_pb, []),
		      pack(2, optional,
			   with_default(Record#nested_msg1_pb.data2, none),
			   bigmessage_pb, [])]).

with_default(undefined, none) -> undefined;
with_default(undefined, Default) -> Default;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _) ->
    protobuffs:encode(FNum, Data, Type).

decode_nested_msg1_pb(Bytes) when is_binary(Bytes) ->
    decode(nested_msg1_pb, Bytes).

decode_nested_msg_pb(Bytes) when is_binary(Bytes) ->
    decode(nested_msg_pb, Bytes).

decode_bigmessage_pb(Bytes) when is_binary(Bytes) ->
    decode(bigmessage_pb, Bytes).

decode_long_pb(Bytes) when is_binary(Bytes) ->
    decode(long_pb, Bytes).

decode_short_pb(Bytes) when is_binary(Bytes) ->
    decode(short_pb, Bytes).

decode_attri_pb(Bytes) when is_binary(Bytes) ->
    decode(attri_pb, Bytes).

decode(attri_pb, Bytes) when is_binary(Bytes) ->
    Types = [{2, value, int32, []}, {1, index, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(attri_pb, Decoded);
decode(short_pb, Bytes) when is_binary(Bytes) ->
    Types = [{1, not_used, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(short_pb, Decoded);
decode(long_pb, Bytes) when is_binary(Bytes) ->
    Types = [{13, telphone, string, []},
	     {12, vip_level, int32, []}, {11, signature, string, []},
	     {10, sex, bool, []}, {9, career, int32, []},
	     {8, name, string, []},
	     {7, attriList, attri_pb, [is_record, repeated]},
	     {6, z, int32, []}, {5, y, int32, []}, {4, x, int32, []},
	     {3, sceneLine, int32, []}, {2, sceneID, int32, []},
	     {1, dbID, int64, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(long_pb, Decoded);
decode(bigmessage_pb, Bytes) when is_binary(Bytes) ->
    Types = [{1, list, long_pb, [is_record, repeated]}],
    Decoded = decode(Bytes, Types, []),
    to_record(bigmessage_pb, Decoded);
decode(nested_msg_pb, Bytes) when is_binary(Bytes) ->
    Types = [{1, data, bigmessage_pb, [is_record]}],
    Decoded = decode(Bytes, Types, []),
    to_record(nested_msg_pb, Decoded);
decode(nested_msg1_pb, Bytes) when is_binary(Bytes) ->
    Types = [{2, data2, bigmessage_pb, [is_record]},
	     {1, data1, nested_msg_pb, [is_record]}],
    Decoded = decode(Bytes, Types, []),
    to_record(nested_msg1_pb, Decoded).

decode(<<>>, _, Acc) -> Acc;
decode(Bytes, Types, Acc) ->
    {{FNum, WireType}, Rest} =
	protobuffs:read_field_num_and_wire_type(Bytes),
    case lists:keysearch(FNum, 1, Types) of
      {value, {FNum, Name, Type, Opts}} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {V, R} = protobuffs:decode_value(Rest,
								   WireType,
								   bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  {V, R} = protobuffs:decode_value(Rest,
								   WireType,
								   Type),
				  {unpack_value(V, Type), R}
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name,
			       lists:reverse([Value1 | lists:reverse(List)])}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types, [{FNum, Name, [Value1]} | Acc])
		end;
	    false ->
		decode(Rest1, Types, [{FNum, Name, Value1} | Acc])
	  end;
      false -> exit({error, {unexpected_field_index, FNum}})
    end.

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(attri_pb, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, attri_pb), Record,
					 Name, Val)
		end,
		#attri_pb{}, DecodedTuples);
to_record(short_pb, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, short_pb), Record,
					 Name, Val)
		end,
		#short_pb{}, DecodedTuples);
to_record(long_pb, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, long_pb), Record,
					 Name, Val)
		end,
		#long_pb{}, DecodedTuples);
to_record(bigmessage_pb, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, bigmessage_pb),
					 Record, Name, Val)
		end,
		#bigmessage_pb{}, DecodedTuples);
to_record(nested_msg_pb, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, nested_msg_pb),
					 Record, Name, Val)
		end,
		#nested_msg_pb{}, DecodedTuples);
to_record(nested_msg1_pb, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, nested_msg1_pb),
					 Record, Name, Val)
		end,
		#nested_msg1_pb{}, DecodedTuples).

set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> 0.

