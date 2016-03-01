-module(test_msg).
-compile(export_all).
-include("msg_pb.hrl").

-ifdef(test).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(df_attri, #attri_pb{index=100, value=1000}).
-define(df_attri_m, #{"index" => 100, "value" => 1000}).

-define(df_long, #long_pb{
    dbID = 100000000000, sceneID = 10001, sceneLine = 100,
    x = 100, y = 1000, z = 1000, 
    attriList = [?df_attri],
    name = "asdfsasdfds", career = 20, sex = true, signature = "asdfasdfasdfa", vip_level = 1, telphone = "1"
}).
-define(df_long_m, #{"dbID"=>100000000000, "sceneID"=>10001,"sceneLine"=>100, "x"=>100, "y"=>1000, "z"=>1000,
	"attriList"=>[?df_attri_m], "name"=>"asdfsasdfds", "career" => 20, "sex" => true, "signature" => "asdfasdfasdfa", "vip_level" => 1,
	"telphone" => "1"}).
	
-define(df_bigmessage, #bigmessage_pb{
    list = [?df_long,?df_long,?df_long,?df_long,?df_long,?df_long,?df_long,
            ?df_long,?df_long,?df_long,?df_long,?df_long,?df_long,?df_long]
}).

-define(df_bigmessage_m, #{
	"list"=> [?df_long_m,?df_long_m,?df_long_m,?df_long_m,?df_long_m,?df_long_m,?df_long_m,
            ?df_long_m,?df_long_m,?df_long_m,?df_long_m,?df_long_m,?df_long_m,?df_long_m]
}).

-define(df_nested_msg, #nested_msg_pb{data=#bigmessage_pb{list=[?df_long]}).
-define(df_nested_msg_m, #{"data" => #{"list" => [?df_long_m]}}).

-define(df_nested_msg1, #nested_msg1_pb{
	data1 = #nested_msg_pb{data=#bigmessage_pb{list=[?df_long]}},
	data2 = ?df_bigmessage
}).
-define(df_nested_msg1_m, #{"data1"=>?df_nested_msg_m, "data2" => ?df_bigmessage_m}).

init_pbc() ->
	%每个线程初始化一次
    SchedulerNum = erlang:system_info(schedulers),
    Pid = self(),
    F = fun() ->
        ok = pbc_nif:register("priv/msg.pb"),
        Pid ! ok
    end,
    spawn_opt(F, [{scheduler, 1}]),
    receive ok -> ok end,
    List = case SchedulerNum > 1 of true -> lists:seq(2, SchedulerNum); false -> [] end,
    [spawn_opt(F, [{scheduler, I}]) || I <- List],
    [begin receive _ -> ok end end || _ <- List].
	
-define(profile_encode_list, [
	{"encode_short_erlang",      fun() -> msg_pb:encode_attri_pb(?df_attri) end},
	{"encode_short_pbc_map",      fun() -> pbc_nif:encode("msg.attriMessage", ?df_attri_m) end},
	{"encode_short_pbc_record",   fun() -> pbc_nif:encode("msg.attriMessage", ?df_attri) end},
	
	{"encode_long_erlang",       fun() -> msg_pb:encode_long_pb(?df_long) end},
	{"encode_long_pbc_map",   fun() -> pbc_nif:encode("msg.longMessage", ?df_long_m) end},
	{"encode_long_pbc_record",   fun() -> pbc_nif:encode("msg.longMessage", ?df_long) end},
	
	{"encode_big_erlang",        fun() -> msg_pb:encode_bigmessage_pb(?df_bigmessage) end},
	{"encode_big_pbc_map",    fun() -> pbc_nif:encode("msg.bigmessageMessage", ?df_bigmessage_m) end},
	{"encode_big_pbc_record",    fun() -> pbc_nif:encode("msg.bigmessageMessage", ?df_bigmessage) end}
]).

-define(encode_short, fun() -> msg_pb:encode_attri_pb(?df_attri) end).
-define(encode_long, fun() -> msg_pb:encode_long_pb(?df_long) end).
-define(encode_big, fun() -> msg_pb:encode_bigmessage_pb(?df_bigmessage) end).
-define(profile_decode_list, [
	{"decode_short_erlang", ?encode_short, fun(Bin) -> msg_pb:decode_attri_pb(Bin) end},
	{"decode_short_pbc_map", ?encode_short, fun(Bin) -> pbc_nif:decode("msg.attriMessage", Bin, 1) end},
	{"decode_short_pbc_record", ?encode_short, fun(Bin) -> pbc_nif:decode("msg.attriMessage", Bin, 0) end},
	
	{"decode_long_erlang", ?encode_long, fun(Bin) -> msg_pb:decode_long_pb(Bin) end},
	{"decode_long_pbc_map", ?encode_long, fun(Bin) -> pbc_nif:decode("msg.longMessage", Bin, 1) end},
	{"decode_long_pbc_record", ?encode_long, fun(Bin) -> pbc_nif:decode("msg.longMessage", Bin, 0) end},
	
	{"decode_big_erlang", ?encode_big, fun(Bin) -> msg_pb:decode_bigmessage_pb(Bin) end},
	{"decode_big_pbc_map", ?encode_big, fun(Bin) -> pbc_nif:decode("msg.bigmessageMessage", Bin, 1) end},
	{"decode_big_pbc_record", ?encode_big, fun(Bin) -> pbc_nif:decode("msg.bigmessageMessage", Bin, 0) end}
]).

test() ->
	init_pbc(),
	Bin1 = pbc_nif:encode("msg.nested_msg1Message", ?df_nested_msg1_m),
	NestMap = pbc_nif:decode("msg.nested_msg1Message", Bin1, 1),
	NestMap = ?df_nested_msg1_m,
	Bin2 = pbc_nif:encode("msg.nested_msg1Message", ?df_nested_msg1),
	NestMap2 = pbc_nif:decode("msg.nested_msg1Message", Bin2, 1),
	NestMap2 = ?df_nested_msg1_m,
	
	NestRecord1 = msg_pb:decode_nested_msg1_pb(Bin1),
	NestRecord2 = msg_pb:decode_nested_msg1_pb(Bin2),
	NestRecord1 = ?df_nested_msg1,
	NestRecord2 = ?df_nested_msg1,
	ok.
	
test_multi_process() ->
	init_pbc(),
	Pid = self(),
	F = fun() ->
		case random:uniform(2) of
			1 ->
			    for(100000, fun() -> msg_pb:encode_long_pb(?df_long) end);
			2 ->
				EncodeFun = ?encode_long, TestBin = EncodeFun(),
				F1 = fun(Bin) -> pbc_nif:decode("msg.longMessage", Bin, 0) end,
				for(100000, fun() ->  F1(TestBin) end)
		end,
		Pid ! ok
	end,
	[spawn(F) || _ <- lists:seq(1, 100)],
	[begin receive ok -> ok end end || _ <- lists:seq(1, 100)].
	
profile() ->
	init_pbc(),
    profile_encode(),
    profile_decode(),
    ok.

profile(F, N, PreTips) ->
	{Time, _} = timer:tc(fun() -> for(N, F) end),
	io:format("~ts (~pw): ~pms~n", [PreTips, N / 10000, round(Time/1000)]).
	
for(0, _F) -> ok;
for(N, F) ->
	F(),
	for(N - 1, F).

profile_encode() ->
    io:format("~nbegin profile encode ~n"),
	F = fun({Tips, EncodeFun}) ->
		profile(EncodeFun, 100000, Tips)
	end,
	[F(Case) || Case <- ?profile_encode_list],
    ok.

profile_decode() ->
    io:format("~nbegin profile decode ~n"),
    F = fun({Tips, EncodeBin, DecodeFun}) ->
		Bin = EncodeBin(),
		profile(fun() -> DecodeFun(Bin) end, 100000, Tips)
	end,
	[F(Case) || Case <- ?profile_decode_list],
    ok.
