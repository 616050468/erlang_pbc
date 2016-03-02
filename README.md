## erlang_pbc

erlang_pbc is erlang nif of protobuff-c which is developed by cloudwu, want to know protobuff-c more,
please see: https://github.com/cloudwu/pbc 

erlang_pbc is faster than erlang-protobuff, support record and map.
if you use map, you don't need any erlang code.

## Quick Example

	package msg;

	message attriMessage {
		optional int32 index = 1;
		optional int32 value = 2;
	}

	message longMessage {
		optional int64 dbID = 1;
		optional int32 sceneID = 2;
		optional int32 sceneLine = 3;
		optional int32 x = 4;
		optional int32 y = 5;
		optional int32 z = 6;
		repeated attriMessage attriList = 7;
		optional string name = 8;
		optional int32 career = 9;
		optional bool sex = 10;
		optional string signature = 11;
		optional int32 vip_level = 12;
		optional string telphone = 13;
	}

```C
Eshell V6.1  (abort with ^G)
1> test_msg:init_pbc().
[ok,ok,ok,ok,ok,ok,ok]
2> pbc_nif:get_default("msg.attriMessage").
{{'msg.attriMessage',0,0},#{"index" => 0,"value" => 0}}
3>
3> Bin = pbc_nif:encode("msg.attriMessage", {'msg.attriMessage', 100, 100}).
<<8,100,16,100>>
4>
4> pbc_nif:decode("msg.attriMessage", Bin, 0).
{'msg.attriMessage',100,100}
5>
5> pbc_nif:decode("msg.attriMessage", Bin, 1).
#{"index" => 100,"value" => 100}
```

## Profile

please see test_msg.erl to know more....

test data in personal note-book:
Intel(R) Xeon(R) CPU E3-1231 v3 @3.40GHz
RAM:  8.00GB
OS TYPE: 64-bit

```C
Eshell V6.1  (abort with ^G)
1> test_msg:profile().

begin profile encode 
encode_short_erlang (10.0w): 439ms
encode_short_pbc_map (10.0w): 58ms
encode_short_pbc_record (10.0w): 52ms
encode_long_erlang (10.0w): 850ms
encode_long_pbc_map (10.0w): 187ms
encode_long_pbc_record (10.0w): 152ms
encode_big_erlang (10.0w): 13084ms
encode_big_pbc_map (10.0w): 2203ms
encode_big_pbc_record (10.0w): 1739ms

begin profile decode 
decode_short_erlang (10.0w): 164ms
decode_short_pbc_map (10.0w): 76ms
decode_short_pbc_record (10.0w): 60ms
decode_long_erlang (10.0w): 1342ms
decode_long_pbc_map (10.0w): 424ms
decode_long_pbc_record (10.0w): 175ms
decode_big_erlang (10.0w): 20312ms
decode_big_pbc_map (10.0w): 6099ms
decode_big_pbc_record (10.0w): 2556ms
ok
```


## compile

Windows:
open project use VS and build the project.

Linux: (use premake4 to create makefile)
> premake4 gmake

