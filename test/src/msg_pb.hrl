-record(attri_pb, {index, value}).

-record(short_pb, {not_used}).

-record(long_pb,
	{dbID, sceneID, sceneLine, x, y, z, attriList = [],
	 name, career, sex, signature, vip_level, telphone}).

-record(bigmessage_pb, {list = []}).

-record(nested_msg_pb, {data = #bigmessage_pb{}}).

-record(nested_msg1_pb,
	{data1 = #nested_msg_pb{}, data2 = #bigmessage_pb{}}).