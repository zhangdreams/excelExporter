%%coding: latin-1 
-module(cfg_vip).
-export([get_value/1, get_value/2, get/1, all/0]). 

get_value(Key) -> 
	?MODULE:get(Key).

get_value(Key,Default) -> 
	case ?MODULE:get(Key) of 
		 undefined -> Default;
		Value -> Value
	end.

get({1,11}) ->
	?get_1_11;
get({2,12}) ->
	?get_2_12;
get({3,13}) ->
	{vip_def,3,13,1600,[1,3,4,6]};
get({4,14}) ->
	{vip_def,4,14,2200,[{1,3},{2,4}]};
get({1,12}) ->
	?get_1_12;
get(_) -> 
	undefined.

-ifdef(LANG_KR).
	-define(get_1_12,{vip_def,1,12,301,[{1,2}]}).
	-define(get_1_11, undefined).
	-define(get_2_12, undefined).
-else.
-ifdef(LANG_TW).
	-define(get_1_12, undefined).
	-define(get_1_11,{vip_def,1,11,62,[1,2]}).
	-define(get_2_12,{vip_def,2,12,302,[{1,2}]}).
-else.
-ifdef(LANG_TH).
	-define(get_2_12, undefined).
	-define(get_1_12, undefined).
	-define(get_1_11, undefined).
-else.
	-define(get_1_11,{vip_def,1,11,60,[1,2]}).
	-define(get_2_12,{vip_def,2,12,300,[{1,2}]}).
	-define(get_1_12, undefined).

-endif.
-endif.
-endif.

all() -> 
	[
		?get_1_11,
		?get_2_12,
		{vip_def,3,13,1600,[1,3,4,6]},
		{vip_def,4,14,2200,[{1,3},{2,4}]},
		?get_1_12
	].