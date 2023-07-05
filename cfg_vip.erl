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

get(0) ->
	?get_0;
get(1) ->
	?get_1;
get(2) ->
	{vip_def,2,13,1600,[1,3]};
get(3) ->
	{vip_def,3,14,2200,[{1,3}]};
get(_) -> 
	undefined.

-ifdef(LANG_KR).
	-define(get_1,{vip_def,1,12,301,[{1,2}]}).
	-define(get_0, undefined).
-else.
-ifdef(LANG_TW).
	-define(get_1, undefined).
	-define(get_0,{vip_def,0,11,62,[1,2]}).
-else.
-ifdef(LANG_TH).
	-define(get_1, undefined).
	-define(get_0, undefined).
-else.
	-define(get_1,{vip_def,1,12,300,[{1,2}]}).
	-define(get_0,{vip_def,0,11,60,[1,2]}).

-endif.
-endif.
-endif.

all() -> 
	[
		?get_0,
		?get_1,
		{vip_def,2,13,1600,[1,3]},
		{vip_def,3,14,2200,[{1,3}]}
	].