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
	{vip_def,0,60,[1,2]};
get(1) ->
	{vip_def,1,300,[{1,2}]};
get(2) ->
	{vip_def,2,600,[1,3]};
get(3) ->
	{vip_def,3,1200,[{1,3}]};
get(_) -> 
	undefined.

all() -> 
	[
		{vip_def,0,60,[1,2]},
		{vip_def,1,300,[{1,2}]},
		{vip_def,2,600,[1,3]},
		{vip_def,3,1200,[{1,3}]}
	].