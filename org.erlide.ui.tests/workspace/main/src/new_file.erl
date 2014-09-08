-module(new_file).

-export([ok/0]).
-compile(export_all).

-include("inc.hrl").

-include_lib("kernel/include/file.hrl").
 
ok() ->  
 	ok.

foo() ->
	Z=[],   
	33, 
	[X || X<-Z, 
		  begin true 
		  end],
	"ok".

zz() -> ok.
