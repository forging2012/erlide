-module(new_file).

-export([ok/0]).
-compile(export_all).

ok() ->   
 	ok.

foo() ->
	Z=[],   
	33, 
	[X || X<-Z, 
		  begin true 
		  end],
	"ok".
