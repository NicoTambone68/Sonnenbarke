-module(sbcli).

-export([
	 % Client functions
	 connect/0,

	 % Interface 1
	 new/1,
	 in/2,
	 rd/2,
	 out/2,

	 % Interface 2
	 rd/3
       ]).

% TO DO: check cluster status
% ok when connect is successf.
connect() -> {ok}.

% Interface 1

new(TS) ->
   try
      Ret = sb:command({new, TS}),
      Ret
   catch Error:Reason ->
      case Reason of
         {ts_name_already_exists} -> {err, Reason};
                                _ -> {Error, Reason}
      end
   end.

in(TS, Pattern) ->
   {_, Ret} =  sb:command({in, TS, Pattern}),
   case Ret of
     no_match -> sbts:halt();
           _ -> ok
   end,
   Ret.

rd(TS, Pattern) ->
   {_, Ret} =  sb:command({rd, TS, Pattern}),
   case Ret of
     no_match -> sbts:halt();
           _ -> ok
   end,
   Ret.


out(TS, Tuple) ->
   sb:command({out, TS, Tuple}).

% Interface 2

rd(TS, Pattern, Timeout) ->
   try	
      {_, Ret} =  sb:command({rd, TS, Pattern}),
      case Ret of
        no_match -> sbts:halt(Timeout);
               _ -> ok
      end,
   Ret
   catch Error:Reason ->
      case Reason of 
         {timeout, _} -> {err, timeout};
	           _  -> {Error, Reason}
      end
   end.

