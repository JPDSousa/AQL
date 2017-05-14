#!/usr/bin/env escript

main(_) ->
  readAQL().

readAQL() ->
	Line = io:get_line("AQL>"),
	Result = aqlparser:parse({str, Line}),
  io:fwrite().%incomplete
