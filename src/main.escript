#!/usr/bin/env escript

main(_) ->
  readAQL().

readAQL() ->
  io:fwrite("Welcome to the AQL Shell.~n"),
	Line = io:get_line("AQL>"),
	Result = aqlparser:parse({str, Line}),
  readAQL().
