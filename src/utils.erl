%% @author Joao
%% @doc @todo Add description to utils.


-module(utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([write_terms/2, effective_filename/1]).

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text),
	ok.

effective_filename(Filename) ->
	Path = tables:path(data),
	string:concat(Path, Filename).

%% ====================================================================
%% Internal functions
%% ====================================================================


