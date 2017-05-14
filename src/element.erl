
-module(element).

-define(CRDT_TYPE, antidote_crdt_gmap).

-export([new/2]).

%% ====================================================================
%% API functions
%% ====================================================================

new(Key, Bucket) when is_atom(Key) and is_atom(Bucket) ->
  objects:create_bound_object(Key, ?CRDT_TYPE, Bucket);
new(Key, Bucket) when is_list(Key) and is_atom(Bucket) ->
  new(list_to_atom(Key), Bucket);
new(Key, Bucket) when is_integer(Key) and is_atom(Bucket) ->
  new(integer_to_list(Key), Bucket).
