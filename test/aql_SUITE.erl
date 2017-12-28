%%%-------------------------------------------------------------------
%%% @author joao
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. ago 2017 14:18
%%%-------------------------------------------------------------------
-module(aql_SUITE).
-author("joao").

-include_lib("parser.hrl").
-include_lib("aql.hrl").
-include_lib("types.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ct_aql.hrl").

-export([init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2,
  all/0]).

%% API
-export([select_all/1,
        insert_artist/1,
        insert_with_default_artist/1,
        insert_duplicate/1,
        delete_album/1,
        delete_non_existent_album/1]).

init_per_suite(Config) ->
  TNameArtist = "ArtistTest",
  TNameAlbum = "AlbumTest",
  TNameTrack = "TrackTest",
  DefaultArtist = 0,
  DefaultAlbum = false,
  {ok, []} = tutils:aql(lists:concat(["CREATE @AW TABLE ", TNameArtist,
    " (Name VARCHAR PRIMARY KEY, City VARCHAR,",
    " Awards INTEGER DEFAULT ", DefaultArtist, ");"])),
  {ok, []} = tutils:aql(lists:concat(["CREATE @AW TABLE ", TNameAlbum,
    " (Name VARCHAR PRIMARY KEY,",
    " IsSingle BOOLEAN DEFAULT ", DefaultAlbum, ");"])),
  {ok, []} = tutils:aql(lists:concat(["CREATE @AW TABLE ", TNameTrack,
    " (Name VARCHAR PRIMARY KEY, Plays COUNTER_INT CHECK GREATER 0);"
  ])),
  lists:append(Config, [
    {tname_artist, TNameArtist},
    {tname_album, TNameAlbum},
    {tname_track, TNameTrack},
    {default_artist, DefaultArtist},
    {default_album, DefaultAlbum},
    {insert_artist, lists:concat(["INSERT INTO ", TNameArtist, " VALUES ('~s', '~s', ~p);"])},
    {insert_artist_def, lists:concat(["INSERT INTO ", TNameArtist, " VALUES ('~s', '~s');"])},
    {insert_album, lists:concat(["INSERT INTO ", TNameAlbum, " VALUES ('~s', ~p);"])},
    {insert_track, lists:concat(["INSERT INTO ", TNameTrack, " VALUES ('~s', ~p);"])},
    {update_artist, lists:concat(["UPDATE ", TNameArtist, " SET ~s WHERE Name = '~s';"])},
    {update_album, lists:concat(["UPDATE ", TNameAlbum, " SET ~s WHERE Name = '~s';"])},
    {update_track, lists:concat(["UPDATE ", TNameTrack, " SET ~s WHERE Name = '~s';"])},
    {delete_artist, lists:concat(["DELETE FROM ", TNameArtist, " WHERE Name = '~s';"])},
    {delete_album, lists:concat(["DELETE FROM ", TNameAlbum, " WHERE Name = '~s';"])},
    {delete_track, lists:concat(["DELETE FROM ", TNameTrack, " WHERE Name = '~s';"])},
    {select_artist, lists:concat(["SELECT * FROM ", TNameArtist, " WHERE Name = '~s';"])},
    {select_album, lists:concat(["SELECT * FROM ", TNameAlbum, " WHERE Name = '~s';"])},
    {select_track, lists:concat(["SELECT * FROM ", TNameTrack, " WHERE Name = '~s';"])}
  ]).

end_per_suite(Config) ->
  Config.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_, _) ->
  ok.

all() ->
  [
    select_all,
    insert_artist,
    insert_with_default_artist,
    insert_duplicate,
    delete_album,
    delete_non_existent_album
  ].

select_all(_Config) ->
  TNameEmpty = "EmptyTableTest",
  TNameFull = "FullTableTest",
  {ok, []} = tutils:create_single_table(TNameEmpty),
  {ok, []} = tutils:create_single_table(TNameFull),
  {ok, [[]]} = tutils:select_all(TNameEmpty),
  {ok, []} = tutils:insert_single(TNameFull, 1),
  {ok, []} = tutils:insert_single(TNameFull, 2),
  {ok, []} = tutils:insert_single(TNameFull, 3),
  {ok, [Res]} = tutils:select_all(TNameFull),
  ?assertEqual(3, length(Res)).

insert_artist(Config) ->
  TName = ?value(tname_artist, Config),
  Artist = "Sam",
  City = "NY",
  Awards = 1,
  {ok, []} = tutils:aql(?format(insert_artist, [Artist, City, Awards], Config)),
  SearchKey = lists:concat(["'", Artist, "'"]),
  [Artist, City, Awards] = tutils:read_keys(TName, "Name", SearchKey, ["Name", "City", "Awards"]).

insert_with_default_artist(Config) ->
  TName = ?value(tname_artist, Config),
  Artist = "Mike",
  City = "LS",
  Awards = ?value(default_artist, Config),
  {ok, []} = tutils:aql(?format(insert_artist_def, [Artist, City], Config)),
  SearchKey = lists:concat(["'", Artist, "'"]),
  [Artist, City, Awards] = tutils:read_keys(TName, "Name", SearchKey, ["Name", "City", "Awards"]).

insert_duplicate(Config) ->
  TName = ?value(tname_artist, Config),
  Artist = "Sam",
  City = "NY",
  Awards = 1,
  Query = ?format(insert_artist, [Artist, City, Awards], Config),
  {ok, []} = tutils:aql(lists:concat([Query, Query])),
  SearchKey = lists:concat(["'", Artist, "'"]),
  [Artist, City, Awards] = tutils:read_keys(TName, "Name", SearchKey, ["Name", "City", "Awards"]).

delete_album(Config) ->
  TName = ?value(tname_album, Config),
  Album = "Hello",
  {ok, []} = tutils:aql(?format(insert_album, [Album, true], Config)),
  {ok, []} = tutils:aql(?format(delete_album, [Album], Config)),
  tutils:assertState(false, list_to_atom(TName), Album).

delete_non_existent_album(Config) ->
  TName = ?value(tname_album, Config),
  Album = "IDontExist",
  {ok, []} = tutils:aql(?format(delete_album, [Album], Config)),
  tutils:assertState(false, list_to_atom(TName), Album).
