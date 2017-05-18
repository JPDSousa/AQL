-module(parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 330).

-file("/usr/lib/erlang/lib/parsetools-2.1.1/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.erl", 179).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
yeccpars2_0(S, create, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, delete, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, drop, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, insert, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, select, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, update, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccgoto_query(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_query(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_3/7}).
yeccpars2_3(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_3(S, semi_colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_query(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_query(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccgoto_query(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_query(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
yeccpars2_8(S, table, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, table_policy, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_9/7}).
yeccpars2_9(S, from, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_10/7}).
yeccpars2_10(S, table, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_11/7}).
yeccpars2_11(S, into, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_12/7}).
yeccpars2_12(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, wildcard, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_13/7}).
yeccpars2_13(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_14/7}).
yeccpars2_14(S, set, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_15(S, where, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_update_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_16/7}).
yeccpars2_16(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_17(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_set_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_18(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_set_assignments(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_19/7}).
yeccpars2_19(S, assign, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, decrement, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, increment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_20/7}).
yeccpars2_20(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_21(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_set_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_22(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_set_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_set_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_set_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_set_assignment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_28: see yeccpars2_16

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_set_assignments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_30: see yeccpars2_16

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_set_assignments(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_33(S, conjunctive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_update_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_where_clauses(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
yeccpars2_35(S, equality, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_36: see yeccpars2_20

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_where_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_38: see yeccpars2_32

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_where_clauses(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
yeccpars2_40(S, from, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_41/7}).
yeccpars2_41(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_select_fields(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).
yeccpars2_43(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_select_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_45/7}).
yeccpars2_45(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_46/7}).
yeccpars2_46(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_46(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_select_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, where, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_select_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_49: see yeccpars2_32

yeccpars2_50(S, conjunctive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_select_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_51/7}).
yeccpars2_51(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_52/7}).
yeccpars2_52(S, start_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(S, values, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_53/7}).
yeccpars2_53(S, values, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_54/7}).
yeccpars2_54(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_55/7}).
yeccpars2_55(S, start_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_insert_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_57: see yeccpars2_20

yeccpars2_58(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_insert_values(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_59/7}).
yeccpars2_59(S, end_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_insert_values_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_61: see yeccpars2_20

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_insert_values(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_63: see yeccpars2_20

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_insert_values(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_65/7}).
yeccpars2_65(S, end_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_66(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccgoto_insert_keys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_67/7}).
yeccpars2_67(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_insert_keys(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_insert_keys_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_70/7}).
yeccpars2_70(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_insert_keys(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_72: see yeccpars2_55

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_insert_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_74/7}).
yeccpars2_74(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_drop_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_76/7}).
yeccpars2_76(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_77/7}).
yeccpars2_77(S, where, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_78: see yeccpars2_32

yeccpars2_79(S, conjunctive, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_delete_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_80/7}).
yeccpars2_80(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_81/7}).
yeccpars2_81(S, table, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_82: see yeccpars2_80

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_create_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_84/7}).
yeccpars2_84(S, start_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_84(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_85/7}).
yeccpars2_85(S, atom_value, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_86/7}).
yeccpars2_86(S, end_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_87/7}).
yeccpars2_87(S, attribute_type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_88(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_create_keys(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_attribute_name(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_85

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_create_keys(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(S, check, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, primary, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_93/7}).
yeccpars2_93(S, comparators, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_94/7}).
yeccpars2_94(S, key, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_96: see yeccpars2_20

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_attribute(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_table_metadata(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_99: see yeccpars2_85

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_create_keys(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_create_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_102(S, create, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, delete, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, drop, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, insert, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, select, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, update, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(S, semi_colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_attribute(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_attribute(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_attribute(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_attribute_name(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_attribute_name(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_attribute_name(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_create_keys(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_create_query(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_create_query(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_delete_query(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_delete_query(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_drop_query(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_drop_query(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_insert_keys(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(65, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_insert_keys_clause(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_insert_query(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_insert_query(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_insert_values(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_insert_values_clause(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_insert_values_clause(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_query(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_query(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_select_fields(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_select_query(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_select_query(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_set_assignment(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_assignment(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_assignment(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_set_assignments(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(17, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_set_clause(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_table_metadata(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_table_metadata(82=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_update_query(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_update_query(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_value(20=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_where_clause(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where_clause(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where_clause(49=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where_clause(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_where_clauses(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where_clauses(49, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where_clauses(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_1_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 111).
yeccpars2_1_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_2_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 99).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 103).
yeccpars2_4_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 119).
yeccpars2_5_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 107).
yeccpars2_6_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 115).
yeccpars2_7_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 211).
yeccpars2_15_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { update , [ { table , __2 } , __3 ] }
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 219).
yeccpars2_17_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { set , __2 }
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 231).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 249).
yeccpars2_21_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 , { number , 1 } }
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 241).
yeccpars2_22_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 , { number , 1 } }
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 245).
yeccpars2_23_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 253).
yeccpars2_24_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_25_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 236).
yeccpars2_25_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 227).
yeccpars2_29_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 223).
yeccpars2_31_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : flatten ( __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 215).
yeccpars2_33_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { update , [ { table , __2 } , __3 , { where , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 150).
yeccpars2_34_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 158).
yeccpars2_37_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 154).
yeccpars2_39_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : append ( __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 142).
yeccpars2_44_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 138).
yeccpars2_47_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : flatten ( __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 126).
yeccpars2_48_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { select , [ { table , __4 } , { keys , __2 } ] }
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 130).
yeccpars2_50_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { select , [ { table , __4 } , { keys , __2 } , { where , __6 } ] }
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 171).
yeccpars2_56_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { insert , [ { table , __3 } , { values , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 203).
yeccpars2_58_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 191).
yeccpars2_60_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 195).
yeccpars2_62_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : flatten ( [ __1 , __3 ] )
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 199).
yeccpars2_64_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_66_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 187).
yeccpars2_66_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 183).
yeccpars2_68_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 175).
yeccpars2_69_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 179).
yeccpars2_71_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : flatten ( [ __1 , __3 ] )
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 166).
yeccpars2_73_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { insert , [ { table , __3 } , { keys , __4 } , { values , __6 } ] }
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 310).
yeccpars2_75_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { drop , [ { table , __3 } ] }
  end | __Stack].

-compile({inline,yeccpars2_79_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 260).
yeccpars2_79_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { delete , [ { table , __3 } , { record_time , __5 } ] }
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 271).
yeccpars2_83_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { create , { table , lists : flatten ( [ __2 ] , __4 ) } }
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 283).
yeccpars2_91_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 299).
yeccpars2_92_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { attribute , [ { name , __1 } , __2 , { constraint , none } ] }
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 291).
yeccpars2_95_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { attribute , [ { name , __1 } , __2 , { constraint , primary_key } ] }
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 295).
yeccpars2_97_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { attribute , [ { name , __1 } , __2 , { constraint , { __4 , __5 } } ] }
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 275).
yeccpars2_98_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ { name , __1 } , { keys , __3 } ]
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 279).
yeccpars2_100_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : flatten ( [ __1 , __3 ] )
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 267).
yeccpars2_101_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { create , { table , __3 } }
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 95).
yeccpars2_102_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 91).
yeccpars2_103_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   lists : append ( __1 , __3 )
  end | __Stack].


-file("/home/joao/workspace/AQL/_build/default/lib/aql/src/parser.yrl", 331).
