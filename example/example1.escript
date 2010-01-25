#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ../ebin 

-include_lib("../example/records.hrl").

main([]) ->
    application:start(etcher),
    etcher:add_record_defs(filename:absname("records.hrl")),
    {ok, TemplateStr} = file:read_file("example1.tpl"),
    {ok, Template} = etcher:compile(TemplateStr),
    Collaborations = extract_source_data(),
    Context = [{collaborations, Collaborations}],
    Page = etcher:render(Template, Context, [{return, binary}]),
    io:format("~s", [Page]);
main(_) ->
    usage().

usage() ->
    io:format("Usage: ./example1.escript~n").

%%---------------------------------------------------------------------
%% The following just extracts data from the "example1-data.txt" file.
%%---------------------------------------------------------------------

extract_source_data() ->
    {ok, Data} = file:read_file("example1-data.txt"),
    Lines = split(Data, "\n"),
    scan_lines(Lines, []).

scan_lines(["        " ++ FilmData | Rest], Acc) ->
    {match, [Title, Year]} = match(FilmData, "^(.*?)\\(.*(\\d{4}).*\\).*$"),
    Film = #film{title=Title, year=Year},
    [#collaboration{films=Films} = LastCollab | AccRest] = Acc,
    Films1 = Films ++ [Film],
    LastCollab1 = LastCollab#collaboration{films=Films1},
    Acc1 = [LastCollab1 | AccRest],
    scan_lines(Rest, Acc1);
scan_lines(["    " ++ CollabStr | Rest], Acc) ->
    [DirName, ActorName] = split(CollabStr, "\\s*/\\s*"),
    Director = scan_name(DirName),
    Actor = scan_name(ActorName),
    Collab = #collaboration{director=Director, actor=Actor},
    scan_lines(Rest, [Collab | Acc]);
scan_lines(["" | Rest], Acc) ->
    scan_lines(Rest, Acc);
scan_lines([], Acc) ->
    lists:reverse(Acc).

scan_name(Name) ->
    [FirstName | OtherNames] = string:tokens(Name, " "),
    LastName = string:join(OtherNames, " "),
    #person{first_name=FirstName, last_name=LastName}.

split(Subject, Regex) ->
    re:split(Subject, Regex, [unicode, {return, list}]).

match(Subject, Regex) ->
    re:run(Subject, Regex, [unicode, {capture, all_but_first, list}]).

