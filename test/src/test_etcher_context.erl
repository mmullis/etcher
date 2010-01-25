
-module(test_etcher_context).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------

proplist_test() ->
    "Fred" =
        render("{{ blog.author }}", 
               [{blog, [{author, "Fred"}]}]).

nested_proplist_test() ->
    "Yabba!" = 
        render("{{ todays_post.previous.title }}", 
               [{todays_post, 
                        [{title, "Dabba!"},
                         {previous, 
                                [{title, "Yabba!"},
                                 {previous, null}]}
                        ]}
               ]).

deep_nested_proplist_test() ->
    "Flintstone Mania" =
        render("{{ todays_post.previous.previous.title }}", 
               [{todays_post, 
                    [{title, "Dabba!"},
                     {previous, 
                            [{title, "Yabba!"},
                             {previous, 
                                    [{title, "Flintstone Mania"},
                                     {previous, null}]}
                                    ]}
                            ]}
                    ]).

record_test() ->
    "Fred" = render("{{ blog.author }}", 
                    [{blog, #blog{author="Fred"}}]).

nested_record_test() ->
    "Yabba!" = 
        render("{{ todays_post.previous.title }}", 
               [{todays_post, #post{title="Dabba!",
                                    previous=#post{title="Yabba!",
                                                   previous=null}}}]).

deep_nested_record_test() ->
    "Flintstone Mania" =
        render("{{ todays_post.previous.previous.title }}", 
               [{todays_post, #post{title="Dabba!",
                                    previous=#post{title="Yabba!",
                                                   previous=#post{title="Flintstone Mania",
                                                                  previous=null}}}}]).

index_record_test() ->
    Context = [{posts, [#post{title="One"},
                        #post{title="Two"},
                        #post{title="Three"}]}],
    "One" = render("{{ posts.0.title }}", Context),
    "Two" = render("{{ posts.1.title }}", Context),
    "Three" = render("{{ posts.2.title }}", Context),
    "" = render("{{ posts.3.title }}", Context),
    "" = render("{{ posts.0.title }}", []),
    ok.

index_record_index_string_test() ->
    Context = [{posts, [#post{comments=["Apple", "Ardvark", "Accountable"]},
                        #post{comments=["Bubble", "Bronze", "Botswana"]},
                        #post{comments=["Careful", "Canary", "Craft"]}]}],
    "Apple" = render("{{ posts.0.comments.0 }}", Context),
    "Ardvark" = render("{{ posts.0.comments.1 }}", Context),
    "Accountable" = render("{{ posts.0.comments.2 }}", Context),
    "" = render("{{ posts.0.comments.3 }}", Context),
    "Bubble" = render("{{ posts.1.comments.0 }}", Context),
    "Bronze" = render("{{ posts.1.comments.1 }}", Context),
    "Botswana" = render("{{ posts.1.comments.2 }}", Context),
    "" = render("{{ posts.1.comments.3 }}", Context),
    "Careful" = render("{{ posts.2.comments.0 }}", Context),
    "Canary" = render("{{ posts.2.comments.1 }}", Context),
    "Craft" = render("{{ posts.2.comments.2 }}", Context),
    "" = render("{{ posts.2.comments.3 }}", Context),
    "" = render("{{ posts.3.comments.0 }}", Context),
    ok.

index_record_index_record_test() ->
    Context = [{posts, [#post{comments=[#comment{opinion="Apple"}, 
                                        #comment{opinion="Ardvark"}, 
                                        #comment{opinion="Accountable"}]},
                        #post{comments=[#comment{opinion="Bubble"}, 
                                        #comment{opinion="Bronze"}, 
                                        #comment{opinion="Botswana"}]},
                        #post{comments=[#comment{opinion="Careful"}, 
                                        #comment{opinion="Canary"}, 
                                        #comment{opinion="Craft"}]}]}],
    "Apple" = render("{{ posts.0.comments.0.opinion }}", Context),
    "Ardvark" = render("{{ posts.0.comments.1.opinion }}", Context),
    "Accountable" = render("{{ posts.0.comments.2.opinion }}", Context),
    "" = render("{{ posts.0.comments.3.opinion }}", Context),
    "Bubble" = render("{{ posts.1.comments.0.opinion }}", Context),
    "Bronze" = render("{{ posts.1.comments.1.opinion }}", Context),
    "Botswana" = render("{{ posts.1.comments.2.opinion }}", Context),
    "" = render("{{ posts.1.comments.3.opinion }}", Context),
    "Careful" = render("{{ posts.2.comments.0.opinion }}", Context),
    "Canary" = render("{{ posts.2.comments.1.opinion }}", Context),
    "Craft" = render("{{ posts.2.comments.2.opinion }}", Context),
    "" = render("{{ posts.2.comments.3.opinion }}", Context),
    "" = render("{{ posts.3.comments.0.opinion }}", Context),
    ok.

string_key_proplist_test() ->
    "Fred" =
        render("{{ blog.author }}", 
               [{"blog", [{"author", "Fred"}]}]).

%%-------------------------------------------------------------------
%% Misc.
%%-------------------------------------------------------------------

render(Text, Context) ->
    {ok, Template} = etcher:compile(Text),
    etcher:render(Template, Context, [{return, list}]).

