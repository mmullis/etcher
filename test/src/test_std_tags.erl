
-module(test_std_tags).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").
-include("../../include/api.hrl").

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------

tag_autoescape_test() ->
    "a &amp; a" = render("{{ s }}", [{s, "a & a"}]),
    "a & a" = render("{% autoescape off %}{{ s }}{% endautoescape %}", 
                        [{s, "a & a"}]),
    "a &amp; a" = render("{% autoescape on %}{{ s }}{% endautoescape %}", 
                        [{s, "a & a"}]),
    ok.

tag_comment_test() ->
    "a-yabba-a" = render("a-{{ s }}-a", [{s, "yabba"}]),
    "b--b" = render("b-{# {{ s }} #}-b", [{s, "yabba"}]),
    "c--c" = render("c-{# {% for i in count %}{{ s }}{% endfor %} #}-c", 
                        [{s, "yabba"}, {count, lists:seq(1, 10)}]),
    "d--d" = render("d-{# {% for i in ... TODO - AFTER SNACK ... #}-d", 
                        [{s, "yabba"}, {count, lists:seq(1, 10)}]),
    ok.

tag_csrf_token_test() ->
    nothing_to_test.

tag_cycle_test() ->
    "xyzxyzxyzx" = render("{% for i in count %}{% cycle 'x' 'y' 'z' %}{% endfor %}", 
                            [{count, lists:seq(1, 10)}]),
    "hot cold hot cold hot " =
                render("{% for i in count %}{% cycle a b %} {% endfor %}", 
                            [{count, lists:seq(1, 5)}, {a, "hot"}, {b, "cold"}]),
    "<p class='comment'>...</p><p class='retort'>...</p><p class='comment'>...</p>" =
                render("<p class='{% cycle 'comment' 'retort' as next_installment %}'>...</p>"
                       "<p class='{% cycle next_installment %}'>...</p>"
                       "<p class='{% cycle next_installment %}'>...</p>"),
    ok.

tag_debug_test() ->
    Context = [{"abc", "hello"}],
    PpContext = lists:flatten(io_lib:format("~p", [Context])),
    DebugText = render("{% debug %}", Context),
    true = is_substring(DebugText, PpContext),
    ok.
    
tag_filter_test() ->
    "SO LONG, AND THANKS FOR ALL THE FISH" =
            render("{% filter upper %}"
                       "So Long, and Thanks for All the Fish"
                   "{% endfilter %}"),
    % No auto-escape in tags
    "Acme & Co." = render("{% filter title %}acme & co.{% endfilter %}"),
    ok.

tag_firstof_test() ->
    "tree" = render("{% firstof '' 0 var 'tree' 2 %}"),
    "22" = render("{% firstof '' 0 var1 var2 var3 %}", [{var1, undefined}, {var3, 22}]),
    "" = render("{% firstof '' 0 var1 var2 var3 %}", [{var1, undefined}, {var3, 0}]),
    ok.

tag_for_test() ->
    L = lists:seq(55, 95, 10),
    "55.65.75.85.95." = render("{% for i in nums %}{{ i }}.{% endfor %}", 
                            [{nums, L}]),
    "95.85.75.65.55." = render("{% for i in nums reversed %}{{ i }}.{% endfor %}", 
                            [{nums, L}]),
    "1.2.3.4.5." = render("{% for i in nums %}{{ forloop.counter }}.{% endfor %}", 
                            [{nums, L}]),
    "0.1.2.3.4." = render("{% for i in nums %}{{ forloop.counter0 }}.{% endfor %}", 
                            [{nums, L}]),
    "5.4.3.2.1." = render("{% for i in nums %}{{ forloop.revcounter }}.{% endfor %}", 
                            [{nums, L}]),
    "4.3.2.1.0." = render("{% for i in nums %}{{ forloop.revcounter0 }}.{% endfor %}", 
                            [{nums, L}]),
    "true.false.false.false.false." = 
                render("{% for i in nums %}{{ forloop.first }}.{% endfor %}", 
                            [{nums, L}]),
    "false.false.false.false.true." = 
                render("{% for i in nums %}{{ forloop.last }}.{% endfor %}", 
                            [{nums, L}]),
    "Fred: 44; Barney: 33; Wilma: 22; " = 
                render("{% for name, age in people %}{{ name }}: {{ age }}; {% endfor %}", 
                            [{people, [{"Fred", 44}, {"Barney", 33}, {"Wilma", 22}]}]),
    "1.1: Getting Started \n" 
    "1.2: Variables and Arithmetic Expressions \n" 
    "2.1: Variable Names \n" 
    "2.2: Data Types and Sizes \n" 
    "2.3: Constants \n" 
    "3.1: Statements and Blocks \n" =
                render("{% for chapter in chapters %}"
                            "{% for section in chapter.sections %}"
                                "{{ forloop.parentloop.counter }}.{{ forloop.counter}}: "
                                    "{{ section.title|safe }} \n"
                            "{% endfor %}"
                       "{% endfor %}", 
                       [{chapters, [
                            #chapter{sections=[
                                        #section{title="Getting Started"},
                                        #section{title="Variables and Arithmetic Expressions"}]},
                            #chapter{sections=[
                                        #section{title="Variable Names"},
                                        #section{title="Data Types and Sizes"},
                                        #section{title="Constants"}]},
                            #chapter{sections=[
                                        #section{title="Statements and Blocks"}]}]}]),
    ok.

tag_if_test() ->
    "aaa" = render("{% if 'woof' %}aaa{% endif %}"),
    "" = render("{% if '' %}bbb{% endif %}"),
    "CCC" = render("{% if '' %}ccc{% else %}CCC{% endif %}"),
    "ddd" = render("{% if not '' %}ddd{% endif %}"),
    "" = render("{% if not 'woof' %}eee{% endif %}"),
    "fff" = render("{% if 'woof' and 'woof' and 'woof' %}fff{% endif %}"),
    "" = render("{% if '' and '' and 'woof' %}ggg{% endif %}"),
    "hhh" = render("{% if '' or '' or 'woof' %}hhh{% endif %}"),
    "iii" = render("{% if not '' and not '' and 'woof' %}iii{% endif %}"),
    "JJJ" = render("{% if not '' and not '' and 0 %}jjj{% else %}JJJ{% endif %}"),
    "" = render("{% if not '' and not '' and var %}kkk{% endif %}"),
    "lll" = render("{% if not '' and not '' and var %}lll{% endif %}", [{var, true}]),
    "" = render("{% if not '' and not '' and var %}mmm{% endif %}", [{var, false}]),
    ok.

tag_ifchanged_test() ->
    Names = ["Fred", "Barney", "Barney", "Barney", "Wilma", "Wilma", "Fred"],
    "Fred:Barney:::Wilma::Fred:" = 
            render("{% for name in names %}"
                        "{% ifchanged name %}{{ name }}{% endifchanged %}"
                        ":"
                   "{% endfor %}",
                   [{names, Names}]),
    "Fred:Barney:***:***:Wilma:***:Fred:" = 
            render("{% for name in names %}"
                        "{% ifchanged name %}{{ name }}"
                        "{% else %}***"
                        "{% endifchanged %}"
                        ":"
                   "{% endfor %}",
                   [{names, Names}]),
    ok.

tag_ifequal_test() ->
    "aaa" = render("{% ifequal 'woof' 'woof' %}aaa{% endifequal %}"),
    "" = render("{% ifequal 'ardvark' 'woof' %}bbb{% endifequal %}"),
    "ccc" = render("{% ifequal s 'woof' %}ccc{% endifequal %}", [{s, "woof"}]),
    "" = render("{% ifequal s 'woof' %}ddd{% endifequal %}", [{s, "ardvark"}]),
    "" = render("{% ifequal s 'woof' %}eee{% endifequal %}"),
    "fff" = render("{% ifequal s s %}fff{% endifequal %}", [{s, "woof"}]),
    "ggg" = render("{% ifequal s1 s2 %}ggg{% endifequal %}", 
                        [{s1, "woof"}, {s2, "woof"}]),
    "" = render("{% ifequal s1 s2 %}hhh{% endifequal %}", 
                        [{s1, "ardvark"}, {s2, "woof"}]),
    ok.

% Same as 'ifequal' tests
tag_ifnotequal_test() ->
    "" = render("{% ifnotequal 'woof' 'woof' %}aaa{% endifnotequal %}"),
    "bbb" = render("{% ifnotequal 'ardvark' 'woof' %}bbb{% endifnotequal %}"),
    "" = render("{% ifnotequal s 'woof' %}ccc{% endifnotequal %}", [{s, "woof"}]),
    "ddd" = render("{% ifnotequal s 'woof' %}ddd{% endifnotequal %}", [{s, "ardvark"}]),
    "eee" = render("{% ifnotequal s 'woof' %}eee{% endifnotequal %}"),
    "" = render("{% ifnotequal s s %}fff{% endifnotequal %}", [{s, "woof"}]),
    "" = render("{% ifnotequal s1 s2 %}ggg{% endifnotequal %}", 
                        [{s1, "woof"}, {s2, "woof"}]),
    "hhh" = render("{% ifnotequal s1 s2 %}hhh{% endifnotequal %}", 
                        [{s1, "ardvark"}, {s2, "woof"}]),
    ok.

% TODO
% tag_now/2

tag_regroup_test() ->
    People1 = [
        #person{first_name="George", last_name="Bush", gender="Male"},
        #person{first_name="Bill", last_name="Clinton", gender="Male"},
        #person{first_name="Margaret", last_name="Thatcher", gender="Female"},
        #person{first_name="Condoleezza", last_name="Rice", gender="Female"},
        #person{first_name="Pat", last_name="Smith", gender="Unknown"}],
    T1 = 
        "{% regroup people by gender as gender_list %}\n"
        "\n"
        "<ul>\n"
        "{% for gender in gender_list %}\n"
        "    <li>{{ gender.grouper }}\n"
        "    <ul>\n"
        "        {% for item in gender.list %}\n"
        "        <li>{{ item.first_name }} {{ item.last_name }}</li>\n"
        "        {% endfor %}\n"
        "    </ul>\n"
        "    </li>\n"
        "{% endfor %}\n"
        "</ul>\n",
    Expected1 =
        "\n"
        "\n"
        "<ul>\n"
        "\n"
        "    <li>Male\n"
        "    <ul>\n"
        "        \n"
        "        <li>George Bush</li>\n"
        "        \n"
        "        <li>Bill Clinton</li>\n"
        "        \n"
        "    </ul>\n"
        "    </li>\n"
        "\n"
        "    <li>Female\n"
        "    <ul>\n"
        "        \n"
        "        <li>Margaret Thatcher</li>\n"
        "        \n"
        "        <li>Condoleezza Rice</li>\n"
        "        \n"
        "    </ul>\n"
        "    </li>\n"
        "\n"
        "    <li>Unknown\n"
        "    <ul>\n"
        "        \n"
        "        <li>Pat Smith</li>\n"
        "        \n"
        "    </ul>\n"
        "    </li>\n"
        "\n"
        "</ul>\n",
    Expected1 = render(T1, [{people, People1}]),

    People2 = [
        #person{first_name="Bill", last_name="Clinton", gender="Male"},
        #person{first_name="Pat", last_name="Smith", gender="Unknown"},
        #person{first_name="Margaret", last_name="Thatcher", gender="Female"},
        #person{first_name="George", last_name="Bush", gender="Male"},
        #person{first_name="Condoleezza", last_name="Rice", gender="Female"}],
    % Note the added dictsort stuff
    T2 = 
        "{% regroup people|dictsort:'gender' by gender as gender_list %}\n"
        "\n"
        "<ul>\n"
        "{% for gender in gender_list %}\n"
        "    <li>{{ gender.grouper }}\n"
        "    <ul>\n"
        "        {% for item in gender.list %}\n"
        "        <li>{{ item.first_name }} {{ item.last_name }}</li>\n"
        "        {% endfor %}\n"
        "    </ul>\n"
        "    </li>\n"
        "{% endfor %}\n"
        "</ul>\n",
    Expected2 =
        "\n"
        "\n"
        "<ul>\n"
        "\n"
        "    <li>Female\n"
        "    <ul>\n"
        "        \n"
        "        <li>Margaret Thatcher</li>\n"
        "        \n"
        "        <li>Condoleezza Rice</li>\n"
        "        \n"
        "    </ul>\n"
        "    </li>\n"
        "\n"
        "    <li>Male\n"
        "    <ul>\n"
        "        \n"
        "        <li>Bill Clinton</li>\n"
        "        \n"
        "        <li>George Bush</li>\n"
        "        \n"
        "    </ul>\n"
        "    </li>\n"
        "\n"
        "    <li>Unknown\n"
        "    <ul>\n"
        "        \n"
        "        <li>Pat Smith</li>\n"
        "        \n"
        "    </ul>\n"
        "    </li>\n"
        "\n"
        "</ul>\n",
    Expected2 = render(T2, [{people, People2}]),
    ok.

tag_spaceless_test() ->
    "<p><a href=\"foo/\">Foo</a></p>" =
            render("{% spaceless %}<p>
                        <a href=\"foo/\">Foo</a>
                            </p>{% endspaceless %}"),
    ok.

tag_templatetag_test() ->
    "{% %}" = render("{% templatetag openblock %} {% templatetag closeblock %}"), 
    "{{ }}" = render("{% templatetag openvariable %} {% templatetag closevariable %}"), 
    "{# #}" = render("{% templatetag opencomment %} {% templatetag closecomment %}"), 
    ok.

tag_url_test() ->
    UrlMapper = fun(What) -> url_mapper(What) end,
    "<a href=\"http://www.wikipedia.org/\">wikipedia</a>" =
            render("<a href=\"{% url wikipedia %}\">wikipedia</a>", 
                        [], [{url_mapper, UrlMapper}]),
    "<a href=\"http://www.wikipedia.org/rooster\">wikipedia</a>" =
            render("<a href=\"{% url wikipedia rooster %}\">wikipedia</a>", 
                        [], [{url_mapper, UrlMapper}]),
    ok.

url_mapper(["wikipedia" | What]) ->
    case What of
        [] ->
            "http://www.wikipedia.org/";
        [Subject] ->
            "http://www.wikipedia.org/" ++ Subject
    end;
url_mapper(["erlang"]) ->
    "http://www.erlang.org/";
url_mapper(_) ->
    "http://www.example.com".

tag_widthratio_test() ->
    "<img src=\"bar.gif\" height=\"10\" width=\"88\" />" =
            render("<img src=\"bar.gif\" height=\"10\" "
                        "width=\"{% widthratio this_value max_value 100 %}\" />",
                   [{this_value, 175}, {max_value, 200}]),
    ok.

tag_with_test() ->
    "(orange)" = render("{% with 'orange' as fruit %}({{ fruit }}){% endwith %}"),
    "(orange)" = render("{% with var as fruit %}({{ fruit }}){% endwith %}", 
                            [{var, "orange"}]),
    "()" = render("{% with var as fruit %}({{ fruit }}){% endwith %}"),
    ok.


%%-------------------------------------------------------------------
%% Misc.
%%-------------------------------------------------------------------

render(Text) ->
    render(Text, []).

render(Text, Context) ->
    render(Text, Context, []).

render(Text, Context, Opts) ->
    {ok, Template} = etcher:compile(Text),
    etcher:render(Template, Context, [{return, list} | Opts]).

is_substring([_ | Rest] = S, MatchStr) ->
    case lists:prefix(MatchStr, S) of
        true ->
            true;
        false ->
            is_substring(Rest, MatchStr)
    end;
is_substring([], _MatchStr) ->
    false.


