
-module(test_etcher_core).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------

auto_escape_test() ->
    "a & a" = render("a & a"),
    "b & b" = render("{{ 'b & b' }}"),
    "c &amp; c" = render("{{ s }}", [{s, "c & c"}]),
    "d &amp; d" = render("{{ s|escape }}", [{s, "d & d"}]),
    "e & e" = render("{{ s|safe }}", [{s, "e & e"}]),
    "f & f" = render("{{ s|safe|escape }}", [{s, "f & f"}]),
    "g & g" = render("{{ s }}", [{s, "g & g"}], [{auto_escape, false}]),
    "h &amp; h" = render("{{ s|escape }}", [{s, "h & h"}], [{auto_escape, false}]),
    "i &amp; i" = render("{{ s }}", [{s, "i & i"}], [{auto_escape, true}]),
    "j & j" = render("{{ s|escape|safe }}", [{s, "j & j"}]),
    "k &amp; k" = render("{{ s|force_escape|safe }}", [{s, "k & k"}]),
    "l &amp; l" = render("{% autoescape on %}{{ s }}{% endautoescape %}", 
                         [{s, "l & l"}], [{auto_escape, false}]),
    "m & m" = render("{% autoescape off %}{{ s }}{% endautoescape %}", 
                     [{s, "m & m"}]),
    "n &amp; n" = render("{% autoescape off %}{{ s|escape }}{% endautoescape %}", 
                         [{s, "n & n"}]),
    "o & o" = render("{% autoescape off %}{{ s|escape|safe }}{% endautoescape %}", 
                     [{s, "o & o"}]),
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

