
-module(test_std_filters).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").
-include("../../include/api.hrl").

% TODO - Need to add tests to each filter test function to ensure 
%        that the 'safe' functions are returning safe strings if that's
%        what they received.

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------

filter_add_test() ->
    "1" = render("{{ 0|add:1 }}"),
    "2" = render("{{ 1|add:1 }}"),
    "2" = render("{{ '1'|add:1 }}"),
    "2" = render("{{ 1|add:'1' }}"),
    "7" = render("{{ -1|add:8 }}"),
    "-8" = render("{{ 1|add:-9 }}"),
    "333" = render("{{ 111|add:222 }}"),
    "3" = render("{{ 1.1|add:2 }}"),
    "4" = render("{{ 1.9|add:2 }}"),
    "2" = render("{{ 1|add:1.1 }}"),
    "201" = render("{{ 2.0e2|add:1 }}"),
    "3001" = render("{{ 3e3|add:1 }}"),
    "53" = render("{{ 4e-2|add:53 }}"),
    "77" = render("{{ -5e-2|add:77 }}"),
    "" = render("{{ 'none'|add:'109' }}"),
    "" = render("{{ 612|add:'none' }}"),
    "" = render("{{ a|add:431 }}"),
    "" = render("{{ 983|add:a }}"),
    "746" = render("{{ 745|add:a }}", [{a, 1}]),
    "322" = render("{{ 321|add:a }}", [{a, "1"}]),
    "651" = render("{{ 650|add:a }}", [{a, 1.442123}]),
    "452" = render("{{ a|add:451 }}", [{a, 1}]),
    "313" = render("{{ a|add:312 }}", [{a, "1"}]),
    "983" = render("{{ a|add:981 }}", [{a, 1.87172}]),
    ok.

filter_addslashes_test() ->
    "\\'a\\'" = render("{{ \"'a'\"|addslashes }}"),
    " \\'b\\' " = render("{{ \" 'b' \"|addslashes }}"),
    "\\'c\\'\\'c\\'" = render("{{ \"'c''c'\"|addslashes }}"),
    "\\'d\\' \\'d\\'" = render("{{ \"'d' 'd'\"|addslashes }}"),
    % The following input is tricky. Erlang strings and Etcher scanner also 
    % use backslashes as escape characters. The test for 'f' does the 
    % same as the test for 'e', but uses variables, so it's clearer.
    "\\\\e" = render("{{ '\\\\e'|addslashes }}"),
    "\\\\f" = render("{{ s|addslashes }}", [{s, "\\f"}]),
    "\\\"g\\\"" = render("{{ '\"g\"'|addslashes }}"),
    ok.

filter_capfirst_test() ->
    "Aa" = render("{{ 'aa'|capfirst }}"),
    " bb" = render("{{ ' bb'|capfirst }}"),
    "<cc>" = render("{{ '<cc>'|capfirst }}"),
    "Dd dd" = render("{{ 'Dd dd'|capfirst }}"),
    "E\x{400}e" = render("{{ 'e\x{400}e'|capfirst }}"),
    "\x{400}ff" = render("{{ '\x{400}ff'|capfirst }}"),
    ok.

filter_center_test() ->
    "  aa  " = render("{{ 'aa'|center:6 }}"),
    " bb  " = render("{{ 'bb'|center:5 }}"),
    " cc " = render("{{ 'cc'|center:fsize }}", [{fsize, 4}]),
    % Note: Django throws exception for the following 2 cases
    "" = render("{{ 'dd'|center:'woof' }}"),
    "" = render("{{ 'ee'|center:fsize }}", [{fsize, "woof"}]),
    "ff" = render("{{ 'ff'|center:0 }}"),
    "gg" = render("{{ 'gg'|center:1 }}"),
    "hh" = render("{{ 'hh'|center:2 }}"),
    "ii" = render("{{ 'ii'|center:-1 }}"),
    " jj " = render("{{ 'jj'|center:4.356 }}"),
    " \x{400}\x{400} " = render("{{ '\x{400}\x{400}'|center:4 }}"),
    ok.

filter_cut_test() ->
    "aa" = render("{{ 'axa'|cut:'x' }}"),
    "bb" = render("{{ 'xbxbx'|cut:'x' }}"),
    "xccx" = render("{{ 'xcxyxcx'|cut:'xyx' }}"),
    "" = render("{{ 'd'|cut:'d' }}"),
    "eex" = render("{{ 'eexyx'|cut:s }}", [{s, "yx"}]),
    "f" = render("{{ 'ffx'|cut:s }}", [{s, "fx"}]),
    "24" = render("{{ 2334|cut:n }}", [{n, 3}]),
    "gg" = render("{{ 'g\x{400}g'|cut:s }}", [{s, "\x{400}"}]),
    "\x{400}hh\x{400}" = render("{{ '\x{400}h\x{400}\x{400}h\x{400}'|cut:s }}", 
                        [{s, "\x{400}\x{400}"}]),
    ok.

% TODO
% filter_date/2
% filter_date/3

filter_default_test() ->
    "{aa}" = render("{{ t|default:'xx' }}", [{t, {aa}}]),
    "bb" = render("{{ t|default:'bb' }}", [{t, undefined}]),
    "cc" = render("{{ t|default:'cc' }}", [{t, 0}]),
    "dd" = render("{{ t|default:'dd' }}", [{t, ""}]),
    "ee" = render("{{ t|default:'ee' }}", [{t, <<>>}]),
    "ff" = render("{{ t|default:'ff' }}", [{t, false}]),
    "gg" = render("{{ t|default:'gg' }}", [{t, #string{val=""}}]),
    "true" = render("{{ t|default:'hh' }}", [{t, true}]),
    " " = render("{{ t|default:'ii' }}", [{t, " "}]),
    "1" = render("{{ t|default:'jj' }}", [{t, 1}]),
    ok.

filter_default_if_none_test() ->
    "{aa}" = render("{{ t|default_if_none:'xx' }}", [{t, {aa}}]),
    "bb" = render("{{ t|default_if_none:'bb' }}", [{t, undefined}]),
    "0" = render("{{ t|default_if_none:'cc' }}", [{t, 0}]),
    "" = render("{{ t|default_if_none:'dd' }}", [{t, ""}]),
    "" = render("{{ t|default_if_none:'ee' }}", [{t, <<>>}]),
    "false" = render("{{ t|default_if_none:'ff' }}", [{t, false}]),
    "" = render("{{ t|default_if_none:'gg' }}", [{t, #string{val=""}}]),
    ok.

filter_dictsort_test() ->
    R1 = [{"name", "Fred"}, {"age", 44}, {"drives", false}],
    R2 = [{"name", "Barney"}, {"age", 33}, {"drives", true}],
    R3 = [{"name", "Wilma"}, {"age", 22}, {"drives", true}],
    L = [R1, R2, R3],
    ByDrives = stringify([R1, R2, R3]),
    ByAge = stringify([R3, R2, R1]),
    ByName = stringify([R2, R1, R3]),
    {by_drives, ByDrives} = 
            {by_drives, 
             render("{{ d|dictsort:'drives' }}", [{d, L}], [{auto_escape, false}])},
    {by_age, ByAge} = 
            {by_age, 
             render("{{ d|dictsort:'age' }}", [{d, L}], [{auto_escape, false}])},
    {by_name, ByName} = 
            {by_name, 
             render("{{ d|dictsort:'name' }}", [{d, L}], [{auto_escape, false}])},
    ok.

filter_dictsortreversed_test() ->
    R1 = [{"name", "Fred"}, {"age", 44}, {"drives", false}],
    R2 = [{"name", "Barney"}, {"age", 33}, {"drives", true}],
    R3 = [{"name", "Wilma"}, {"age", 22}, {"drives", true}],
    L = [R1, R2, R3],
    ByDrivesRev = stringify([R3, R2, R1]),
    ByAgeRev = stringify([R1, R2, R3]),
    ByNameRev = stringify([R3, R1, R2]),
    {by_drives, ByDrivesRev} = 
            {by_drives, 
             render("{{ d|dictsortreversed:'drives' }}", [{d, L}], [{auto_escape, false}])},
    {by_age, ByAgeRev} = 
            {by_age, 
             render("{{ d|dictsortreversed:'age' }}", [{d, L}], [{auto_escape, false}])},
    {by_name, ByNameRev} = 
            {by_name, 
             render("{{ d|dictsortreversed:'name' }}", [{d, L}], [{auto_escape, false}])},
    ok.

filter_divisibleby_test() ->
    "true" = render("{{ 4|divisibleby:2 }}"),
    "false" = render("{{ 19|divisibleby:2 }}"),
    "" = render("{{ 'woof'|divisibleby:2 }}"),
    "" = render("{{ 2|divisibleby:'meow' }}"),
    "true" = render("{{ 88.47|divisibleby:44 }}"),
    "true" = render("{{ 220|divisibleby:22.39 }}"),
    ok.

% Escape is better tested in test_etcher_core:auto_escape_test/0
filter_escape_test() ->
    "a &amp; a" = render("{{ s|escape }}", [{s, "a & a"}], [{auto_escape, false}]),
    % Literals are marked safe
    "b & b" = render("{{ 'b & b'|escape }}", [], [{auto_escape, false}]),
    "c &amp; c" = render("{{ s|escape|escape }}", [{s, "c & c"}], [{auto_escape, false}]),
    ok.

% TODO
% filter_escapejs/2

filter_estringformat_test() ->
    "1.22aa" = render("{{ 1.224522|estringformat:'.2faa' }}"),
    % Shouldn't have leading tilda
    "" = render("{{ 1.224522|estringformat:'~f' }}"),
    "" = render("{{ 44.224522|estringformat:'d' }}"),
    "55" = render("{{ 55|estringformat:'b' }}"),
    ok.

% Note: etcher's implemementation of filesizeformat doesn't pluralize 
% correctly at the moment.
filter_filesizeformat_test() ->
    "aa - 0 bytes" = render("aa - {{ 0|filesizeformat }}"),
    "bb - 1 bytes" = render("bb - {{ 1|filesizeformat }}"),
    "cc - 1023 bytes" = render("cc - {{ 1023|filesizeformat }}"),
    "dd - 1.0 KB" = render("dd - {{ 1024|filesizeformat }}"),
    "ee - 1.5 KB" = render("ee - {{ 1500|filesizeformat }}"),
    "ff - 1024.0 KB" = render("ff - {{ 1048575|filesizeformat }}"),
    "gg - 1.0 MB" = render("gg - {{ 1048576|filesizeformat }}"),
    "hh - 1024.0 MB" = render("hh - {{ 1073741823|filesizeformat }}"),
    "ii - 1.0 GB" = render("ii - {{ 1073741824|filesizeformat }}"),
    % Note: Django throws a runtime exception for the following
    "jj - " = render("jj - {{ 'woof'|filesizeformat }}"),
    ok.

filter_first_test() ->
    "97" = render("{{ 'abc'|first }}"),
    "98" = render("{{ 'b\x{400}cd'|first }}"),
    "1024" = render("{{ '\x{400}cde'|first }}"),
    "dd" = render("{{ items|first }}", [{items, ["dd", "ee", "ff"]}]),
    "" = render("{{ items|first }}", [{items, []}]),
    "" = render("{{ items|first }}"),
    ok.

filter_fix_ampersands_test() ->
    "a &amp; a" = render("{{ 'a & a'|fix_ampersands }}"),
    "b &amp; b" = render("{{ 'b &amp; b'|fix_ampersands }}"),
    "c &lt; c" = render("{{ 'c &lt; c'|fix_ampersands }}"),
    ok.

filter_floatformat_test() ->
   "aa - 1.20" = render("aa - {{ 1.204321|floatformat:2 }}"),
   "bb - 1.20" = render("bb - {{ 1.204321|floatformat:-2 }}"),
   "cc - 1.00" = render("cc - {{ 1.003333|floatformat:2 }}"),
   "dd - 1" = render("dd - {{ 1.0033333|floatformat:-2 }}"),
   "ee - " = render("ee - {{ 5|floatformat:0 }}"),
   "ff - 500.0000" = render("ff - {{ 500|floatformat:4 }}"),
   "gg - 500" = render("gg - {{ 500|floatformat }}"),
   "hh - 81.8" = render("hh - {{ 81.78432|floatformat }}"),
   ok.

filter_force_escape_test() ->
    "a &amp; a" = render("{{ 'a & a'|force_escape }}"),
    "b &amp; b" = render("{{ 'b & b'|force_escape|safe }}"),
    "c &amp; c" = render("{{ 'c & c'|safe|force_escape }}"),
    "d &amp;amp; d" = render("{{ 'd & d'|force_escape|force_escape }}"),
    ok.

filter_get_digit_test() ->
   "aa - 5" = render("aa - {{ 12345|get_digit:1 }}"),
   "bb - 4" = render("bb - {{ 12345|get_digit:2 }}"),
   "cc - 1" = render("cc - {{ 12345|get_digit:5 }}"),
   "dd - 0" = render("dd - {{ 12345|get_digit:6 }}"),
   "ee - 0" = render("ee - {{ 12345|get_digit:999999 }}"),
   "ff - 4" = render("ff - {{ 12345.89923|get_digit:2 }}"),
   "gg - 6" = render("gg - {{ 12345.89923|get_digit:1 }}"),
   "hh - woof" = render("hh - {{ 'woof'|get_digit:1 }}"),
   "ii - " = render("ii - {{ yabba|get_digit:1 }}"),
   ok.

filter_iriencode_test() ->
    "http://whatever/a/a#a" = render("{{ 'http://whatever/a/a#a'|iriencode }}"),
    "http://whatever/b%20b/b" = render("{{ 'http://whatever/b b/b'|iriencode }}"),
    % Note that the unicode "\x{400}" (same as [1024]) is first turned into a 
    % UTF-8 encoded binary <<208,128>>, and is then output as a %hex string.
    "http://whatever/c%D0%80c/c" = render("{{ 'http://whatever/c\x{400}c/c'|iriencode }}"),
    ok.

filter_join_test() ->
   "aa~xx~yy" = render("{{ items|join:'~' }}", [{items, ["aa", "xx", "yy"]}]),
   "bb" = render("{{ items|join:'~' }}", [{items, ["bb"]}]),
   % Join only works on strings - not ints 
   "cc" = render("{{ 'cc'|join:'~' }}"),
   ok.

% These are the reverse of the tests for 'first' filter
filter_last_test() ->
    "97" = render("{{ 'cba'|last }}"),
    "98" = render("{{ 'dc\x{400}b'|last }}"),
    "1024" = render("{{ 'edc\x{400}'|last }}"),
    "dd" = render("{{ items|last }}", [{items, ["ff", "ee", "dd"]}]),
    "" = render("{{ items|last }}", [{items, []}]),
    "" = render("{{ items|last }}"),
    ok.

filter_length_test() ->
    "6" = render("{{ 'aaaaaa'|length }}"),
    "7" = render("{{ items|length }}", [{items, lists:seq(1, 7)}]),
    "8" = render("{{ items|length }}", [{items, [[N] || N <- lists:seq(1, 8)]}]),
    "0" = render("{{ items|length }}", [{items, []}]),
    % The following returns zero because the default 'template_string_if_invalid'
    % is an empty string.
    "0" = render("{{ items|length }}"),
    "" = render("{{ items|length }}", [{items, undefined}]),
    "" = render("{{ items|length }}", [{items, {woof}}]),
    ok.

filter_length_is_test() ->
    "true" = render("{{ 'aaaaaa'|length_is:6 }}"),
    "false" = render("{{ 'bbbbbb'|length_is:7 }}"),
    "" = render("{{ 'cccccc'|length_is:n }}"),
    "" = render("{{ 'dddddd'|length_is:'woof' }}"),
    "" = render("{{ 2222|length_is:4 }}"),
    ok.

filter_linebreaks_test() ->
    "<p>aa<br />aa<br />aa</p>" = 
            render("{{ s|linebreaks|safe }}", [{s, "aa\naa\naa"}]),
    "<p>bb<br />bb<br />bb</p>\n\n<p>bb</p>" = 
            render("{{ s|linebreaks|safe }}", [{s, "bb\nbb\nbb\n\nbb"}]),
    "<p>cc<br />cc<br />cc</p>\n\n<p>cc</p>" = 
            render("{{ s|linebreaks|safe }}", [{s, "cc\ncc\ncc\n\n\n\n\n\ncc"}]),
    ok.

filter_linebreaksbr_test() ->
    "aa<br />aa<br />aa" = render("{{ s|linebreaksbr|safe }}", [{s, "aa\naa\naa"}]),
    % Just operates on $\n not on $\r\n
    "bb\r<br />bb" = render("{{ s|linebreaksbr|safe }}", [{s, "bb\r\nbb"}]),
    "cc<br /><br /><br />" = render("{{ s|linebreaksbr|safe }}", [{s, "cc\n\n\n"}]),
    ok.

filter_linenumbers_test() ->
    "1. aa\n2. aa\n3. aa" = render("{{ s|linenumbers }}", [{s, "aa\naa\naa"}]),
    "01. b\n02. b\n03." ++ _  = 
            render("{{ s|linenumbers }}", [{s, "b\nb\nb\nb\nb\nb\nb\nb\nb\nb\nb"}]),
    "1. ccc" = render("{{ s|linenumbers }}", [{s, "ccc"}]),
    "1. dd\x{400}\n2. d\x{400}d\n3. \x{400}dd" = 
            render("{{ s|linenumbers }}", [{s, "dd\x{400}\nd\x{400}d\n\x{400}dd"}]),
    ok.

filter_ljust_test() ->
    "xaa        x" = render("x{{ 'aa'|ljust:10 }}x"),
    "xaaaaax" = render("x{{ 'aaaaa'|ljust:2 }}x"),
    "x  x" = render("x{{ s|ljust:2 }}x"),
    ok.

filter_lower_test() ->
    "aaa" = render("{{ 'AaA'|lower }}"),
    "\x{400}bb\x{400}bb" = render("{{ s|lower }}", [{s, "\x{400}bB\x{400}Bb"}]),
    ok.

filter_make_list_test() ->
    "abc" = render("{{ 'abc'|make_list }}"),
    "123" = render("{{ 123|make_list }}"),
    "{woof}" = render("{{ t|make_list }}", [{t, {woof}}]),
    ok.

filter_phone2numeric_test() ->
    "0800-43556-96753" = render("{{ tel|phone2numeric }}", 
                                [{tel, "0800-HELLO-world"}]),
    ok.

filter_pluralize_test() ->
    "You have 0 messages" =
            render("You have {{ num_messages }} message{{ num_messages|pluralize }}",
                   [{num_messages, 0}]),
    "You have 1 message" =
            render("You have {{ num_messages }} message{{ num_messages|pluralize }}",
                   [{num_messages, 1}]),
    "You have 2 messages" =
            render("You have {{ num_messages }} message{{ num_messages|pluralize }}",
                   [{num_messages, 2}]),
    "You have 0 walruses" =
            render("You have {{ num_walruses }} walrus{{ num_walruses|pluralize:'es' }}",
                   [{num_walruses, 0}]),
    "You have 1 walrus" =
            render("You have {{ num_walruses }} walrus{{ num_walruses|pluralize:'es' }}",
                   [{num_walruses, 1}]),
    "You have 2 walruses" =
            render("You have {{ num_walruses }} walrus{{ num_walruses|pluralize:'es' }}",
                   [{num_walruses, 2}]),
    "You have 0 cherries" =
            render("You have {{ num_cherries }} cherr{{ num_cherries|pluralize:'y,ies' }}",
                   [{num_cherries, 0}]),
    "You have 1 cherry" =
            render("You have {{ num_cherries }} cherr{{ num_cherries|pluralize:'y,ies' }}",
                   [{num_cherries, 1}]),
    "You have 2 cherries" =
            render("You have {{ num_cherries }} cherr{{ num_cherries|pluralize:'y,ies' }}",
                   [{num_cherries, 2}]),
    ok.

filter_pprint_test() ->
    "hello_atom" = render("{{ t|pprint }}", [{t, hello_atom}]),
    % Note that 2-tuples that get passed into etcher:render can get corrupted.
    % It's assumed that the tuple represents a {key, value} pair and as a 
    % part of the normalization process the key is turned into a string().
    "{\"hello\",tuple}" = render("{{ t|pprint|safe }}", [{t, {hello, tuple}}]),
    ok.

filter_random_test() ->
    AZ = lists:seq($A, $Z),
    AZStrs = [integer_to_list(I) || I <- AZ],
    S = render("{{ letters|random }}", [{letters, AZ}]),
    true = lists:member(S, AZStrs),
    "97" = render("{{ 'a'|random }}"),
    "" = render("{{ ''|random }}"),
    "" = render("{{ letters|random }}"),
    "" = render("{{ letters|random }}", [{letters, []}]),
    ok.

filter_removetags_test() ->
    "Joel <button>is</button> a slug" =
            render("{{ value|removetags:\"b span\"|safe }}", 
                   [{value, "<b>Joel</b> <button>is</button> a <span>slug</span>"}]),
    "</p><p><p>aaa   <p>" =
            render("{{ value|removetags:'b'|safe }}", 
                   [{value, "</b></p><p><p><b>aaa<b><b></b> </b> </b></b></b></b> <p>"}]),
    "bb" = render("{{ 'b<b >b'|removetags:'b'|safe }}"), 
    "" = render("{{ '<b style=\\'display: block\\'>'|removetags:'b'|safe }}"), 
    "<p style='display: block'>" = 
            render("{{ '<p style=\\'display: block\\'>'|removetags:'b'|safe }}"), 
    "<bad/>" = render("{{ '<bad/>'|removetags:'b'|safe }}"), 
    "< b>" = render("{{ '< b>'|removetags:'b'|safe }}"), 
    ok.

% ljust tests in reverse
filter_rjust_test() ->
    "x        aax" = render("x{{ 'aa'|rjust:10 }}x"),
    "xaaaaax" = render("x{{ 'aaaaa'|rjust:2 }}x"),
    "x  x" = render("x{{ s|rjust:2 }}x"),
    ok.

% Safe is tested properly in test_etcher_core:auto_escape_test/0
filter_safe_test() ->
    "<a> & <a>" = render("{{ s|safe }}", [{s, "<a> & <a>"}]),
    ok.

filter_safeseq_test() ->
    "<a>_<b>_<c>" = render("{{ items|safeseq|join:'_' }}", 
                         [{items, ["<a>", "<b>", "<c>"]}]),
    ok.

% Same tests as for 'first' filter
filter_sfirst_test() ->
    "a" = render("{{ 'abc'|sfirst }}"),
    "b" = render("{{ 'b\x{400}cd'|sfirst }}"),
    "\x{400}" = render("{{ '\x{400}cde'|sfirst }}"),
    "" = render("{{ items|sfirst }}", [{items, ["dd", "ee", "ff"]}]),
    "" = render("{{ items|sfirst }}", [{items, []}]),
    "" = render("{{ items|sfirst }}"),
    ok.

% Same tests as for 'last' filter
filter_slast_test() ->
    "a" = render("{{ 'cba'|slast }}"),
    "b" = render("{{ 'dc\x{400}b'|slast }}"),
    "\x{400}" = render("{{ 'edc\x{400}'|slast }}"),
    "" = render("{{ items|slast }}", [{items, ["ff", "ee", "dd"]}]),
    "" = render("{{ items|slast }}", [{items, []}]),
    "" = render("{{ items|slast }}"),
    ok.

% Test cases borrowed from 'dive into python' slicing examples:
% http://diveintopython.org/native_data_types/lists.html#odbchelper.list.slice
filter_slice_test() ->
    Li = ["a", "b", "mpilgrim", "z", "example"],
    "b,mpilgrim" = render("{{ items|slice:'1:3'|join:',' }}", [{items, Li}]),
    "b,mpilgrim,z" = render("{{ items|slice:'1:-1'|join:',' }}", [{items, Li}]),
    "a,b,mpilgrim" = render("{{ items|slice:'0:3'|join:',' }}", [{items, Li}]),
    "a,b,mpilgrim" = render("{{ items|slice:':3'|join:',' }}", [{items, Li}]),
    "z,example" = render("{{ items|slice:'3:'|join:',' }}", [{items, Li}]),
    "a,b,mpilgrim,z,example" = render("{{ items|slice:':'|join:',' }}", [{items, Li}]),
    "a,b,mpilgrim" = render("{{ items|slice:'-20:3'|join:',' }}", [{items, Li}]),
    "" = render("{{ items|slice:'-20:-20'|join:',' }}", [{items, Li}]),
    "" = render("{{ items|slice:'20:-20'|join:',' }}", [{items, Li}]),
    "" = render("{{ items|slice:'20:20'|join:',' }}", [{items, Li}]),
    ok.

% filter_slugify/2,

filter_slugify_test() ->
    "joel-is-a-slug" =  render("{{ 'Joel is a slug'|slugify }}"),
    "a-a-aa" =  render("{{ 'a  -  ..A----Aa'|slugify }}"),
    "-b-" =  render("{{ '  b   '|slugify }}"),
    "cc" =  render("{{ 'c.;@<>()*&%$c'|slugify }}"),
    ok.

% Based on tests for 'random' filter
filter_srandom_test() ->
    AZ = lists:seq($A, $Z),
    AZStrs = [[Char] || Char <- AZ],
    S = render("{{ letters|srandom }}", [{letters, AZ}]),
    true = lists:member(S, AZStrs),
    "a" = render("{{ 'a'|srandom }}"),
    "" = render("{{ ''|srandom }}"),
    "" = render("{{ letters|srandom }}"),
    "" = render("{{ letters|srandom }}", [{letters, []}]),
    ok.

% TODO
% filter_stringformat/3

filter_striptags_test() ->
    "Joel is a slug" = 
            render("{{ s|striptags }}",
                   [{s, "<b>Joel</b> <button>is</button> a <span>slug</span>"}]),
    "aa" = render("{{ 'a< hah - this aint even a tag >a'|striptags }}"),
    "b<b" = render("{{ 'b<b'|striptags }}"),
    "c>c" = render("{{ 'c>c'|striptags }}"),
    ok.

% TODO
% filter_time/2,
% filter_time/3,

filter_timesince_test() ->
    Y2K = {2000,1,1},
    Zero = {0,0,0},
    Millenium = {Y2K, Zero},
    "0 Minutes" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, Millenium}]),
    "0 Minutes" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {{1999,1,1},Zero}}]),
    "0 Minutes" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {Y2K,{0,0,59}}}]),
    "1 Minute" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {Y2K,{0,1,0}}}]),
    "2 Minutes" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {Y2K,{0,2,0}}}]),
    "1 Hour, 5 Minutes" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {Y2K,{1,5,0}}}]),
    "2 Hours" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {Y2K,{2,0,0}}}]),
    "1 Day, 2 Hours" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {{2000,1,2},{2,0,0}}}]),
    "5 Days" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {{2000,1,6},{0,33,0}}}]),
    "1 Month" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {{2000,2,3},{0,33,0}}}]),
    "1 Year, 1 Month" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {{2001,2,3},{0,33,0}}}]),
    "5 Years" = 
            render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, {{2005,1,9},{55,33,0}}}]),
    "" = render("{{ millenium|timesince:to_date }}", 
                   [{millenium, Millenium}, {to_date, "Bad Date"}]),
    "" = render("{{ millenium|timesince:to_date }}", 
                   [{millenium, "Bad Date"}, {to_date, Millenium}]),
    % Test now() format dates
    T1 = {1264,334197,193493},
    T2 = {1264,375364,716523},
    "11 Hours, 26 Minutes" = 
            render("{{ t1|timesince:t2 }}", [{t1, T1}, {t2, T2}]),
    ok.

filter_timeuntil_test() ->
    % Uses the same logic as timesince - so it would be redundant to repeat 
    % all those tests. 
    Millenium = {{2000,1,1},{0,0,0}},
    "1 Year, 1 Month" = 
            render("{{ to_date|timeuntil:millenium }}", 
                   [{millenium, Millenium}, {to_date, {{2001,2,3},{0,33,0}}}]),
    "5 Years" = 
            render("{{ to_date|timeuntil:millenium }}", 
                   [{millenium, Millenium}, {to_date, {{2005,1,9},{55,33,0}}}]),
    % Test now() format dates
    T1 = {1264,334197,193493},
    T2 = {1264,375364,716523},
    "11 Hours, 26 Minutes" = 
            render("{{ t2|timeuntil:t1 }}", [{t1, T1}, {t2, T2}]),
    ok.

filter_title_test() ->
    "A Really Exciting Blog Post" = 
            render("{{ 'a really eXciting Blog post'|title }}"),
    "I'M All For De-Emphasisation, But This Is Crazy!" = 
            render("{{ 'I\\'M ALL FOR DE-EMPHASISATION, BUT THIS IS CRAZY!'|title }}"),
    "\"Stop Yer Grinnin' & ...\"" = 
            render("{{ '\"Stop yer grinnin\\' & ...\"'|title }}"),
    "Aa-Aa--Aa---A_A-" = 
            render("{{ 'aa-aa--aa---a_a-'|title }}"),
    ok.

filter_truncatewords_test() ->
    "One ..." = render("{{ s|truncatewords:1 }}", [{s, "One Two Three"}]),
    "One Two ..." = render("{{ s|truncatewords:2 }}", [{s, "One Two Three"}]),
    "One Two Three" = render("{{ s|truncatewords:3 }}", [{s, "One Two Three"}]),
    "" = render("{{ s|truncatewords:0 }}", [{s, "One Two Three"}]),
    "One Two ..." = render("{{ s|truncatewords:-1 }}", [{s, "One Two Three"}]),
    "One ..." = render("{{ s|truncatewords:-2 }}", [{s, "One Two Three"}]),
    "" = render("{{ s|truncatewords:-3 }}", [{s, "One Two Three"}]),
    "" = render("{{ s|truncatewords:-4 }}", [{s, "One Two Three"}]),
    "" = render("{{ s|truncatewords:-6000 }}", [{s, "One Two Three"}]),
    "! ! ! ..." = render("{{ s|truncatewords:3 }}", [{s, "! ! ! ! !"}]),
    ok.

filter_truncatewords_html_test() ->
    Html = "<p>One</p><p>Two</p><p>Three</p><p>Four</p>",
    NestedHtml = "<p>One<em>Two<strong>Three"
            "<span class='bling_bling'>Four</span></strong></em></p>",
    "<p>One ...</p>" = 
            render("{{ s|safe|truncatewords_html:1 }}", [{s, Html}]),
    "<p>One</p><p>Two ...</p>" = 
            render("{{ s|safe|truncatewords_html:2 }}", [{s, Html}]),
    "<p>One</p><p>Two</p><p>Three ...</p>" = 
            render("{{ s|safe|truncatewords_html:3 }}", [{s, Html}]),
    Html = render("{{ s|safe|truncatewords_html:4 }}", [{s, Html}]),
    Html = render("{{ s|safe|truncatewords_html:100 }}", [{s, Html}]),
    "" = render("{{ s|safe|truncatewords_html:0 }}", [{s, Html}]),
    "" = render("{{ s|safe|truncatewords_html:-1 }}", [{s, Html}]),
    "<p>One ...</p>" = 
            render("{{ s|safe|truncatewords_html:1 }}", [{s, NestedHtml}]),
    "<p>One<em>Two ...</em></p>" = 
            render("{{ s|safe|truncatewords_html:2 }}", [{s, NestedHtml}]),
    "<p>One<em>Two<strong>Three ...</strong></em></p>" = 
            render("{{ s|safe|truncatewords_html:3 }}", [{s, NestedHtml}]),
    NestedHtml = render("{{ s|safe|truncatewords_html:4 }}", [{s, NestedHtml}]),
    NestedHtml = render("{{ s|safe|truncatewords_html:100 }}", [{s, NestedHtml}]),
    ok.

% TODO 
% filter_udate/2,
% filter_udate/3,

filter_unordered_list_test() ->
    Expected =	"	<li>States\n"
                "	<ul>\n"
                "		<li>Kansas\n"
                "		<ul>\n"
                "			<li>Lawrence</li>\n"
                "			<li>Topeka</li>\n"
                "		</ul>\n"
                "		</li>\n"
                "		<li>Illinois</li>\n"
                "	</ul>\n"
                "	</li>\n",
    Expected = 
            render("{{ states|unordered_list }}",
                   [{states, ["States", ["Kansas", ["Lawrence", "Topeka"], "Illinois"]]}]),
    ok.

% Reverse of 'upper' filter tests
filter_upper_test() ->
    "AAA" = render("{{ 'aAa'|upper }}"),
    "\x{400}BB\x{400}BB" = render("{{ s|upper }}", [{s, "\x{400}Bb\x{400}bB"}]),
    ok.

% Note that urlencode does not do plus encoding of URL parameter arguments.
% Django's version doesn't, and we track their implementation.
filter_urlencode_test() ->
    "http%3A//www.example.org/foo%3Fa%3Db%26c%3Dd%26u%3D%D0%80%26e%3Df%20f" =
            render("{{ url|urlencode }}",
                   [{url, "http://www.example.org/foo?a=b&c=d&u=\x{400}&e=f f"}]),
    ok.

filter_urlize_test() ->
    "Check out "
        "<a href=\"http://www.djangoproject.com\" rel=\"nofollow\">"
            "www.djangoproject.com"
        "</a>" = 
            render("{{ s|urlize }}",
                   [{s, "Check out www.djangoproject.com"}]),
    "Ping me on <a href=\"mailto:jabba@example.com\">jabba@example.com</a>" =
            render("{{ s|urlize }}",
                   [{s, "Ping me on jabba@example.com"}]),
    "<a href=\"http://example.com\" rel=\"nofollow\">http://example.com</a>" = 
            render("{{ s|urlize }}",
                   [{s, "http://example.com"}]),
    "<a href=\"http://example.com\" rel=\"nofollow\">example.com</a>" = 
            render("{{ s|urlize }}",
                   [{s, "example.com"}]),
    "<a href=\"http://example.local\" rel=\"nofollow\">http://example.local</a>" = 
            render("{{ s|urlize }}",
                   [{s, "http://example.local"}]),
    "example.local" = 
            render("{{ s|urlize|safe }}",
                   [{s, "example.local"}]),
    "example" = 
            render("{{ s|urlize|safe }}",
                   [{s, "example"}]),
    ok.

filter_urlizetrunc_test() ->
    "<a href=\"http://example.com\" rel=\"nofollow\">examp...</a>" = 
            render("{{ s|urlizetrunc:8 }}", [{s, "example.com"}]),
    "<a href=\"http://example.com\" rel=\"nofollow\">e...</a>" = 
            render("{{ s|urlizetrunc:4 }}", [{s, "example.com"}]),
    "<a href=\"http://example.com\" rel=\"nofollow\">...</a>" = 
            render("{{ s|urlizetrunc:1 }}", [{s, "example.com"}]),
    "<a href=\"http://example.com\" rel=\"nofollow\">...</a>" = 
            render("{{ s|urlizetrunc:-444 }}", [{s, "example.com"}]),
    "<a href=\"http://example.com\" rel=\"nofollow\">example.com</a>" = 
            render("{{ s|urlizetrunc:11 }}", [{s, "example.com"}]),
    "<a href=\"http://example.com\" rel=\"nofollow\">example...</a>" = 
            render("{{ s|urlizetrunc:10 }}", [{s, "example.com"}]),
    "<a href=\"http://example.com\" rel=\"nofollow\">example.com</a>" = 
            render("{{ s|urlizetrunc:444 }}", [{s, "example.com"}]),
    ok.

filter_wordcount_test() ->
    "4" = render("{{ 'Joel is a slug'|wordcount }}"),
    "0" = render("{{ ''|wordcount }}"),
    "4" = render("{{ '  .  .  .  .  '|wordcount }}"),
    ok.

filter_wordwrap_test() ->
    "A a a a a\nA a" = render("{{ s|wordwrap:10|safe }}", [{s, "A a a a a A a"}]),
    "A a a a a\nA a" = render("{{ s|wordwrap:9|safe }}", [{s, "A a a a a A a"}]),
    "A a a a\na A a" = render("{{ s|wordwrap:8|safe }}", [{s, "A a a a a A a"}]),
    Orig = "If the milk turns out to be sour, I ain't the kinda pussy to drink it.",
    Expected = 
        "If the\n"
        "milk turns\n"
        "out to be\n"
        "sour, I\n"
        "ain't the\n"
        "kinda\n"
        "pussy to\n"
        "drink it.",
    Expected = render("{{ s|wordwrap:10|safe }}", [{s, Orig}]),
    Email = 
        "Dear Java Coder,\n"
        "\n"
        "It would be better if you indented your code in the same way as Sun does. Everyone pretty much uses this style, and it's sooo much easier to grok other peoples code if your style is the same as theirs.\n"
        "\n"
        "So, I'd change it to read:\n"
        "\n"
        "       public static void main(String[] args) {\n"
        "		System.out.println(\"Hello, world!\");\n"
        "       }\n"
        "\n"
        "Just my 2c!\n",
    WrappedEmail = 
        "Dear Java Coder,\n"
        "\n"
        "It would be better if you indented your code in the same way as Sun\n"
        "does. Everyone pretty much uses this style, and it's sooo much easier\n"
        "to grok other peoples code if your style is the same as theirs.\n"
        "\n"
        "So, I'd change it to read:\n"
        "\n"
        "       public static void main(String[] args) {\n"
        "		System.out.println(\"Hello, world!\");\n"
        "       }\n"
        "\n"
        "Just my 2c!\n",
    WrappedEmail = render("{{ email|wordwrap:70|safe }}", [{email, Email}]),
    ok.

filter_yesno_test() ->
    "yes" = render("{{ 200|yesno }}"),
    "no" = render("{{ ''|yesno }}"),
    "no" = render("{{ woof|yesno }}"),
    "maybe" = render("{{ woof|yesno }}", [{woof, undefined}]),
    "waahey!" = render("{{ woof|yesno:'waahey!,nay!' }}", [{woof, "Boo"}]),
    "nay!" = render("{{ woof|yesno:'waahey!,nay!' }}", [{woof, 0}]),
    "nay!" = render("{{ woof|yesno:'waahey!,nay!' }}", [{woof, undefined}]),
    "jury's out!" = render("{{ woof|yesno:'waahey!,nay!,jury\\'s out!'|safe }}", [{woof, undefined}]),
    ok.

%%-------------------------------------------------------------------
%% Misc.
%%-------------------------------------------------------------------

stringify(T) ->
    lists:flatten(io_lib:format("~p", [T])).

render(Text) ->
    render(Text, []).

render(Text, Context) ->
    render(Text, Context, []).

render(Text, Context, Opts) ->
    {ok, Template} = etcher:compile(Text),
    etcher:render(Template, Context, [{return, list} | Opts]).

