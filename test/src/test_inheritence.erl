
-module(test_inheritence).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------

override_test() ->
    SrcBase = "{% block content %}in base{% endblock %}",
    SrcMid = "{% extends base_tpl %}{% block content %}in mid{% endblock %}",
    SrcMain = "{% extends mid_tpl %}{% block content %}in main{% endblock %}",
    [TplBase, TplMid, TplMain] = compile_all([SrcBase, SrcMid, SrcMain]),
    "in main" = render(TplMain, [{base_tpl, TplBase}, {mid_tpl, TplMid}]),
    ok.

cascade_test() ->
    SrcBase = "{% block menu %}base menu{% endblock %} "
                "{% block content %}base content{% endblock %} "
                "{% block footer %}base footer{% endblock %}",
    SrcMid = "{% extends base_tpl %}"
                "{% block menu %}mid menu{% endblock %}",
    SrcMain = "{% extends mid_tpl %}"
                "{% block content %}main content{% endblock %}",
    [TplBase, TplMid, TplMain] = compile_all([SrcBase, SrcMid, SrcMain]),
    "mid menu main content base footer" = 
            render(TplMain, [{base_tpl, TplBase}, {mid_tpl, TplMid}]),
    ok.

prologue_test() ->
    SrcBase = "base prologue {% block content %}{% endblock %} base epilogue",
    SrcMid = "mid prologue {% extends base_tpl %} mid epilogue",
    SrcMain = "main prologue {% extends mid_tpl %}"
                "{% block content %}yeehaa!{% endblock %} main epilogue",
    [TplBase, TplMid, TplMain] = compile_all([SrcBase, SrcMid, SrcMain]),
    "main prologue mid prologue base prologue yeehaa! base epilogue" =
        render(TplMain, [{base_tpl, TplBase}, {mid_tpl, TplMid}]),
    ok.

super_1_hop_test() ->
    SrcBase = "{% block content %}in base{% endblock %}",
    SrcMid = "{% extends base_tpl %}{% block content %}in mid{% endblock %}",
    SrcMain = "{% extends mid_tpl %}{% block content %}" 
                "in main {{ block.super }} out main{% endblock %}",
    [TplBase, TplMid, TplMain] = compile_all([SrcBase, SrcMid, SrcMain]),
    "in main in mid out main" =
        render(TplMain, [{base_tpl, TplBase}, {mid_tpl, TplMid}]),
    ok.

super_2_hops_test() ->
    SrcBase = "{% block content %}in base{% endblock %}",
    SrcMid = "{% extends base_tpl %}{% block content %}"
                "in mid {{ block.super }} out mid{% endblock %}",
    SrcMain = "{% extends mid_tpl %}{% block content %}" 
                "in main {{ block.super }} out main{% endblock %}",
    [TplBase, TplMid, TplMain] = compile_all([SrcBase, SrcMid, SrcMain]),
    "in main in mid in base out mid out main" =
        render(TplMain, [{base_tpl, TplBase}, {mid_tpl, TplMid}]),
    ok.

super_multi_call_test() ->
    SrcBase = "{% block content %}- in base -{% endblock %}",
    SrcMid = "{% extends base_tpl %}{% block content %}"
                "{{ block.super }}- in mid -{{ block.super }}{% endblock %}",
    SrcMain = "{% extends mid_tpl %}{% block content %}" 
                "in main -{{ block.super }}- out main{% endblock %}",
    [TplBase, TplMid, TplMain] = compile_all([SrcBase, SrcMid, SrcMain]),
    "in main -- in base -- in mid -- in base -- out main" =
        render(TplMain, [{base_tpl, TplBase}, {mid_tpl, TplMid}]),
    ok.

no_self_extend_test() ->
    SrcMain = "{% extends self %}{% block content %}in main{% endblock %}",
    [TplMain1, TplMain2] = compile_all([SrcMain, SrcMain]),
    ok = 
        try render(TplMain1, [{self, TplMain2}]) of
            _ ->
                die
        catch
            throw:{render_loop, _} ->
                ok
        end,
    ok.

%%-------------------------------------------------------------------
%% Misc.
%%-------------------------------------------------------------------

render(Template, Context) ->
    etcher:render(Template, Context, [{return, list}]).

compile_all(L) ->
    [begin {ok, Tpl} = etcher:compile(Src), Tpl end || Src <- L].

