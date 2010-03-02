
-module(test_template_loader).

-include_lib("eunit/include/eunit.hrl").

-define(RENDERED_BLOG_PAGE, 
        "[site header]\nThe Blog\n\nHow To Get Behind In Advertising\n\n[site footer]\n").

-define(RENDERED_NEWS_PAGE, 
        "[site header]\nThe News\n\nA New Decade - 2010!\n\n[site footer]\n").

%%-------------------------------------------------------------------
%% Tests
%%-------------------------------------------------------------------

load_inherited_source_templates_test() ->
    RenderOpts = render_opts(source_template_dir()),
    ?RENDERED_BLOG_PAGE = etcher:render(blog_page(), [], RenderOpts),
    ?RENDERED_NEWS_PAGE = etcher:render(news_page(), [], RenderOpts).

load_inherited_compiled_templates_test() ->
    RenderOpts = render_opts(compiled_template_dir()),
    ?RENDERED_BLOG_PAGE = etcher:render(compiled_blog_page(), [], RenderOpts),
    ?RENDERED_NEWS_PAGE = etcher:render(compiled_news_page(), [], RenderOpts).

load_missing_template_test() ->
    RenderOpts = render_opts(source_template_dir()),
    ok = 
        try etcher:render("no_such_template", [], RenderOpts) of
            _ ->
                die
        catch
            throw:{failed_to_load_template, _} ->
                ok
        end,
    ok.

load_without_template_loaders_test() ->
    ok = 
        try etcher:render(blog_page(), [], []) of
            _ ->
                die
        catch
            throw:{failed_to_load_template, _} ->
                ok
        end,
    ok.

%%-------------------------------------------------------------------
%% Misc.
%%-------------------------------------------------------------------

blog_page() ->
    filename:join(["blog", "items", "01.txt"]).

compiled_blog_page() ->
    blog_page() ++ ".eterm".

news_page() ->
    filename:join(["news", "items", "01.txt"]).

compiled_news_page() ->
    news_page() ++ ".eterm".

render_opts(TemplateDir) ->
    TemplateLoader = {file, [TemplateDir]},
    [{template_loaders, [TemplateLoader]}, {return, list}].

source_template_dir() ->
    asset_dir("source_template_dir").

compiled_template_dir() ->
    asset_dir("compiled_template_dir").

asset_dir(S) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "assets", S]).

