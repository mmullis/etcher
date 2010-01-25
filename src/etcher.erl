%%%-------------------------------------------------------------------
%%% File    : etcher.erl
%%% Project : Etcher (http://github.com/jinsky/etcher)
%%% Author  : Rory Byrne <rory [at] jinsky [dot] com>
%%% License : BSD
%%%
%%% Copyright (c) 2010 Rory Byrne
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   * Redistributions of source code must retain the above copyright
%%%     notice, this list of conditions and the following disclaimer.
%%%
%%%   * Redistributions in binary form must reproduce the above
%%%     copyright notice, this list of conditions and the following
%%%     disclaimer in the documentation and/or other materials provided
%%%     with the distribution.
%%%
%%%   * Neither the names of the copyright holders, nor the names of its
%%%     contributors may be used to endorse or promote products derived
%%%     from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-module(etcher).
-export([compile/1,
         compile/2,
         add_record_defs/1,
         % add_record_defs/2,
         render/2,
         render/3
         ]).

-include("internal.hrl").

%% @type template() = term().
%%       You can treat a template as an opaque object. If you are interested 
%%       in what it contains, take a look at the <code>include/api.hrl</code>
%%       file. Note that each template is unique, so even if you compile the 
%%       same code twice, it will result in two different templates.
%% @type chardata() = charlist() | unicode_binary().
%% @type charlist() = [unicode_char() | unicode_binary() | charlist()].
%%       A unicode_binary is allowed as the tail of the list.
%% @type custom_filter() = #filter_def{}.
%%       A custom_filter() is a record of type <code>filter_def</code>. You
%%        can find the record definition of <code>filter_def</code> in
%%        <code>include/api.hrl</code>.
%% @type custom_tag() = #tag_def{}.
%%       A custom_tag() is a record of type <code>tag_def</code>. You can 
%%       find the record definition of <code>tag_def</code> in 
%%       <code>include/api.hrl</code>.
%% @type context() = term().
%%       At it's top-most level, a context is always a proplist, but below 
%%       that it can contain nested records, lists and proplists. For example,
%%       this is what a context for a blog page might look like:
%%       <pre>
%%        [{blog_title, "Freds Blog"},
%%         {post, #blog_post{
%%                    title="Getting Ahead by Failing",
%%                    content="And another thing ...",
%%                    comments=[
%%                        #comment{
%%                            author="Sam",
%%                            email="sam@example.com",
%%                            says="You said ..."},
%%                        #comment{
%%                            author="Bob",
%%                            email="bob@example.com",
%%                            says="Always the ..."}]}},
%%         {page_hits, 8143}]
%%       </pre>
%%       If you are using nested records, as this example does, you will need to
%%       inform the <code>etcher</code> application about these records in 
%%       advance so it can recognise them and auto-expand them. You tell 
%%       <code>etcher</code> about record definitions using 
%%       {@link add_record_defs/1} below.
%% @end

%% @spec compile(Source::Source) -> {ok, template()}
%%       Source = chardata()
%% @doc
%% Calls <code>compile(Source, [])</code>.
%% @end
compile(Source) ->
    compile(Source, []).

%% @spec compile(Source::Source, Options::Options) -> {ok, template()}
%%       Source = chardata()
%%       Options = [ Option ]
%%       Option = {filters, [custom_filter()]} | {tags, [custom_tag()]} 
%% @doc
%% Compiles unicode/UTF-8-encoded template source code into a 
%% template. You can do what you want with this term - store it 
%% in an mneisa database, call <code>erlang:term_to_binary/1</code>
%% on it and write it to a file.
%% @end
compile(Source, Options) when is_list(Options) ->
    case unicode:characters_to_list(Source) of
        Unicode when is_list(Unicode) ->
            {ok, Tokens} = etcher_scanner:scan(Unicode),
            {ok, TemplateContent} = etcher_parser:parse(Tokens, Options),
            Template = to_template_record(TemplateContent),
            {ok, Template};
        Err ->
            {error, {unicode, Err}}
    end.

%% @spec add_record_defs(RecDefs::RecDefs) -> ok
%%       RecDefs = filename() | [{record_name(), [record_field()]}]
%% @doc
%% Use this function to inform the <code>etcher</code> application
%% about records you would like to use in rendering contexts. 
%% 
%% The easiest way to inform <code>etcher</code> about your record 
%% defintions is to pass in the name of a <code>.hrl</code> file 
%% containing those definitions. 
%%
%% You can also pass in a list of {record_name(), [record_fields()]} 
%% if it's more convenient.
%%
%% Note that there is currently no way to get <code>etcher</code> to 
%% forget about record definitions it's already been informed about. 
%% This functionality will be added later.
%% @end
add_record_defs(T) ->
    etcher_rec_resolver:add_record_defs(T).

% TODO - re-enable this
% add_record_defs(T, Namespace) ->
%     etcher_rec_resolver:add_record_defs(T, Namespace).

%% @spec render(Template::Template, Context::Context) -> chardata()
%%       Template = template()
%%       Context = context()
%% @doc
%% Calls <code>render(Template, Context, [])</code>. 
%% @end
render(Template, Context) -> 
    render(Template, Context, []).

%% @spec render(Template::Template, Context::Context, Options::Options) -> RenderedContent
%%       Template = template()
%%       Context = context()
%%       Options = [ Option ]
%%       Option = {return, list} | {return, binary} | {auto_escape, bool()} | {url_mapper, function()}
%%       RenderedContent = chardata() | unicode_binary() | list()
%% @doc
%% Renders the Template using the Context you provide.
%%
%% The returned content defaults to chardata(), but using the return 
%% option you can get a binary() (<code>{return, binary}</code>) or a 
%% list() (<code>{return, list}</code>).
%%
%% Auto-escape is on by default, as it is with Django. You can use 
%% use the <code>{auto_escape, false}</code> option to turn it off.
%% You'll certainly want to do this if you are  generating 
%% non-XML/non-HTML documents, such as emails.
%%
%% The <code>url_mapper</code> option is specifically for use with
%% the <code>{% url ... %}</code> tag. Django has it's own way of 
%% using this tag which ties in with the rest of it's framework.
%% We obviously don't have a 'rest of a framework' so we'll have 
%% to do something a little different. If you pass in a 
%% <code>fun()</code> of Arity 1 as a <code>url_mapper</code>,
%% then when the <code>url</code> tag is used, it's parameters 
%% will be passed to your fun as a list(). The parameter list 
%% will contain either one, or two parameters. If the tag looks 
%% like <code>{% url required_param arg1,arg2,name1=value1 %}</code>,
%% then your UrlMapper fun() will receive 
%% <code>["required_param", "arg1,arg2,name1=value1"]</code>.
%% You are then responsible for returning a URL string. Note that 
%% no checking, or URL-encoding, will be done on the URL string 
%% you return. So, you need to make sure you get it right. There's
%% a couple of url encoding functions exported from 
%% {@link etcher_util} that you can use if you need to.
%% @end
render(_Template, Context, _Options) when not is_list(Context) ->
    throw({invalid_context_arg, list_expected});
render(_Template, _Context, Options) when not is_list(Options) ->
    throw({invalid_options_arg, list_expected});
render(#etcher_template{version=?CURRENT_TVER,
                        content=Parts},
       Context, 
       Options) ->
    {RetType, RenderOpts} = get_option(return, Options, chardata),
    RecResolver = etcher_rec_resolver:get_record_resolver(),
    RS = etcher_renderer:new(Context, RecResolver, RenderOpts),
    {_RS1, RenderedData} = etcher_renderer:render(RS, Parts),
    return_as(RetType, RenderedData);
render(#etcher_template{version=Ver}, _Context, _Options) ->
    Err = {template_version_mismatch, 
                {expected_version, ?CURRENT_TVER},
                {received_version, Ver}},
    throw(Err);
render(T, _Context, _Options) ->
    throw({template_not_recognised, T}).

%%------------------------------------------------------------------
%% Misc.
%%------------------------------------------------------------------

to_template_record(TemplateContent) ->
    Template = #etcher_template{},
    Version = Template#etcher_template.version,
    Timestamp = now(),
    Id = create_template_id(Version, Timestamp, TemplateContent),
    Template#etcher_template{
                id = Id,
                created = Timestamp,
                content = TemplateContent}.

create_template_id({_,_} = Version, 
                   {_,_,_} = Timestamp, 
                   TemplateContent) 
                        when is_list(TemplateContent) ->
    T = {Version, Timestamp, TemplateContent},
    erlang:md5(term_to_binary(T)).

get_option(Name, List, Default) ->
    extract(List, Name, Default, []).

extract([{Name, Value} | Rest], Name, _Current, Acc) ->
    extract(Rest, Name, Value, Acc);
extract([T | Rest], Name, Current, Acc) ->
    extract(Rest, Name, Current, [T | Acc]);
extract([], _Name, Current, Acc) ->
    {Current, lists:reverse(Acc)}.
 
return_as(chardata, CharData) ->
    CharData;
return_as(binary, CharData) ->
    unicode:characters_to_binary(CharData);
return_as(list, CharData) ->
    unicode:characters_to_list(CharData).

