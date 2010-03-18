
%%%             doc_gen
%%%
%%% @author Joseph Lenton
%%% @doc
%%% This module is designed for handling generating the documentation for  the
%%% project. As more  packages are added, this should be expanded to include
%%% them.
%%%

-module( doc_gen ).
-author("Joseph Lenton").

-export([
        start_halt/2,
        start/0, start/2, start/3
]).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Public API
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

%%          start_halt
%%
%% @doc This will run the start function once, and then halt the Erlang VM afterwoulds.
%% @spec start_halt( Source::string(), Destination::string() ) -> ok
start_halt(Source, Destination) ->
    start(Source, Destination),
    halt().

start() ->
    start("./../src", "./../doc", public).

start(Source, Destination) ->
    start( Source, Destination, public ).

start(Source, Destination, public) ->
    generate_doc( Source, Destination, public_packages() ++ private_packages() );
start(Source, Destination, _Type) ->
    generate_doc( Source, Destination, public_packages() ++ private_packages() ).

%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%
%%%         Private - Document Generation
%%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%

public_packages() ->
    [
            "actor",
            "actor_state",
            "color",
            "controls",
            "display",
            "fps_actor",
            "graphics",
            "image",
            "mainloop",
            "world",
            "world_state"
    ].

private_packages() ->
    [
            "controls_handler",
            "data_server",
            "function_server",
            "doc_gen"
    ].

generate_doc(_Source, _Destination, _Packages) ->
    edoc:application(
            'ErlWorld',
            "./..",
            [
                    { exclude_packages, private_packages() }
            ]
    ),
    halt().

generate_doc2(Source, Destination, Packages) ->
    edoc:files(
            Packages,
            [
                    { souce_path    , Source        },
                    { souce_suffic  , ".erl"        },
                    { dir           , Destination   }
            ]
     ).
