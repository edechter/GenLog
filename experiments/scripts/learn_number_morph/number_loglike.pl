#!/usr/bin/env swipl

%% ----------------------------------------------------------------------
%% number_loglike.pl
%% Author: Eyal Dechter
%%
%% Compute the loglikelihood of goals corresponding to numbers for a
%% given gl file.
%%
%% number_loglike <result_file> <gl_file> <number> ... <number>
%%
%% The result is written as JSON to the result file in the form
%% [
%%  {"number" : 1,
%%   "loglikelihood" : -100.42},
%%  ...
%% ]
%% ----------------------------------------------------------------------

:- module(number_loglike, [main/0]).

:- initialization main.

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', src], Src),
   asserta(user:file_search_path(genlog, Src));
   true.

:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/experiments/scripts/learn_number_morph/'], Exp),
   asserta(user:file_search_path(genlog, Exp));
   true.

%% ----------------------------------------------------------------------
%% Constants
beam_width(50).
time_limit_seconds(10).

%% ----------------------------------------------------------------------
:- use_module(library(lists)).
:- use_module(library(http/json)).

:- use_module(genlog(experiment)).
:- use_module(genlog(gl_rule)).
:- use_module(genlog(sdcl)).
:- use_module(genlog(compile)).

:- use_module(genlog(learn)).
:- use_module(genlog(pgen)).
:- use_module(genlog(pprint)).

:- use_module(genlog(data_utils)).
:- use_module(genlog(number_words)).

:- ensure_loaded(genlog(number_syllables)).
:- ensure_loaded(genlog(consonants)).
:- ensure_loaded(genlog(vowels)).


%% ------------------------------------------
%% make number data
number_goal(N, Goal) :-
        number_phones(N, Phones),
        Goal = hear(Phones-[]).

number_phones(N, Phones) :-
       number_syllables(N, Syls), % Syls is list of words, each one is list of triples
       flatten(Syls, Syls1),      % Syls1 is a list of triples
       findall(P,
               (
                member(Trip, Syls1),
                (Trip = (P, _, _);
                 Trip = (_, P, _);
                 Trip = (_, _, P))
               ),
               Phones).


run :-
        current_prolog_flag(argv, [ResultFile, GlFile | NumberAtoms]),
        maplist(atom_number, NumberAtoms, Numbers),
        open(ResultFile, write, ResultStream, []),
        run(ResultStream, GlFile, Numbers),
        close(ResultStream).
        
run(ResultStream, GlFile, Numbers) :-
        (\+ exists_file(GlFile) ->
         throw(error('number_loglike.pl: Cannot find GenLog file.'))
        ;
         load_gl(GlFile),
         findall(N-Goal,
                 (member(N, Numbers), 
                  number_goal(N, Goal)),
                 NGoals),
         write(ResultStream, '['),
         forall(member(N-Goal, NGoals),
                (
                 beam_width(BeamWidth),
                 time_limit_seconds(TimeLimitSeconds),
                 prove_goals([Goal], [D], [beam_width(BeamWidth),
                                           time_limit_seconds(TimeLimitSeconds)]),
                 loglikelihood(D, L),
                 term_to_atom(D, D1),
                 Result=result{number:N,
                               gl_file:GlFile,
                               dsearch_results:D1,
                               loglikelihood:L},
                 json_write_dict(ResultStream, Result),
                 writeln(ResultStream, ', ')
                )
               ),
         write(ResultStream, ']')
        ).

main :-
        catch(run, E, (print_message(error, E), fail)),
        halt.
main :-
        halt(1).


        


