

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- getenv('GENLOG_DIR', Dir),
   asserta(file_search_path(genlog, Dir));
   true.
        
%% ----------------------------------------------------------------------

:- use_module(library(lists)).

:- use_module(genlog(experiment)).

:- use_module(genlog(gl_rule)).
:- use_module(genlog(sdcl)).
:- use_module(genlog(compile)).

:- use_module(genlog(learn)).
:- use_module(genlog(pgen)).
:- use_module(genlog(pprint)).

:- use_module(genlog(data_utils)).
:- use_module(genlog(number_words)).

:- [number_syllables].
:- [consonants].
:- [vowels].

%% this file
:- absolute_file_name('./syllables.pl', Abs),
   set_setting(experiment:runner, Abs).

%% experiment data directory
:- set_setting(experiment:root, '../../data').


%% ------------------------------------------
%% make number data
number_goal(N, Count, Goal) :-
        number_phones(N, Phones),
        Goal = count(hear(Phones-[]), Count).

number_goals(Lo, Hi, Count, Goals) :-
        findall(Goal,
                (between(Lo, Hi, N),
                 number_goal(N, Count, Goal)),
                Goals).

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

number_phone_lexicon(Phones) :-
        findall(P,
                (between(1, 99, N),
                 number_phones(N, Xs),
                 member(P, Xs)),
                Ps),
        sort(Ps, Phones).

number_phone(Phone) :-
        number_phone_lexicon(Phones),
        member(Phone, Phones).

power_law_goals(A, GoalWeights) :-
        number_goals(1, 100, 1, Gs),
        enum(Gs, IGs),
        findall(G-W,
                (member(I-G, IGs),
                 W is A/I),
                GoalWeights).        

gl_file('../../gls/number_morph.gl').
        
main(Options) :-
        gl_file(GlFile),
        compile_sdcl_file(GlFile),
        Options0 = [beam_width(50), time_limit_seconds(10)],
        merge_options(Options, Options0, Options1),
        set_rule_alphas(uniform),
        power_law_goals(100, GoalWeights),
        list_to_categorical(GoalWeights, GoalGen),
        run_online_vbem(GoalGen, Data, Options1).

%% ----------------------------------------------------------------------
%%    analyze
analyze1(GlFile, Ls) :-
        load_gl(GlFile),
        number_goals(1, 20, 1, Goals),
        prove_goals(Goals, Ds, [beam_width(50), time_limit_seconds(10)]), 
        findall(L,
                (member(D, Ds), 
                 loglikelihood(D, L)),
                Ls).

analyze(Dir, LoglikelihoodData) :-
        absolute_file_name(Dir, Path),
        directory_files(Path, Files),
        findall(F, 
                (member(F, Files),
                 atom_prefix(F, 'ovbem_gl'),
                 file_name_extension(_, 'gl', F)),
                Files1),
        writeln(Files1),
        retractall(worked(_)),
        findall(Ls,
                (member(F, Files1),
                 directory_file_path(Path, F, P),
                 (analyze1(P, Ls) ->
                  assert(worked(P))
                 ;
                  throw(error('FAIL'))
                 )
                ),
                LoglikelihoodData),
        length(LoglikelihoodData, N).
        
%% ----------------------------------------------------------------------
consonant(C) :-
        consonants(Cs),
        member(C, Cs).

vowel(V) :-
        vowels(Vs),
        member(V, Vs).