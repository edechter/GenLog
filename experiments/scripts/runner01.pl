

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- getenv('GENLOG_ROOT', Dir),
   asserta(file_search_path(genlog, Dir));
   true.
   
        
%% ----------------------------------------------------------------------

:- use_module(genlog(experiment)).

:- use_module(genlog(sdcl)).
:- use_module(genlog(compile)).

:- use_module(genlog(learn)).
:- use_module(genlog(pgen)).
:- use_module(genlog(pprint)).

:- use_module(genlog(data_utils)).
:- use_module(genlog(number_words)).



%% this file
:- absolute_file_name('./runner01.pl', Abs),
   set_setting(experiment:runner, Abs).

%% experiment data directory
:- set_setting(experiment:root, '../data').


number_sentences(Xs) :-
        Xs = [
              'one' - 100,
              'one two' - 100,
              'one two three' - 100,
              'one two three four' - 100,
              'one two three four five' - 100,
              'one two three four five six' - 100,
              'one two three four five six seven' - 100,
              'one two three four five six seven eight nine ten eleven' - 100
              ].

count_sentence(Lo, Hi, X) :-
        findall(W,
                (between(Lo, Hi, N),
                 number_word(N, L),
                 atomic_list_concat(L, ' ', W)
                 ),
                Ws),
        atomic_list_concat(Ws, ' ', X).

number_sentences1(Xs) :-
        findall(X-W,
                (member(N, [1,2,3,4,5]),
                 count_sentence(1, N, X),
                 W is 1),
                Xs).
        
goal(Goal) :-
        number_sentences1(Xs),
        member(X-C, Xs),
        atomic_list_concat(Ws, ' ', X),
        append(Ws, Z, Ws1), 
        T = hear(Ws1-[]),
        Goal = count(T,1)-C.
                      
goals(Goals) :-
        findall(G, goal(G),
                 Goals).
        
main(Options) :-
        % experiment:setup_experiment,
        compile_sdcl_file('../gls/test.gl'),
        Options1 = [beam_width(10), time_limit_seconds(4)],
        merge_options(Options1, Options, Options2), 
        set_rule_alphas(uniform),
        goals(Goals),
        list_to_categorical(Goals, GoalGen),
        writeln(GoalGen),
        run_online_vbem(GoalGen, Data, Options2),        
        writeln(Data).
  

        