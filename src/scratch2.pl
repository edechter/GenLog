

:- use_module(sdcl).
:- use_module(compile).

:- use_module(learn).
:- use_module(pgen).
:- use_module(pprint).
:- [data_utils].
:- ['number_words.pl'].
        
        
              

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
        findall(X-1,
                (between(1, 10, N),
                 count_sentence(1, N, X)),
                Xs).
        
goal(Goal) :-
        number_sentences1(Xs),
        member(X-C, Xs),
        atomic_list_concat(Ws, ' ', X),
        T = hear(Ws-[]),
        Goal = count(T,C).
                      
goals(Goals) :-
        findall(G, goal(G),
                 Goals).
        
go :-
        compile_sdcl_file('test.gl'),
        Options = [beam_width(100), time_limit_seconds(1)],
        set_rule_alphas(uniform),
        goals(Goals),
        list_to_random_choice(Goals, GoalGen),
        % sentence_data_set(hear, Xs, Goals),
        writeln(GoalGen),
        run_online_vbem(GoalGen, Data, Options),        
        writeln(Data).
  