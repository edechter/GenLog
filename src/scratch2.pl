

:- use_module(sdcl).
:- use_module(compile).

:- use_module(learn).
:- [data_utils].
:- ['number_words.pl'].
        
        
              

number_sentences(Xs) :-
        Xs = [
              'one' - 100,
              'one two' - 100,
              'one two three' - 100,
              'one two three four' - 100,
              'one two three four five' - 100,
              'one two three four five six' - 10,
              'one two three four five six seven' - 10
              % 'one two three four five six seven eight nine ten eleven' - 10
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
        findall(X-10,
                (between(1, 20, N),
                 count_sentence(1, N, X)),
                Xs).
        
goal(Goal) :-
        number_sentences(Xs),
        member(X-C, Xs),
        atomic_list_concat(Ws, ' ', X),
        T = hear(Ws-[]|[]-[]),
        Goal = count(T,C).
                      
goals(Goals) :-
        findall(G, goal(G),
                 Goals).
        

go :-
        compile_sdcl_file('../example/lpn.gl'),
        Options = [beam_width(10000), time_limit_seconds(1)],
        set_rule_alphas(uniform),
        number_sentences(Xs),
        goals(Goals),
        % sentence_data_set(hear, Xs, Goals),
        writeln(Goals),
        (compile:show_rules;true),
        run_batch_vbem(Goals, Options).
  