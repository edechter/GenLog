

:- [sdcl].
:- [learn].
:- [data_utils].
:- [number_words].

number_lexicon(L) :-
        findall(W,
               (number_word(N, Xs),
                N < 20, 
                member(W, Xs)),
                Ws),
        sort(Ws, L).

number_lex(W) :-
        number_lexicon(L),
        member(W, L).

        
        
              

number_sentences(Xs) :-
        Xs = [
              'one' - 100,
              'one two' - 50,
              'one two three' - 25,
              'one two three four' - 12,
              'one two three four five' - 10,
              'one two three four five six' - 10,
              'one two three four five six seven' - 10
             
              ].
              
        

go :-
        compile_sdcl_file('number.pl'),
        Options = [beam_width(100), time_limit_seconds(2)],
        set_rule_alphas(uniform),
        number_sentences(Xs),
        sentence_data_set(hear, Xs, Goals),
        writeln(Goals), 
        run_batch_vbem(Goals, Options).
  