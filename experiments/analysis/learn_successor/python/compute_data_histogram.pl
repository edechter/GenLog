

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/', src], Src),
   asserta(user:file_search_path(genlog, Src));
   true.

:- getenv('GENLOG_ROOT', Dir),
   atomic_list_concat([Dir, '/experiments/scripts/learn_successor'], Exp),
   asserta(user:file_search_path(experiment, Exp));
   true.


:- use_module(experiment(runner)).
:- ensure_loaded(experiment(number_words)).


zero_padded_int(Int, Width, Out) :-
        format(atom(Format), '~~|~~`0t~~d~~~d+', [Width]),
        format(atom(Out), Format, [Int]).




data_from_iter(Dir, Iter, Datum) :-
        zero_padded_int(Iter, 4, I),
        format(atom(Path), '~w/ovbem_gl_~w.gl', [Dir, I]),
        process_create(path(tail),
                       ['-n',  1, Path], [stdout(pipe(Stream))]),
        read_term(Stream, ovbem_info(Info), []),
        Datum = Info.goal,
        close(Stream).

data_from_iters(Dir, Iters, Data) :-
        findall(Datum,
                (member(Iter, Iters),
                 data_from_iter(Dir, Iter, Datum)),
                Data).

data_from_iters_as_numbers(Dir, Iters, Data) :-
        data_from_iters(Dir, Iters, DataGoals),
        findall(Number,
                (member(count(succ(S1, S2), _), DataGoals),
                 number_word(Number, S1)),
                Data).



                
        
        



     