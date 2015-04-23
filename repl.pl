

%% A repl for sdcl.
%% NB: Skeleton. This doesn't do anything yet. 

:- [sdcl].

repl :-        
        read_history(h, '!h', [trace], '>> ', Goal, Bindings),
        process(Goal, Bindings),
        (
         Goal = end_of_file ->
         writeln('Leaving SDCL.')
        ;
         repl
        ).

process((Goal1, Goal2), Bindings) :-
        !, 
        process(Goal1, Bindings),
        process(Goal2, Bindings).
process(Goal, Bindings) :-
        writeln(Rule),
        writeln(Bindings).