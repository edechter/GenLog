

:- [sdcl].
:- [learn].


observations([
              'they see stop',
              'they see stop',
              'they see the car stop',
              'they see s the book stop',
              'eyal see s the car stop',
              'eyal see s the book stop'
             ]).

observations2([
              'a a a a'
             ]).
              

data_set(Goals) :-
        findall(Goal,
                (observations2(Obs),
                 member(Ob, Obs),
                 atomic_list_concat(Ws, ' ', Ob),
                 Goal = s(Ws, [])),
                Goals).
                

go0 :-  compile_sdcl_file('trivial.pl'),
        Options = [beam_width(100), inference_limit(1E9)],
        % data_set(Goals),
        % variation_em_single_iteration(Goals, Options),
        % variation_em_single_iteration(Goals, Options).        
        % expected_rule_counts(Goals, ExpectedCounts, Options),
        % update_hyperparams(ExpectedCounts, constant(0.1), HyperParams),
        % compute_variational_weights(HyperParams, NewWeights),
        % map_keys(pprint_rule, NewWeights, NewWeights1),
        % pprint_num_assoc(NewWeights1).
        replicate(3, [a], Xs),
        mi_best_first(s(Xs, []), Derivations, _, Options).
        

go :-
        compile_sdcl_file('trivial.pl'),
        Options = [beam_width(100), inference_limit(1E7)],
        data_set(Goals),
        variation_em_single_iteration(Goals, Options),
        variation_em_single_iteration(Goals, Options).
        % expected_rule_counts(Goals, ExpectedCounts, Options),
        % update_hyperparams(ExpectedCounts, constant(0.1), HyperParams),
        % compute_variational_weights(HyperParams, NewWeights),
        % map_keys(pprint_rule, NewWeights, NewWeights1),
        % pprint_num_assoc(NewWeights1).
        % replicate(3, [a], Xs),
        % time(mi_best_first(s(Xs, []), Derivations, _, Options)),
        
        % pprint_derivs(Derivations), 
        % findall(D-W,
        %         (
        %          member(deriv(_, D, W), Derivations),
        %          pprint_dgraph(D)
        %         ),
        %         Ds).
        % expected_rule_counts1(Ds, Assoc),
        % write(Assoc), nl,
        % assoc_to_list(Assoc, RCs),
        % (
        %  member(R-C, RCs),
        %  writeln(R-C),
        %  fail
        % ;
        %  true
        % ).
        
