:- use_module(library(real)).
:- ['sdcl.pl'].
:- r(source("optimize.r")).
:- debug( real ).

quote_atom(Ain, Aout) :-
        atomic_list_concat(['"', Ain, '"'], Aout).

term_to_quoted_atom(Term, QAtom) :-
        term_to_atom(Term, Atom),
        quote_atom(Atom, QAtom).

r_build_weights(RuleWeights, R_RuleWeights) :-
        r_weights <- list(.), 
        rules(RuleIds),
        findall(_,
                (member(RuleId, RuleIds),
                 rule_functor(RuleId, F/A),
                 get_assoc(RuleId, RuleWeights, W),
                 term_to_quoted_atom(F/A, R_FunctorId),
                 term_to_atom(RuleId, R_RuleId),
                 r_weights[[R_FunctorId]][[+R_RuleId]] <- W),
                _).
                 
                 
                 

log_prior(RuleWeights) :-
        us <- list(.),
        us^[[1]] <- c(-1,-2),
        us^[[2]] <- c(-1,-2),        
        alpha <- 0.1,
        V <- logprior(alpha, us).


        