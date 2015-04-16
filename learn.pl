%% learn.pl
%% author: Eyal Dechter

%% TODO
%% - Implement online learning rule
%% - For each derivaton, compute its probability given current weights
%% - 


%% context_magnitude(+Context:list of RuleIds, -Mag:number)
%%
%% The sum of the weights of the rules in Context

context_magnitude(RuleIds, Mag) :-
        maplist(get_rule_p, RuleIds, Ws),
        sum_list(Ws, Mag).


%% ----------------------------------------------------------------------
%% lagrangian(DerivationP, DGraph, LagrangeMultipliers, Val)
%%
%% Arguments
%%  - DerivationP: conditional probability of derivation
%%  - DGraph: the derivation graph of the derivation
%%  - LagrangeMultipliers: A list of lagrange multiplier values, one for each rule_functor

%% data structures
%%
%% RuleWeights = assoc of RuleId and Weight
%% LagrangeMultipliers = list of lagrange_multiplier(Functor/Arity, Value)

lagrange_init_rule_weights_from_db(RuleWeights) :-
        rules(RuleIds),
        findall(W,
                (member(RuleId, RuleIds),
                 get_rule_p(RuleId, W)),
                Ws),
        pairs_keys_values(Pairs, RuleIds, Ws),
        list_to_assoc(Pairs, RuleWeights).

lagrange_init_multipliers(V, LagrangeMultipliers) :-
        rule_functors(Functors), 
        findall(lagrange_multiplier(F/A, V),
                member(F/A, Functors),
                LagrangeMultipliers).
                 
                
        
lagrangian(DerivationP,
           dgraph(_, HyperEdges),
           RuleWeights, 
           LagrangeMultipliers,
           Val) :-
        lagrangian_f_ll(DerivationP, HyperEdges, RuleWeights, V_f_ll),
        lagrangian_f_lp(RuleWeights, V_f_lp),         
        lagrangian_g(LagrangeMultipliers, RuleWeights, V_g),
        Val is V_f_ll + V_f_lp + V_g.

lagrangian_f_ll(DerivationP, HyperEdges, RuleWeights, Val) :-
        lagrangian_f_ll_loop(HyperEdges, RuleWeights, 0, V0),
        Val is V0 * DerivationP,
        format("f_ll: ~w\n", [Val]).
        
lagrangian_f_ll_loop([], _, V_in, V_out) :- !, V_in = V_out.
lagrangian_f_ll_loop([hyperedge(_, RuleId-Context, _, _)|Hs],
                     RuleWeights, V_in, V_out) :-
        get_assoc(RuleId, RuleWeights, W_r),
        context_magnitude(Context, MagC),
        % write(MagC), nl,
        V_next is V_in + log(W_r) - log(MagC),
        % write(V_next), nl,
        lagrangian_f_ll_loop(Hs, RuleWeights, V_next, V_out).

lagrangian_f_lp(RuleWeights, Val) :-
        rules(RuleIds), 
        lagrangian_f_lp_loop(RuleIds, RuleWeights, 0, Val).
        % format("f_lp: ~w\n", [Val]).
lagrangian_f_lp_loop([], _, V_in, V_out) :- !, V_out = V_in.
lagrangian_f_lp_loop([RuleId|RuleIds], RuleWeights, V_in, V_out) :-
        get_rule_alpha(RuleId, Alpha),
        get_assoc(RuleId, RuleWeights, W),
        V_next is V_in + (Alpha - 1) * log(W),
        lagrangian_f_lp_loop(RuleIds, RuleWeights, V_next, V_out).

%% LagrangeMultipliers is a list of structures: lagrange_multiplier(Functor/Arity, Val).
lagrangian_g(LagrangeMultipliers, RuleWeights, Val) :-
        lagrangian_g_loop(LagrangeMultipliers, RuleWeights, 0, Val).
lagrangian_g_loop([], _, V_in, V_out) :- !, V_in = V_out.
lagrangian_g_loop([lagrange_multiplier(Functor/Arity, L)|Ls], RuleWeights, V_in, V_out) :-
        functor_rules(Functor/Arity, RuleIds),
        findall(W,
                (member(RuleId, RuleIds),
                 member(rule_weight(RuleId, W), RuleWeights)),
               Ws),
        sum_list(Ws, SumW),
        V_next is V_in - L * (SumW - 1),
        lagrangian_g_loop(Ls, RuleWeights, V_next, V_out).

        
        
%% ----------------------------------------------------------------------
%% Gradients of lagrangian

%% dl_dWr(RuleId, DerivationP, HyperEdges, LagrangeMultipliers, RuleWeights, Val)
dL_dWr(RuleId, DerivationP, HyperEdges, LagrangeMultipliers, RuleWeights, Val) :-
        dL_dWr_loop(RuleId, DerivationP, HyperEdges, LagrangeMultipliers,
                    RuleWeights, 0, V0),
        % Lagrangian
        rule_functor(RuleId, F/A),
        member(lagrange_multiplier(F/A, L), LagrangeMultipliers),
        Val is V0 - L.


dL_dWr_loop(_, _, [], _, _, V_in, V_out) :- !, V_in = V_out.
dL_dWr_loop(RuleId, DerivationP, [hyperedge(_, EdgeRuleId-Context, _, _)|Hs],
            Multipliers, RuleWeights, V_in, V_out) :-

        get_assoc(RuleId, RuleWeights, W_r),         
        % is r the rule for this edge
        (EdgeRuleId = RuleId ->
         Term1 is 1 / W_r
        ;
         Term1 = 0
        ),
        % is r in context?
        (member(RuleId, Context) ->
         context_magnitude(Context, Mag), 
         Term2 is 1/Mag
        ;
         Term2 is 0
        ),
        % term from deriv of prior
        get_rule_alpha(RuleId, Alpha),
        Term3 is (Alpha - 1) / W_r,
        V_next is V_in + DerivationP * (Term1 - Term2 + Term3),
        format("~w ~t Term1: ~t ~w, Term2: ~t ~w, Term3: ~t ~w\n",
               [RuleId, Term1, Term2, Term3]),
        dL_dWr_loop(RuleId, DerivationP, Hs, Multipliers, RuleWeights, V_next, V_out).

%% Derivations = list of deriv(Goal, dgraph(_, HyperEdges), ConditionalProb)
dL_dW(Derivations, LagrangeMultipliers, RuleWeights, Pairs) :-
        rules(RuleIds), 
        findall(RuleId-V,
                (member(deriv(_, dgraph(_, HyperEdges), DerivationP), Derivations),
                 member(RuleId, RuleIds), 
                 dL_dWr(RuleId, DerivationP, HyperEdges,
                        LagrangeMultipliers, RuleWeights, V)),
                Pairs).

%% dl_dlambda_f(Functor/Arity, RuleWeights, Val) returns the first derivative of
%% the Lagrangian w.r.t to lambda_f, the lagrange multiplier
%% associated with functor Functor/Arity. This first derivative is
%% just the negative constraint that the weights of the functor must
%% sum to one.
dL_dlambda_f(Functor/Arity, RuleWeights, Val) :-
        functor_rules(Functor/Arity, RuleIds),
        findall(W,
                (member(RuleId, RuleIds),
                 get_assoc(RuleId, RuleWeights, W)),
                Ws),
        sum_list(Ws, Z),
        Val is - (Z - 1).

%% dL_dlambda(RuleWeights, Pairs
%%
%% Pairs: List of Functor/Arity-Val where Val is the first derivative
%% of the Lagrangian w.r.t to corresponding Functor/Arity
dL_dlambda(RuleWeights, Pairs) :-
        
                 
        
        
        
        
        
        



