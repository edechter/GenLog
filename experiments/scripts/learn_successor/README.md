## README.md

#+STARTUP: indent

### Experiment:

- model how children learn the successor relation between number words
  in different languages.

----------------------------------------------------------------------


*  9/11/2015
** Experiment 1

commit 757cdbe
PWD: /Users/edechter/Dropbox/Projects/SDCL/experiments/scripts/learn_successor/

Learning successors 1..99 in two phases. 
    1. 1-9, single batch, count 100 each.  
  2. 1-98, single batch, count 100 each. 

Code:     


:LOGBOOK:

IN: ../gls/succ_04.gl.
num_preds(1, 4).
num_preds(2, 4).

IN: runner.pl
number_word_settings([split_teens(false), irregular_decade(true), split_decade(false)]).

exp_constants(
constants{beam_width:          200,
time_limit_seconds:  5,
init_alpha :         uniform(2),
max_iter   :         10
}
).

phase1(
constants{
goal_generator: GoalGen, 
goals_per_iter: 9,
max_online_iter: 1,
run_id: phase1
}
) :-
succ_goals(1, 9, 100, Goals),
list_to_circular(Goals, GoalGen). 

phase2(
constants{
goal_generator: GoalGen, 
goals_per_iter: 98,
max_online_iter: 1,
run_id: phase2
}
) :-
succ_goals(1, 98, 100, Goals),
list_to_circular(Goals, GoalGen).

phases([phase1, phase2]).

?- run_experiment('runner.pl').
?- count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), 
asserta(data(D)), 
pairs_keys_values(D.transition_probs, Xs, Ys), 
plot_xy(Xs, Ys), 
show. 
:END:
       
RESULTS:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/390cghfpg36dt71/Screenshot%202015-09-11%2011.15.48.png?dl=0

Succeeds in learning up to 19 (with exception of 8-9?) but not beyond. 

Some sample interrogations:

:LOGBOOK:
?- interact:ask(succ([twenty, one], X), L, [beam_width(200), time_limit_seconds(5)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,5)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 16
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 164
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 162
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 200
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -9.72 ; cond prob: 0.22
% Expl Search:     score: -9.81 ; cond prob: 0.20
% Expl Search:     score: -10.66 ; cond prob: 0.09
% Expl Search:     score: -10.66 ; cond prob: 0.09
% Expl Search:     score: -10.75 ; cond prob: 0.08
% Expl Search:     score: -10.75 ; cond prob: 0.08
% Expl Search:     score: -10.82 ; cond prob: 0.07
% Expl Search:     score: -11.05 ; cond prob: 0.06
% Expl Search:     score: -11.15 ; cond prob: 0.05
% Expl Search:     score: -11.68 ; cond prob: 0.03
% Expl Search:     
X = [two, two],
L = 0.5353574252404573 ;
X = [two, one],
L = 0.16544635118254014 ;
X = [one, two],
L = 0.16544635118254014 ;
X = [twenty, two],
L = 0.07407287853599932 ;
X = [two, thirty],
L = 0.05967699385846397 ;
X = [two, seventy],
L = 7.962693151529136e-28 a

?- interact:ask(succ([twenty, nine], X), L, [beam_width(500), time_limit_seconds(6)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(500,6)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 16
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 406
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 402
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 612
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -11.81 ; cond prob: 0.03
% Expl Search:     score: -11.87 ; cond prob: 0.03
% Expl Search:     score: -11.87 ; cond prob: 0.03
% Expl Search:     score: -11.90 ; cond prob: 0.03
% Expl Search:     score: -11.93 ; cond prob: 0.03
% Expl Search:     score: -11.96 ; cond prob: 0.02
% Expl Search:     score: -11.96 ; cond prob: 0.02
% Expl Search:     score: -11.96 ; cond prob: 0.02
% Expl Search:     score: -11.96 ; cond prob: 0.02
% Expl Search:     score: -12.02 ; cond prob: 0.02
% Expl Search:     
X = [one, ten],
L = 0.07632984947858547 ;
X = [ten, ten],
L = 0.07182420775752052 ;
X = [one, one],
L = 0.05562747643834421 ;
X = [twenty, ten],
L = 0.05555964828993593 ;
X = [ten, one],
L = 0.05234386615494547 ;
X = [one, eightty],
L = 0.04918998960381789 ;
X = [ten],
L = 0.04815839207757795 a

?- interact:ask(succ([threeteen], X), L, [beam_width(500), time_limit_seconds(6)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(500,6)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 16
% Expl Search:     Beam Size: 80
% Expl Search:     Beam Size: 64
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
1441985913.558166-1441985907.452394>=6
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 1574
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -5.27 ; cond prob: 0.77
% Expl Search:     score: -7.04 ; cond prob: 0.13
% Expl Search:     score: -8.39 ; cond prob: 0.03
% Expl Search:     score: -9.30 ; cond prob: 0.01
% Expl Search:     score: -10.16 ; cond prob: 0.01
% Expl Search:     score: -10.54 ; cond prob: 0.00
% Expl Search:     score: -12.01 ; cond prob: 0.00
% Expl Search:     score: -12.16 ; cond prob: 0.00
% Expl Search:     score: -12.62 ; cond prob: 0.00
% Expl Search:     score: -12.62 ; cond prob: 0.00
% Expl Search:     
X = [fourteen],
L = 0.9535470602505295 ;
X = [fourteen, fiveteen, fiveteen],
L = 0.002311466527629369 ;
X = [fourteen, fourteen, fourteen],
L = 0.0016241285049746188 ;
 X = [fourteen, two],
L = 0.001390261917832639 a

:END: 





** Experiment 2
commit: 9043f16

Same as Experiment 1, but we increase the number of predicates of type
1 and 2 to 5.

Experiment log: 
:LOGBOOK:
?- run_experiment('runner.pl') 

% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-11-11-48-15
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-11-11-48-15/data
% Experiment Info:     git_hash:         9043f16f1d2010f7e8dff820185811918a229630
% Experiment Info:     start_time:       date(2015,9,11,11,48,15.87036395072937,14400,EDT,true)
% Experiment Info:     random_seed:      12487481...
% Experiment Info:     ...............................................
% Compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl...
% Success! Finished compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl.
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),9,/Users/edechter/data/exp_2015-09-11-11-48-15/data,phase1)
% Online VBEM:     
...

-- Note: After fininshing training, this resulted in exception due to 
bug in run_testing, so manually ran the following.
?- count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), asserta(data(D)), pairs_keys_values(D.transition_probs, Xs, Ys), plot_xs(Xs, Ys), show. 
:END:

RESULTS:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/c18d11f09ftd45q/Screenshot%202015-09-11%2012.19.04.png?dl=0

Much higher probabilities betweeen 1 and 10. Failing at 18. Slightly
higher probability at the decade transitions vs within decade,
probably because the result is of length 1, not 2. 

:LOGBOOK:
Some interrogations: 
interact:ask(succ([twenty, one], X), L, [beam_width(200), time_limit_seconds(5)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,5)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 25
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 189
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 169
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
1441988585.719006-1441988580.615021>=5
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 364
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -8.24 ; cond prob: 0.33
% Expl Search:     score: -9.18 ; cond prob: 0.13
% Expl Search:     score: -9.19 ; cond prob: 0.13
% Expl Search:     score: -9.47 ; cond prob: 0.10
% Expl Search:     score: -9.75 ; cond prob: 0.07
% Expl Search:     score: -9.76 ; cond prob: 0.07
% Expl Search:     score: -10.41 ; cond prob: 0.04
% Expl Search:     score: -10.42 ; cond prob: 0.04
% Expl Search:     score: -10.62 ; cond prob: 0.03
% Expl Search:     score: -10.71 ; cond prob: 0.03
% Expl Search:     
X = [twenty],
L = 0.3275453659450952 ;
X = [one],
L = 0.20050252435539187 ;
X = [thirty],
L = 0.1991315583589306 ;
X = [twenty, two],
L = 0.1260392185553493 ;
X = [one, two],
L = 0.04930934659968724 ;
X = [thirty, two],
L = 0.04897218656784609 a

?-  interact:ask(succ([twenty, nine], X), L, [beam_width(200), time_limit_seconds(3)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,3)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 25
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 189
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 169
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
1441988698.817007-1441988695.658908>=3
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 364
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -9.37 ; cond prob: 0.33
% Expl Search:     score: -10.31 ; cond prob: 0.13
% Expl Search:     score: -10.32 ; cond prob: 0.13
% Expl Search:     score: -10.60 ; cond prob: 0.10
% Expl Search:     score: -10.88 ; cond prob: 0.07
% Expl Search:     score: -10.89 ; cond prob: 0.07
% Expl Search:     score: -11.54 ; cond prob: 0.04
% Expl Search:     score: -11.55 ; cond prob: 0.04
% Expl Search:     score: -11.75 ; cond prob: 0.03
% Expl Search:     score: -11.84 ; cond prob: 0.03
% Expl Search:     
X = [twenty],
L = 0.3275453659450952 ;
X = [one],
L = 0.20050252435539187 ;
X = [thirty],
L = 0.1991315583589306 ;
X = [twenty, ninety],
L = 0.09562235507186667 a

?-  interact:ask(succ([twenty, nine], X), L, [beam_width(500), time_limit_seconds(6)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(500,6)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 25
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 449
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 427
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
1441988720.387642-1441988713.571632>=6
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 224
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -9.37 ; cond prob: 0.38
% Expl Search:     score: -10.31 ; cond prob: 0.15
% Expl Search:     score: -10.32 ; cond prob: 0.15
% Expl Search:     score: -10.88 ; cond prob: 0.08
% Expl Search:     score: -10.89 ; cond prob: 0.08
% Expl Search:     score: -11.34 ; cond prob: 0.05
% Expl Search:     score: -11.61 ; cond prob: 0.04
% Expl Search:     score: -11.97 ; cond prob: 0.03
% Expl Search:     score: -12.31 ; cond prob: 0.02
% Expl Search:     score: -12.43 ; cond prob: 0.02
% Expl Search:     
X = [twenty],
L = 0.4266529899990143 ;
X = [one],
L = 0.23158176649118545 ;
X = [thirty],
L = 0.23033327322929043 ;
X = [ninety],
L = 0.09336682690778347 

?-  interact:ask(succ([threeteen], X), L, [beam_width(500), time_limit_seconds(6)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(500,6)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 25
% Expl Search:     Beam Size: 150
% Expl Search:     Beam Size: 125
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
% Expl Search:     Beam Size: 500
1441988808.66424-1441988801.602493>=6
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 616
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -5.47 ; cond prob: 0.60
% Expl Search:     score: -6.84 ; cond prob: 0.15
% Expl Search:     score: -7.42 ; cond prob: 0.09
% Expl Search:     score: -7.61 ; cond prob: 0.07
% Expl Search:     score: -8.76 ; cond prob: 0.02
% Expl Search:     score: -9.00 ; cond prob: 0.02
% Expl Search:     score: -9.30 ; cond prob: 0.01
% Expl Search:     score: -10.12 ; cond prob: 0.01
% Expl Search:     score: -10.17 ; cond prob: 0.01
% Expl Search:     score: -10.28 ; cond prob: 0.00
% Expl Search:     
X = [fourteen],
L = 0.979071581598065 ;
X = [fourteen, fourteen],
L = 0.006726150200643701 ;
X = [fourteen, fiveteen],
L = 0.0048548646150131875 ;
X = [fourteen, fourteen, fourteen],
L = 0.0011798535287663799 ;
X = [fourteen, fiveteen, fiveteen],
L = 0.0003933943137764275 a

:END:
      

** Experiment 3:
commit: 57dd412

Same as Experiment 2, but we increase the number of predicates of type
1 and 2 to 6.  
  
:LOGBOOK:

:-? run_experiment('runner.pl')
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-11-12-31-39
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-11-12-31-39/data
% Experiment Info:     git_hash:         57dd4124c496bef3aba4d7541ca2328c672ce54c
% Experiment Info:     start_time:       date(2015,9,11,12,31,39.11435508728027,14400,EDT,true)
% Experiment Info:     random_seed:      12487481...
% Experiment Info:     ...............................................
% Compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl...
% Success! Finished compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl.
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),9,/Users/edechter/data/exp_2015-09-11-12-31-39/data,phase1)
% Online VBEM:     
% Online VBEM:     Running ...
% Online VBEM:     Iter 1 ...


-- Note: After fininshing training, this resulted in exception due to 
bug in run_testing, so manually ran the following.
?- count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), asserta(data(D)), pairs_keys_values(D.transition_probs, Xs, Ys), plot_xs(Xs, Ys), show. 
:END:

RESULTS:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/om1mv9j0e4sxgzn/Screenshot%202015-09-11%2012.50.44.png?dl=0

Transition probabilities: 
:LOGBOOK:


Ys = [0.8348960076808607, 0.999253885131731, 0.9986538643591465,
0.9391001523421316, 0.9730483824073327, 0.9449757873672098,
0.9971780000883821, 0.789456843422771, 0.8794408386258151,
0.9975555499135818, 0.9001371398237797, 0.9563340562193792,
0.9967708006390283, 0.9107669206573844, 0.9107669206573844,
0.9975555499135816, 0.9938129656515987, 0.9676153966798274,
0.9885466529654681, 0.012321756209360765, 0.6798276086324226,
0.714234259323295, 0.19138312482816436, 1.0, 0.4844497711816233,
0.7082975563001966, 0.03834661441979709, 0.38073878664534594,
0.040979729320411475, 0.03363982000168605, 0.49319172435950304,
0.32471670217292986, 0.27844446724080346, 0.40828605589618067,
0.34053545494270526, 0.3460329031601364, 0.28049122855693664,
0.3460329031601362, 0.04239924695924115, 4.379478066228747e-30,
0.08495714231458074, 0.2646891617462048, 0.1726765890252186,
0.2646891617462049, 0.1386475674695755, 0.26468916174620505,
0.14575191204811141, 0.13323886722429543, 0.04188196295044259,
0.027229563767386868, 0.22276141974957683, 0.2751776404608424,
0.15728853912227564, 0.6307033801944121, 0.19862554038859268,
0.4129928457132665, 0.159639102622173, 0.07796537924292767,
0.0470196375597008, 0.07354731075302953, 0.21452364368870178,
0.19511861712071873, 0.4417921179746485, 0.16221203124790504,
0.17671018766958807, 0.18011162460971047, 0.22167195518355712,
0.18011162460971064, 0.04319656888495399, 0.02209149117335391,
0.4121078631143307, 0.23961048567634619, 0.015800949271745628,
0.9950544805217981, 0.49135796960265354, 0.6912595968535984,
0.3164101398927246, 0.04142940907041435, 0.043125754669901394,
0.006144407516198805, 0.7525353878604034, 0.34960566437647106,
0.01138228048581305, 0.5229520093492716, 0.3541270945120603,
0.5330225528816955, 0.20313644441381115, 0.5286979095486882,
0.04241986401500388, 0.12496472510159319, 0.25560930800247933,
0.1866832358436482, 0.1339037178817731, 0.4976924636850074,
0.34697614882518835, 0.3482424648918911, 0.13492084502653034]
:END:

Lower but still high probabilities between 1 and 8. Failing
at 19. Failing at 18. Somewhat higher probabilities within
decades.

Some interrogations: 
:LOGBOOK:
?- interact:ask(succ([twenty, one], X), L, [beam_width(200), time_limit_seconds(5)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,5)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 36
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 191
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 176
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 228
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -8.98 ; cond prob: 0.22
% Expl Search:     score: -9.44 ; cond prob: 0.14
% Expl Search:     score: -9.68 ; cond prob: 0.11
% Expl Search:     score: -9.68 ; cond prob: 0.11
% Expl Search:     score: -9.68 ; cond prob: 0.11
% Expl Search:     score: -9.68 ; cond prob: 0.11
% Expl Search:     score: -9.88 ; cond prob: 0.09
% Expl Search:     score: -10.47 ; cond prob: 0.05
% Expl Search:     score: -10.63 ; cond prob: 0.04
% Expl Search:     score: -11.09 ; cond prob: 0.03
% Expl Search:     
X = [twenty, two],
L = 0.6798276086324226 ;
X = [twenty, nine],
L = 0.10672399591189384 ;
X = [twenty, four],
L = 0.10672399591189384 ;
X = [twenty, six],
L = 0.10672399591189365 a

?- interact:ask(succ([twenty, nine], X), L, [beam_width(200), time_limit_seconds(5)]).
% Expl Search:     Initializing with options: 
% Expl Search:     options(200,5)
% Expl Search:     
% Expl Search:     Running ...
% Expl Search:     Beam Size: 36
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 191
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 176
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% Expl Search:     Beam Size: 200
% 
% Expl Search:      ---  Derivation Scores  --- 
% Expl Search:     Number of derivations found: 256
% Expl Search:     Top 10 derivations:
% Expl Search:     score: -7.66 ; cond prob: 0.23
% Expl Search:     score: -8.12 ; cond prob: 0.14
% Expl Search:     score: -8.17 ; cond prob: 0.14
% Expl Search:     score: -9.30 ; cond prob: 0.04
% Expl Search:     score: -9.34 ; cond prob: 0.04
% Expl Search:     score: -9.40 ; cond prob: 0.04
% Expl Search:     score: -9.40 ; cond prob: 0.04
% Expl Search:     score: -9.68 ; cond prob: 0.03
% Expl Search:     score: -9.68 ; cond prob: 0.03
% Expl Search:     score: -9.76 ; cond prob: 0.03
% Expl Search:     
X = [twenty, ten],
L = 0.6155793603408802 ;
X = [ninety],
L = 0.04242055518274406 ;
X = [fourty],
L = 0.04145030122645818 ;
X = [thirty],
L = 0.040979729320411495 ;
X = [fifty],
L = 0.04072817537476827 ;
X = [eightty],
L = 0.04060541828246643 ;
X = [sixty],
L = 0.03973425358073265 ;
X = [seventy],
L = 0.03973425358073265 a

:END: 


** Experiment 4:
commit: 3257b17
 Using the saved grammar from Experiment 3 as an initial state, will
 run another phase with count 10.

:LOGBOOK:
%%                               Constants
exp_constants(
   constants{beam_width:          200,
             time_limit_seconds:  5,
             init_alpha :         uniform(2),
             max_iter   :         10
            }
             ).

phase1(
       constants{
        goal_generator: GoalGen, 
                 goals_per_iter: 98,
                 max_online_iter: 1,
                 run_id: phase1
                }
      ) :-
        succ_goals(1, 98, 10, Goals),
        list_to_circular(Goals, GoalGen). 



?- load_gl('/Users/edechter/data/exp_2015-09-11-12-31-39/data/ovbem_gl_phase2_0001.gl.gz').
[Note: if this file is already ungzipped, needs to be run with just the .gl extension.]

?- run_experiment('runner.pl', run_phase(phase1), []).
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-11-13-35-16
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-11-13-35-16/data
% Experiment Info:     git_hash:         3257b17725cd20b43c36c44ca8b924b750e6af3e
% Experiment Info:     start_time:       date(2015,9,11,13,35,16.314450979232788,14400,EDT,true)
% Experiment Info:     random_seed:      12487481...
% Experiment Info:     ...............................................
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),98,/Users/edechter/data/exp_2015-09-11-13-35-16/data,phase1)
% Online VBEM:     

?- count(1, 98, D, [beam_width(200), time_limit_seconds(3)]), asserta(data(D)), 
pairs_keys_values(D.transition_probs, Xs, Ys), plot_xs(Xs, Ys), show. 

:END:


RESULTS: 
 :LOGBOOK:

D = results{count_probs:[1-0.8390001040147295,
2-0.8373576321805238, 3-0.8363214057107003, 4-0.7870824206275033,
5-0.7752689382151293, 6-0.7350197737518672, 7-0.7330610911768565,
8-0.581211211281657, 9-0.514225571358012, 10-0.5130055664738716,
11-0.4631174440864562, 12-0.44496566864008413, 13-0.4436403734201107,
14-0.4051889537108783, 15-0.3699524873921848, 16-0.36907477172291847,
17-0.3668618352208935, 18-0.35524892000448166, 19-0.3514388265156975,
20-0.004441083145107802, 21-0.003250291733076483,
22-0.0021411009419126095, 23-0.00041963131897050003,
24-0.00034625175147086926, 25-0.00023549202155979793,
26-0.0001700935313980902, 27-6.3873511412007796e-6,
28-2.727629476290606e-6, 29-1.0660151696264009e-7,
30-3.504429358041355e-9, 31-1.8052367825100212e-9,
32-7.495513433538626e-10, 33-2.1068453810675387e-10,
34-8.95179249707476e-11, 35-3.248904344030328e-11,
36-1.1978453352637196e-11, 37-3.3989698357059276e-12,
38-1.4364403974915184e-12, 39-5.80937116945751e-14,
40-2.2639843503918793e-43, 41-5.166161113534382e-44,
42-1.029698285790497e-44, 43-2.198151531211833e-45,
44-4.254170635066141e-46, 45-2.4006025861660414e-47,
46-4.7847837369289404e-48, 47-8.657268415343265e-49,
48-1.0996442268987749e-49, 49-4.3815662103330167e-51,
50-1.1599955302723025e-52, 51-2.6683299433157714e-53,
52-8.430961917286475e-54, 53-1.3637536486869197e-54,
54-8.890723760615465e-55, 55-2.465904723445398e-55,
56-1.0831080774659141e-55, 57-1.757298807080553e-56,
58-2.2156040565149053e-57, 59-9.875815063469617e-59,
60-7.071767713946587e-60, 61-2.654368844455653e-60,
62-1.0569900639275715e-60, 63-4.77814626298516e-61,
64-1.4491087960378401e-61, 65-4.11461032924732e-62,
66-1.168305527358529e-62, 67-3.2275348183031876e-63,
68-9.16428644813009e-64, 69-3.4511425411091973e-65,
70-7.66370696744935e-67, 71-3.950927217951057e-67,
72-1.0963215384695047e-67, 73-1.6746290758766072e-69,
74-1.6746290758766072e-69, 75-1.1979915973076806e-69,
76-8.465719175205023e-70, 77-2.7124702671376968e-70,
78-8.162963786257057e-71, 79-3.340502672832985e-72,
80-2.0466739362264267e-74, 81-1.5768067718338626e-74,
82-6.050405444432932e-75, 83-6.4595366494724035e-77,
84-3.3708587343020917e-77, 85-1.8186430522754376e-77,
86-1.0120247650543928e-77, 87-2.0177959873903926e-78,
88-1.1387545584035195e-78, 89-4.59819006645908e-80,
90-5.629954302209654e-81, 91-1.6199786134397618e-81,
92-2.857090514106272e-82, 93-3.898099778575489e-83,
94-2.5033354264029265e-83, 95-9.82196980108112e-84,
96-3.6197289650391064e-84, 97-5.04012635721577e-85],
transition_probs:[1-0.8390001040147295, 2-0.9980423460898918,
3-0.9987625042992382, 4-0.9411243276245523, 5-0.9849907937176952,
6-0.9480836101135096, 7-0.9973351974396378, 8-0.7928550816257078,
9-0.8847481971727081, 10-0.9976274908287458, 11-0.9027532532827667,
12-0.9608052435118737, 13-0.9970215787118503, 14-0.9133275012533173,
15-0.9130369522762547, 16-0.9976274908287457, 17-0.9940040970785011,
18-0.9683452621627444, 19-0.9892748625703462, 20-0.01263685970368853,
21-0.7318691469798154, 22-0.658741158562405, 23-0.19598857333445027,
24-0.8251332439159782, 25-0.6801179215973158, 26-0.7222899963721223,
27-0.0375519932398352, 28-0.42703609305215523,
29-0.039082110634620006, 30-0.03287410402677036,
31-0.5151300249119526, 32-0.41520943436111357, 33-0.28108086253844505,
34-0.42489081436716, 35-0.3629333840224732, 36-0.3686920907550542,
37-0.2837569872873116, 38-0.42261051639876823,
39-0.040442827837496906, 40-3.897124635958309e-30,
41-0.22818890566271613, 42-0.19931594527566684,
43-0.21347530257606645, 44-0.1935340023042375,
45-0.056429391110418364, 46-0.19931594527566643,
47-0.18093332721657834, 48-0.12701976814648336,
49-0.03984530726533202, 50-0.026474449422599005,
51-0.23002932974141682, 52-0.31596399607200865,
53-0.16175540372098454, 54-0.6519303372112576, 55-0.27735702849850935,
56-0.4392335466848775, 57-0.1622459331290377, 58-0.1260800979086617,
59-0.044573916690710744, 60-0.07160692731180104,
61-0.37534728964878744, 62-0.39820768170006704, 63-0.4520521456209805,
64-0.30327845073802856, 65-0.28394074623641136, 66-0.2839407462364111,
67-0.2762577718518936, 68-0.28394074623641186,
69-0.037658606162549395, 70-0.02220628929741695,
71-0.5155373547986808, 72-0.27748462018949943,
73-0.015274981080955829, 74-1.0, 75-0.7153772823874897,
76-0.706659311653817, 77-0.32040636016868657, 78-0.30094205585047507,
79-0.04092266926942594, 80-0.00612684418088102, 81-0.7704240250115826,
82-0.383712548202477, 83-0.010676204609421537, 84-0.5218421873304819,
85-0.5395192132404719, 86-0.5564724555421552, 87-0.19938207611766726,
88-0.5643556462198471, 89-0.04037911446787556, 90-0.12243848603120278,
91-0.28774276423592815, 92-0.17636594029100816, 93-0.1364359917660801,
94-0.6421937786614941, 95-0.39235532312161736, 96-0.3685339130894779,
97-0.13924043501310376]}
:END:
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/xo1exvfo30aklqz/Screenshot%202015-09-11%2013.58.30.png?dl=0

Slight improvements across the board.

** Experiment 5: 
ABORTED see note below
commit: ee3ab61

Same as Experiment 4, but count 100. That is, start with grammar learned
after experiment 3, and train on numbers 1-98 for a one batch 100
count.

:LOGBOOK:
?- load_gl('/Users/edechter/data/exp_2015-09-11-12-31-39/data/ovbem_gl_phase2_0001.gl').

:END:

Note: Experiment 5 failed because of bug in add_to_pqueue. The bug was
that if an a really bad derivation was inserted into a saturated
pqueue, then the infinum was not being updated when a better
derivation was found. This meant that insertion of derivations was not
being pruned in some cases. This bug should have only affected the
runtime of the code, not the accuracy, but since the code is
time-bound, it's hard to tell.

ABORT

** Experiment 5*: 

commit: 884b25b

Same as Experiment 4, but count 100. That is, start with grammar
learned after experiment 3, and train on numbers 1-98 for a one batch
100 count.

:LOGBOOK:
?- load_gl('/Users/edechter/data/exp_2015-09-11-12-31-39/data/ovbem_gl_phase2_0001.gl').
?- run_experiment('runner.pl', run_phase(phase1), []), run_experiment('runner.pl', run_testing, []).


:END:

RESULTS: 
 :LOGBOOK:
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-11-15-42-11
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-11-15-42-11/data
% Experiment Info:     git_hash:         884b25b294dc005d2ee38dfb6dfecd8830a96bcf
% Experiment Info:     start_time:       date(2015,9,11,15,42,11.601059913635254,14400,EDT,true)
% Experiment Info:     random_seed:      31024141...
% Experiment Info:     ...............................................
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),98,/Users/edechter/data/exp_2015-09-11-15-42-11/data,phase1)
% Online VBEM:     
% Online VBEM:     Running ...
% Online VBEM:     Iter 1 ...
:END:

RESULTS: 
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/zp5ouvyw81ym8h5/Screenshot%202015-09-11%2016.33.37.png?dl=0

:LOGBOOK:  
Transition probabilities:
transition_probs:[1-0.8819267439168621, 2-0.9964615894890433, 3-1.0,
4-0.9669823914640329, 5-0.9758684563494353, 6-0.9663924666516334,
7-0.9970987700462761, 8-0.8174769545520547, 9-0.9216563070139926,
10-0.9970987700462761, 11-0.9144725343409025, 12-0.9837306680090411,
13-0.9961126147059345, 14-0.9205690265007777, 15-0.9205690265007777,
16-0.9970987700462761, 17-0.993179262532709, 18-0.9752436242022461,
19-0.993255860317384, 20-0.012657617617366453, 21-0.9966493583867896,
22-0.8269665943420598, 23-0.8081647458490632, 24-0.8209888670063071,
25-0.821134310415119, 26-0.8245199357026037, 27-0.6440907197728113,
28-0.9966349576931541, 29-0.024549546221530247,
30-0.022013923356399418, 31-1.0, 32-0.8223503816831369,
33-0.692473425551038, 34-0.8162275078011475, 35-0.8174503267706124,
36-0.8198526696975572, 37-0.627716609723971, 38-1.0,
39-0.02541776096850718, 40-1.0131102799606889e-30,
41-0.5642422794307462, 42-0.5659383614722829, 43-0.5659383614722834,
44-0.8263708673379954, 45-0.5603711153072097, 46-0.5642422794307461,
47-0.5014249868954685, 48-0.5351001248261147, 49-0.024755290041375017,
50-0.028951896863469363, 51-0.996381547272608, 52-0.8375662435029375,
53-0.34126736607734953, 54-0.9917315381602689, 55-0.7328307562083985,
56-0.7356738899698532, 57-0.3062244447984002, 58-0.996364746762134,
59-0.026304163370902637, 60-0.0755933475092099, 61-0.7838294111706392,
62-0.6398634791369182, 63-0.7324623559221278, 64-0.8263708672993428,
65-0.6341462559330567, 66-0.6379277740622953, 67-0.5868411962677674,
68-0.8220568193178489, 69-0.024500416892379247,
70-0.02288443455626418, 71-0.9966493583867897, 72-0.9112779482504613,
73-0.18190353215159313, 74-0.9924835974411014, 75-0.8174011161654047,
76-0.8208636560626952, 77-0.47725354671351045, 78-0.9966349576931541,
79-0.024892372059492327, 80-3.261103571913881e-29,
81-0.9159105877328165, 82-0.7378253114677445, 83-0.6525430329627088,
84-0.9140872334981047, 85-0.7330526042584907, 86-0.7356423948961681,
87-0.6676509272447101, 88-0.8890161175092535, 89-0.025151670550171076,
90-0.08114420535058751, 91-0.8687285261922455, 92-0.8624305767469064,
93-0.2957895073376663, 94-0.8597528406354771, 95-0.695795600818389,
96-0.6987164081317703, 97-0.2848718820426597]

 :END:

 At this point, we have pretty accurate transitions within decade, but
 fail at all decade transitions (29-30, ...) and (20-21, 30-31, 40-41,
 ...) but we succeed at 19-20.

** Experiment 6:
commit: 7ac5437
One more run of 100 count 1-98 successors after Experiment 5*. 

:LOGBOOK:
[missing log]
data directory: /Users/edechter/data/exp_2015-09-11-16-39-37/data/
:END:

Results: 
#+CAPTION: TRANSITION probabilities 1-99
#+NAME:   fig:SED-HR4049
https://www.dropbox.com/s/rz5rciq3o31431u/Screenshot%202015-09-11%2016.48.44.png?dl=0

Correct everywhere now except at the transitions. 
The mistakes: 
- twenty, twenty ; thirty, thirty; etc...
- twenty nine, twenty ten; ...

:LOGBOOK:
transition_probs:[1-0.911677462805283, 2-0.9959919767024176,
3-0.9998755160417777, 4-0.9824139690011261, 5-0.9826447208088298,
6-0.9746807409189961, 7-0.9998615729677117, 8-0.8757171683935869,
9-0.9472672684188188, 10-0.9953924505487585, 11-0.9376887441522864,
12-0.9901512638959299, 13-0.9954704305566492, 14-0.942508741362208,
15-0.942508741362208, 16-0.995662860481755, 17-0.9902368895033326,
18-0.978942970020516, 19-0.9885938923472515, 20-0.011492857197984202,
21-0.9968522917066619, 22-0.8541404644793412, 23-0.9147236015437279,
24-0.8481885759080233, 25-0.8486286517010172, 26-0.8515361218310231,
27-0.9147236015437278, 28-0.997087940195129, 29-0.020759877094622847,
30-0.01602613370275822, 31-0.9968470849328279, 32-0.852354334643844,
33-0.9196498271400195, 34-0.8463352436338558, 35-0.8480637286865867,
36-0.8497237113585889, 37-0.9218181420952868, 38-1.0,
39-0.02116449258784373, 40-0.00211180476834485, 41-0.6434823553950119,
42-0.6454695529630224, 43-0.6454695529630226, 44-0.8313511843811717,
45-0.6402197577138626, 46-0.6425830441112907, 47-0.6454695529630222,
48-0.6434823553950119, 49-0.021046900481418038,
50-0.030472100234483164, 51-0.9966518580251639, 52-0.7812363332030589,
53-0.6747115205388514, 54-0.7750918633675569, 55-0.7766914373590925,
56-0.7786827689465345, 57-0.6628745836898212, 58-0.9969308295947763,
59-0.02179330933820736, 60-0.06962759481290538, 61-0.8246474792569097,
62-0.7030456112235746, 63-0.8591426597428082, 64-0.8313511843575324,
65-0.6987553440234436, 66-0.7008759706045637, 67-0.8591426597428082,
68-0.8635420448842418, 69-0.02057233509027047,
70-0.025496905194668497, 71-0.9968522917066619, 72-0.8523381942886668,
73-0.5486180175865701, 74-0.8463184966289177, 75-0.8467611127253261,
76-0.8497073341355528, 77-0.7169704504243815, 78-0.997087940195129,
79-0.020895172631556116, 80-0.01427676950075687,
81-0.9966946355055951, 82-0.8525869015871699, 83-0.8508874497983598,
84-0.992975587633154, 85-0.847018822787599, 86-0.8499596925660188,
87-0.8878286038615864, 88-1.0, 89-0.021405601192379164,
90-0.07124416032128635, 91-0.9965967563001095, 92-0.7673745248885419,
93-0.5544006768511222, 94-0.7612590649889001, 95-0.7615895082040836,
96-0.7648597485093025, 97-0.5643839504575511]
:END:
** Experiment 7: 
commit:666564b
Since the previous experiments were unable to learn the transitions
between the decades, we add here just the decades, i.e., succ(20, 30), succ(30, 40), ...

We add a 100 count of each of those, training starting with the grammar from Experiment 6.

:LOGBOOK:
?- run_experiment('runner.pl', run_phase(phase), []), run_experiment('runner.pl', run_testing, []).
Correct to: "experiment:run_experiment('runner.pl',run_phase(phase),[])"? 
Correct to: "experiment:run_experiment('runner.pl',run_testing,[])"? 
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-11-17-08-00
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-11-17-08-00/data
% Experiment Info:     git_hash:         666564bb731b0f50558590261dba5b60253f8236
% Experiment Info:     start_time:       date(2015,9,11,17,8,0.08765602111816406,14400,EDT,true)
% Experiment Info:     random_seed:      11358634...
% Experiment Info:     ...............................................
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),8,/Users/edechter/data/exp_2015-09-11-17-08-00/data,phase)
% Online VBEM:     
:END:

RESULTS:
#+CAPTION: TRANSITION probabilities 1-99
https://www.dropbox.com/s/rvqf3t123eoi2hd/Screenshot%202015-09-14%2011.58.05.png?dl=0

Did not succeed in substantially changing the results from
Experiment 6. If anything, a little worse.

** Experiment 8
commit: 016ed26
Here, we start with Experiment 6, but train on the transitions
themselves. We train on succ(29, 30), succ(39, 40), etc. We add a 100
count of each of these.

:LOGBOOK:
CLOCK: [2015-09-14 Mon 12:30]--[2015-09-14 Mon 12:49] =>  0:19
:PROPERTIES:
:END:
% load learned grammar from experiment 6
?- load_gl('/Users/edechter/data/exp_2015-09-11-16-39-37/data/ovbem_gl_phase1_0001.gl.gz').
?- run_experiment('runner.pl', run_phase(phase), []), run_experiment('runner.pl', run_testing, []).
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/data/exp_2015-09-14-12-28-07
% Experiment Info:     data_path:        /Users/edechter/data/exp_2015-09-14-12-28-07/data
% Experiment Info:     git_hash:         016ed26331b9fedc7c2c2b7061f00982df8d7af9
% Experiment Info:     start_time:       date(2015,9,14,12,28,7.388531923294067,14400,EDT,true)
% Experiment Info:     random_seed:      49517316...
% Experiment Info:     ...............................................
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),8,/Users/edechter/data/exp_2015-09-14-12-28-07/data,phase)
% Online VBEM:     

:END:


RESULTS: 
#+CAPTION: TRANSITION probabilities 1-99
https://www.dropbox.com/s/8vb078rru901glb/Screenshot%202015-09-14%2012.35.31.png?dl=0
:LOGBOOK:
D = results{count_probs:[1-0.9097868779153935, 2-0.9062107467082636,
3-0.9060980083559553, 4-0.8897833592158845, 5-0.8705677264649659,
6-0.8475042868367454, 7-0.8473867479590088, 8-0.7411202423500726,
9-0.7056270077975041, 10-0.7024610472388881, 11-0.6583975867053455,
12-0.6519600296964568, 13-0.6490610229131171, 14-0.6114616359237223,
15-0.5760403398257404, 16-0.5736117260933892, 17-0.5681373259222153,
18-0.5560921079426524, 19-0.5490027415390101, 20-0.006231463624781259,
21-0.0055997973426418195, 22-0.0045612192083520995,
23-0.003715263141438097, 24-0.0030058736572854475,
25-0.0024328474835869315, 26-0.001975963619521926,
27-0.0016094873912198351, 28-0.0016048818367155153,
29-4.242342373912723e-5, 30-6.654036403893294e-7,
31-6.633274037439953e-7, 32-5.388656913990544e-7,
33-4.944128491258025e-7, 34-3.989110578205315e-7,
35-3.2197788323686485e-7, 36-2.608071853044738e-7,
37-2.398567399638298e-7, 38-2.398567399638298e-7,
39-6.453838025630731e-9, 40-1.3619184943433388e-11,
41-8.354372146773287e-12, 42-5.1396391692516865e-12,
43-3.161924119014592e-12, 44-2.6253622655065205e-12,
45-1.6021796986991506e-12, 46-9.811023185607667e-13,
47-6.035776018747422e-13, 48-3.70250637351823e-13,
49-9.918483334288972e-15, 50-2.8281766691281913e-16,
51-2.525114143154763e-16, 52-1.8748965357532695e-16,
53-1.2686154349330754e-16, 54-9.348059314651919e-17,
55-6.890315378293215e-17, 56-5.100414623476777e-17,
57-3.3910521872520164e-17, 58-3.3808310302383774e-17,
59-9.376271078049294e-19, 60-6.489572402269166e-20,
61-4.7601265106025904e-20, 62-3.1893796724520034e-20,
63-2.741027189848227e-20, 64-2.2758893262092458e-20,
65-1.513459916037402e-20, 66-1.0111129284584892e-20,
67-8.689740054626539e-21, 68-7.496344632393098e-21,
69-1.9679740534163923e-22, 70-4.944279666498345e-24,
71-4.4430916723368174e-24, 72-3.6093381372149855e-24,
73-1.989707519191222e-24, 74-1.605332226165767e-24,
75-1.2956993825584525e-24, 76-1.049511825016689e-24,
77-7.542897244632449e-25, 78-7.521313214480892e-25,
79-2.0016119866199667e-26, 80-2.798136832490861e-28,
81-2.7889890608308733e-28, 82-2.2664709934565048e-28,
83-1.924840853131731e-28, 84-1.9114529251416058e-28,
85-1.5433673635750057e-28, 86-1.2505915032782236e-28,
87-1.1087871984063961e-28, 88-1.1087871984063961e-28,
89-3.0115895308649114e-30, 90-2.133609244648663e-31,
91-1.9016052106169365e-31, 92-1.3867000648275998e-31,
93-7.552965919098833e-32, 94-5.46550523821645e-32,
95-3.9560312381814585e-32, 96-2.875998705115919e-32,
97-1.5958453111442607e-32], transition_probs:[1-0.9097868779153935,
2-0.9960692649081465, 3-0.999875593670989, 4-0.9819946087623871,
5-0.9784041446135247, 6-0.9735075871444578, 7-0.9998613117602327,
8-0.8745950348351722, 9-0.9521086693840389, 10-0.9955132661822313,
11-0.9372727346139126, 12-0.9902223866871952, 13-0.995553397982558,
14-0.9420711063181068, 15-0.9420711063181068, 16-0.9957839519831442,
17-0.9904562617496376, 18-0.9787987561633786, 19-0.9872514529474793,
20-0.011350514584522298, 21-0.8986327578600585, 22-0.8145329070427128,
23-0.8145329070427129, 24-0.8090607698172194, 25-0.8093645179298701,
26-0.8122020113684286, 27-0.8145329070427128, 28-0.9971384960643715,
29-0.026433985835336796, 30-0.015684817059581777,
31-0.9968797335642479, 32-0.8123676006110314, 33-0.9175066385135057,
34-0.8068379665412564, 35-0.8071420356106584, 36-0.8100158392326889,
37-0.9196707509565512, 38-1.0, 39-0.02690705304593052,
40-0.002110245855155683, 41-0.6134267345272687, 42-0.6152035220548286,
43-0.6152035220548289, 44-0.8303052719445747, 45-0.6102699500748847,
46-0.6123547310937394, 47-0.6152035220548286, 48-0.6134267345272688,
49-0.026788565187165738, 50-0.028514204982842118,
51-0.8928417275760746, 52-0.7424997166309714, 53-0.6766322358280901,
54-0.7368710057626775, 55-0.7370851153558156, 56-0.7402294878322666,
57-0.6648581414623216, 58-0.9969858449681006, 59-0.027733628194332408,
60-0.0692127216486077, 61-0.7335038759931468, 62-0.67001993861887,
63-0.8594232958602005, 64-0.8303052719208026, 65-0.6649971501726064,
66-0.6680804147795492, 67-0.8594232958602005, 68-0.8626661540240136,
69-0.02625244902578799, 70-0.02512370352604549, 71-0.8986327578600585,
72-0.8123483383624799, 73-0.5512665878200338, 74-0.8068181934691103,
75-0.8071222650610754, 76-0.8099963920213898, 77-0.7187053127783961,
78-0.9971384960643715, 79-0.026612533337479344,
80-0.013979416845998962, 81-0.996730763287282, 82-0.8126496533411629,
83-0.849267808275028, 84-0.9930446571890122, 85-0.8074315319382865,
86-0.8103006016541612, 87-0.8866102124473816, 88-1.0,
89-0.027161113829536607, 90-0.07084661514399349,
91-0.8912621724837295, 92-0.7292260544330933, 93-0.5446719237038363,
94-0.7236237124274691, 95-0.7238180306771463, 96-0.7269908986962352,
97-0.5548838767922669]}
:END:

Strangely, didn't improve the performance on the decade transition
probs at all. Still the most probable explanation is 29, twenty-ten;
thirty-nine, thirty-ten, etc.

** Experiment 9
commit: 93e4276

Starting with experiment 8, train an additional 1000 on transition
probabilities. If we still cannot correctly learn the transition
probabilities, something is wrong.

:LOGBOOK:
CLOCK: [2015-09-14 Mon 12:49]
% load learned grammar from experiment 8. 
?- load_gl('/Users/edechter/data/exp_2015-09-14-12-28-07/data/ovbem_gl_phase_0001.gl.gz').
?- run_experiment('runner.pl', run_phase(phase), []), run_experiment('runner.pl', run_testing, []).
?-  run_experiment('runner.pl', run_phase(phase), []), run_experiment('runner.pl', run_testing, []).
Correct to: "experiment:run_experiment('runner.pl',run_phase(phase),[])"? 
Correct to: "experiment:run_experiment('runner.pl',run_testing,[])"? 
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/Dropbox/Projects/SDCL/experiments/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/Dropbox/Projects/SDCL/experiments/data/exp_2015-09-14-12-52-59
% Experiment Info:     data_path:        /Users/edechter/Dropbox/Projects/SDCL/experiments/data/exp_2015-09-14-12-52-59/data
% Experiment Info:     git_hash:         651607063aa831dfa2ddf5d7aedff328553022e9
% Experiment Info:     start_time:       date(2015,9,14,12,52,59.458017110824585,14400,EDT,true)
% Experiment Info:     random_seed:      10784701...
% Experiment Info:     ...............................................
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),8,/Users/edechter/Dropbox/Projects/SDCL/experiments/data/exp_2015-09-14-12-52-59/data,phase)
% Online VBEM:     
% Online VBEM:     Running ...
% Online VBEM:     Iter 1 ...
:END:

RESULTS
:LOGBOOK:
D = results{count_probs:[1-0.8909676862745302, 2-0.8880337266684043,
3-0.8879027234453251, 4-0.8688407445777633, 5-0.8480662345950187,
6-0.8161781298824343, 7-0.8160425447413765, 8-0.6757656952460939,
9-0.6657867081345606, 10-0.663380948549582, 11-0.6201836991546984,
12-0.6144063114290534, 13-0.6124396293114837, 14-0.5746487341950193,
15-0.5391897452540406, 16-0.537384305187948, 17-0.5330596136830654,
18-0.5212938208909783, 19-0.5191865743267852, 20-0.005304014241552178,
21-0.0030983335776202546, 22-0.0017245688869694826,
23-0.0009599153129881873, 24-0.0005316297753654202,
25-0.00029427483716540793, 26-0.00016352057866333956,
27-9.10174760971475e-5, 28-4.9191152910048065e-5,
29-3.1174425941773028e-6, 30-3.8668205186880715e-8,
31-2.246824444143166e-8, 32-1.2426666048887915e-8,
33-6.476560588922513e-9, 34-3.5639138550899115e-9,
35-1.960070419752253e-9, 36-1.0822266804925743e-9,
37-5.654194320658992e-10, 38-2.9200953281261006e-10,
39-1.8722602295864334e-11, 40-3.792952716183974e-14,
41-1.5858908628330212e-14, 42-6.6421226531669044e-15,
43-2.7818934060129003e-15, 44-1.1579847860100167e-15,
45-4.815398835698449e-16, 46-2.013391040137746e-16,
47-8.432604380791018e-17, 48-3.5257993542394393e-17,
49-2.244056228672284e-18, 50-6.328678365672843e-20,
51-3.599931815092574e-20, 52-1.7871560881628588e-20,
53-7.807689741386063e-21, 54-3.854164773592747e-21,
55-1.901159235886275e-21, 56-9.421497394315235e-22,
57-4.0457662327669543e-22, 58-2.138749504101605e-22,
59-1.4013862899024094e-23, 60-8.788730725420425e-25,
61-3.9998830492454026e-25, 62-1.8235064554688365e-25,
63-8.517156410143936e-26, 64-3.860447538112157e-26,
65-1.7497475196831204e-26, 66-7.963363155724513e-27,
67-3.719493799688509e-27, 68-1.7271970042324142e-27,
69-1.0905161617593633e-28, 70-2.330184847779829e-30,
71-1.3611746928163229e-30, 72-7.527997400985242e-31,
73-3.0948668004553362e-31, 74-1.7029607645130678e-31,
75-9.365457380916121e-32, 76-5.170774747059479e-32,
77-2.7108746502311885e-32, 78-1.4651147796843238e-32,
79-9.331376942920195e-34, 80-1.1071064140497382e-35,
81-6.117880832227325e-36, 82-3.3864979595127624e-36,
83-1.489573252515382e-36, 84-8.203751050106308e-37,
85-4.515704890530934e-37, 86-2.495382922831954e-37,
87-1.1535686285639758e-37, 88-5.579792497377005e-38,
89-3.5960003770646623e-39, 90-2.4552910360907177e-40,
91-1.3866283852815e-40, 92-6.754759267337365e-41,
93-2.4896239766285933e-41, 94-1.2058307040604467e-41,
95-5.83586693408538e-42, 96-2.8378349025616052e-42,
97-1.0715201141026337e-42], transition_probs:[1-0.8909676862745302,
2-0.9967069966157877, 3-0.9998524794507854, 4-0.9785314557955227,
5-0.9760893925470306, 6-0.9623990398252182, 7-0.9998338780027378,
8-0.82810105870186, 9-0.9852330664581913, 10-0.9963865911476075,
11-0.9348831926974536, 12-0.9906843928121306, 13-0.9967990528726902,
14-0.9382944974365072, 15-0.9382944974365072, 16-0.9966515682429345,
17-0.9919523300864359, 18-0.9779278105298695, 19-0.9959576605750063,
20-0.010216008086167763, 21-0.5841488043805765, 22-0.5566117539526124,
23-0.5566117539526119, 24-0.5538298724608037, 25-0.5535334001244299,
26-0.5556729900471471, 27-0.5566117539526121, 28-0.5404583275583679,
29-0.06337405020528633, 30-0.012403822690786485,
31-0.5810521676101649, 32-0.5530768583758606, 33-0.5211824767353519,
34-0.5502787793239544, 35-0.5499769353159081, 36-0.5521366322284301,
37-0.5224593352370034, 38-0.5164476426741849, 39-0.06411640782932626,
40-0.0020258683361670327, 41-0.4181151154524698,
42-0.4188259614095686, 43-0.4188259614095681, 44-0.4162577845387965,
45-0.4158430139907551, 46-0.4181151154524698, 47-0.4188259614095681,
48-0.4181151154524698, 49-0.06364673661801029,
50-0.028201959847580393, 51-0.56882837254281, 52-0.4964416494418801,
53-0.4368778862182165, 54-0.49363702980704444, 55-0.4932739899737256,
56-0.4955659271709115, 57-0.42941860125207887, 58-0.5286389230251906,
59-0.06552362898108866, 60-0.06271454765004487, 61-0.4551149846559966,
62-0.45588994303542196, 63-0.46707574763995635,
64-0.45325544726575207, 65-0.45324991530354664, 66-0.4551149846559965,
67-0.4670757476399563, 68-0.46436345837626064, 69-0.06313791415148969,
70-0.021367724106174366, 71-0.5841488043805765, 72-0.5530515253269604,
73-0.41111422276132686, 74-0.5502533305351033, 75-0.5499514478593411,
76-0.5521112890434912, 77-0.524268563772346, 78-0.540458327558368,
79-0.06369041574292732, 80-0.011864341359500116,
81-0.5526009744490985, 82-0.5535410140180594, 83-0.4398565333048943,
84-0.5507450564282734, 85-0.5504439204639708, 86-0.5526009744490986,
87-0.4622812066273246, 88-0.4836983564924984, 89-0.06444684777713687,
90-0.06827838650269884, 91-0.5647511292548323, 92-0.487135510785471,
93-0.3685733092912976, 94-0.4843425012693532, 95-0.48397066971623875,
96-0.4862747788142229, 97-0.377583668851001]}
:END:

Instead of increasing the success probabilities on transitions, just
decreased the within decade probabilities...????
https://www.dropbox.com/s/islcpgkcbaexwu5/Screenshot%202015-09-14%2012.58.08.png?dl=0

This suggests to me that there is a bottle neck in the size of the
knowledge base and that we should increase the number of predicates. 

** Experiment 10

Following up on Experiment 9, I will try increasing the number of
predicates from 6 to 10. I will train in a single phase with the
following distribution.

1-9: count 1000
1-99: count 1000
decade transitions: count: 100

RESULTS:
   https://www.dropbox.com/s/g4r4lzvf7eag8pc/Screenshot%202015-09-15%2013.00.12.png?dl=0

** Experiment 10* 

commit:a13b252
Same as Experiment 10, but using 6 predicates instead of 10. Using same data scheme (unlike Experiment 9). 

RESULTS:
https://www.dropbox.com/s/tcn43tovx3ubcwm/Screenshot%202015-09-15%2014.06.40.png?dl=0

Failure to learn like in experiment 10, suggesting that the failure in
experiment 11 is due to the change in the number of predicates.

** Experiment 11
Same as Experiment 10* but set transition to count 1000


RESULTS:

https://www.dropbox.com/s/9r45jtizsqxhm5y/Screenshot%202015-09-15%2015.34.29.png?dl=0

Poor performance. Much more similar to Exp 10* than to Exp 10. Suggests that the grammar in 10* is maybe just too small.

** Experiment 12

:LOGBOOK:
?- run_experiment('runner.pl').
Correct to: "experiment:run_experiment('runner.pl')"? 
% Experiment Info:     Experiment settings ...........................
% Experiment Info:     root:             /Users/edechter/Dropbox/Projects/SDCL/experiments/data
% Experiment Info:     runner:           runner.pl
% Experiment Info:     ...............................................
% Experiment Info:     Experiment config .............................
% Experiment Info:     path:             /Users/edechter/Dropbox/Projects/SDCL/experiments/data/exp_2015-09-15-16-14-18
% Experiment Info:     data_path:        /Users/edechter/Dropbox/Projects/SDCL/experiments/data/exp_2015-09-15-16-14-18/data
% Experiment Info:     git_hash:         d9d060fc93a895a306e229f5bcd859354a7c2235
% Experiment Info:     start_time:       date(2015,9,15,16,14,18.810581922531128,14400,EDT,true)
% Experiment Info:     random_seed:      11929389...
% Experiment Info:     ...............................................
% Compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl...
% Success! Finished compiling rules in file /Users/edechter/Dropbox/Projects/SDCL/experiments/gls/succ_04.gl.
% Online VBEM:     Initializing with options: 
% Online VBEM:     online_vbem_options(1,normal(0.1,0.01),115,/Users/edechter/Dropbox/Projects/SDCL/experiments/data/exp_2015-09-15-16-14-18/data,phase)
% Online VBEM:     
% Online VBEM:     Running ...
% Online VBEM:     Iter 1 ...
% Online VBEM:     current goal: [count(succ([one],[two]),1000),count(succ([two],[three]),1000),count(succ([three],[four]),1000),count(succ([four],[five]),1000),count(succ([five],[six]),1000),count(succ([six],[seven]),1000),count(succ([seven],[eight]),1000),count(succ([eight],[nine]),1000),count(succ([nine],[ten]),1000),count(succ([one],[two]),1000),count(succ([two],[three]),1000),count(succ([three],[four]),1000),count(succ([four],[five]),1000),count(succ([five],[six]),1000),count(succ([six],[seven]),1000),count(succ([seven],[eight]),1000),count(succ([eight],[nine]),1000),count(succ([nine],[ten]),1000),count(succ([ten],[eleven]),1000),count(succ([eleven],[twelve]),1000),count(succ([twelve],[threeteen]),1000),count(succ([threeteen],[fourteen]),1000),count(succ([fourteen],[fiveteen]),1000),count(succ([fiveteen],[sixteen]),1000),count(succ([sixteen],[seventeen]),1000),count(succ([seventeen],[eightteen]),1000),count(succ([eightteen],[nineteen]),1000),count(succ([nineteen],[twenty]),1000),count(succ([twenty],[twenty,one]),1000),count(succ([twenty,one],[twenty,two]),1000),count(succ([twenty,two],[twenty,three]),1000),count(succ([twenty,three],[twenty,four]),1000),count(succ([twenty,four],[twenty,five]),1000),count(succ([twenty,five],[twenty,six]),1000),count(succ([twenty,six],[twenty,seven]),1000),count(succ([twenty,seven],[twenty,eight]),1000),count(succ([twenty,eight],[twenty,nine]),1000),count(succ([twenty,nine],[thirty]),1000),count(succ([thirty],[thirty,one]),1000),count(succ([thirty,one],[thirty,two]),1000),count(succ([thirty,two],[thirty,three]),1000),count(succ([thirty,three],[thirty,four]),1000),count(succ([thirty,four],[thirty,five]),1000),count(succ([thirty,five],[thirty,six]),1000),count(succ([thirty,six],[thirty,seven]),1000),count(succ([thirty,seven],[thirty,eight]),1000),count(succ([thirty,eight],[thirty,nine]),1000),count(succ([thirty,nine],[fourty]),1000),count(succ([fourty],[fourty,one]),1000),count(succ([fourty,one],[fourty,two]),1000),count(succ([fourty,two],[fourty,three]),1000),count(succ([fourty,three],[fourty,four]),1000),count(succ([fourty,four],[fourty,five]),1000),count(succ([fourty,five],[fourty,six]),1000),count(succ([fourty,six],[fourty,seven]),1000),count(succ([fourty,seven],[fourty,eight]),1000),count(succ([fourty,eight],[fourty,nine]),1000),count(succ([fourty,nine],[fifty]),1000),count(succ([fifty],[fifty,one]),1000),count(succ([fifty,one],[fifty,two]),1000),count(succ([fifty,two],[fifty,three]),1000),count(succ([fifty,three],[fifty,four]),1000),count(succ([fifty,four],[fifty,five]),1000),count(succ([fifty,five],[fifty,six]),1000),count(succ([fifty,six],[fifty,seven]),1000),count(succ([fifty,seven],[fifty,eight]),1000),count(succ([fifty,eight],[fifty,nine]),1000),count(succ([fifty,nine],[sixty]),1000),count(succ([sixty],[sixty,one]),1000),count(succ([sixty,one],[sixty,two]),1000),count(succ([sixty,two],[sixty,three]),1000),count(succ([sixty,three],[sixty,four]),1000),count(succ([sixty,four],[sixty,five]),1000),count(succ([sixty,five],[sixty,six]),1000),count(succ([sixty,six],[sixty,seven]),1000),count(succ([sixty,seven],[sixty,eight]),1000),count(succ([sixty,eight],[sixty,nine]),1000),count(succ([sixty,nine],[seventy]),1000),count(succ([seventy],[seventy,one]),1000),count(succ([seventy,one],[seventy,two]),1000),count(succ([seventy,two],[seventy,three]),1000),count(succ([seventy,three],[seventy,four]),1000),count(succ([seventy,four],[seventy,five]),1000),count(succ([seventy,five],[seventy,six]),1000),count(succ([seventy,six],[seventy,seven]),1000),count(succ([seventy,seven],[seventy,eight]),1000),count(succ([seventy,eight],[seventy,nine]),1000),count(succ([seventy,nine],[eightty]),1000),count(succ([eightty],[eightty,one]),1000),count(succ([eightty,one],[eightty,two]),1000),count(succ([eightty,two],[eightty,three]),1000),count(succ([eightty,three],[eightty,four]),1000),count(succ([eightty,four],[eightty,five]),1000),count(succ([eightty,five],[eightty,six]),1000),count(succ([eightty,six],[eightty,seven]),1000),count(succ([eightty,seven],[eightty,eight]),1000),count(succ([eightty,eight],[eightty,nine]),1000),count(succ([eightty,nine],[ninety]),1000),count(succ([ninety],[ninety,one]),1000),count(succ([ninety,one],[ninety,two]),1000),count(succ([ninety,two],[ninety,three]),1000),count(succ([ninety,three],[ninety,four]),1000),count(succ([ninety,four],[ninety,five]),1000),count(succ([ninety,five],[ninety,six]),1000),count(succ([ninety,six],[ninety,seven]),1000),count(succ([ninety,seven],[ninety,eight]),1000),count(succ([ninety,eight],[ninety,nine]),1000),count(succ([nineteen],[twenty]),1000),count(succ([twenty,nine],[thirty]),1000),count(succ([thirty,nine],[fourty]),1000),count(succ([fourty,nine],[fifty]),1000),count(succ([fifty,nine],[sixty]),1000),count(succ([sixty,nine],[seventy]),1000),count(succ([seventy,nine],[eightty]),1000),count(succ([eightty,nine],[ninety]),1000)]
% Batch VBEM:     Initializing with options: 
% Batch VBEM:     vbem_options(10,1.0e-6,uniform(0.0,1.0),/Users/edechter/Dropbox/Projects/SDCL/experiments/data/exp_2015-09-15-16-14-18/data)
:END:





 


---

** Experiment ...: 

In this experiment, we interleave training and testing sessions to
evaluate the learning trajectory. We will interleave 100 counts of
1-98 on the succ_04 grammar with 6 predicates of each type (type 1 and
2). 

sciprt: runner.

PWD: /Users/edechter/Dropbox/Projects/SDCL/experiments/scripts/learn_successor/
script: runner_0001.pl   

To Be Done ...


