

macro(
      ( ('?A'(Xi-Xf, Yi-Yf) ---> '?B'(Xi-Xm, Xm-Xf), '?C'(Yi-Ym, Ym-Yf))
      :-
      (member('?A', [a1, a2, a3]),
        member('?B', [a1, a2, a3]),
      member('?C', [a1, a2, a3]))   
      )
     ).
