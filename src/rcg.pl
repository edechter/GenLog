
% palindrome rcg
s(X1-X2, Y1-Y2) ---> s(X1-Z, U-Y2), s(Z-X2, Y1-U).
s([a|X]-X, [a|Y]-Y).
s([b|X]-X, [b|Y]-Y).

