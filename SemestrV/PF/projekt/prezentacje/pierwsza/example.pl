list_member(X, [X|_]).
list_member(X, [_|Y]) :-
  list_member(X,Y).

list_append([], X, X).
list_append([X|Y], Z, [X|R]) :-
  list_append(Y,Z,R).

parent(julia, tomasz).
parent(julia, agata).
parent(agata, wladyslaw).
parent(tomasz, stanislawa).
