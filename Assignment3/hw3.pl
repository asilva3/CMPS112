% Assignment #3
% Author : Armando Silva asilva3
% Partner: Kevin Thai kjthai
% Time programming together : 6
% Additional individual effort : 2

 % 7
father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

% 8
grandma(X,Y):-mother(X,Z), mother(Z,Y).
grandma(X,Y):-mother(X,Z), father(Z,Y).

% 9
descendants(X,Y) :- mother(X,Y).
descendants(X,Y) :- father(X,Y).
descendants(X,Z) :- mother(X,Y), descendants(Y,Z).
descendants(X,Z) :- father(X,Y), descendants(Y,Z).
% 10
siblings(X,Y):- father(Z,X), father(Z,Y),X\=Y.
siblings(X,Y):- mother(Z,X), mother(Z,Y),X\=Y.

%    ASCII-ART for the NFA:
%
%    (q0)  ---a-->  (q1)  ---b-->  (q2*)
%     |
%     a
%     |
%     V  / --<-- \
%    (q3*)        a
%        \ -->-- /

% 11
%  Transition relation:
transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).

%  Accepting states:
accepting(q2).
accepting(q3).

accepts(State, []) :- accepting(State).
accepts(State,[H|T]):- transition(State,X,H), accepts(X,T).