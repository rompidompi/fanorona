:- dynamic cell/2.

cell(a5,n).
cell(b5,n).
cell(c5,n).
cell(d5,n).
cell(e5,n).
cell(f5,n).
cell(g5,n).
cell(h5,n).
cell(i5,n).
cell(a4,n).
cell(b4,n).
cell(c4,n).
cell(d4,n).
cell(e4,n).
cell(f4,n).
cell(g4,n).
cell(h4,n).
cell(i4,n).
cell(a3,n).
cell(b3,b). 
cell(c3,n). 
cell(d3,b). 
cell(e3,-). 
cell(f3,n). 
cell(g3,b). 
cell(h3,n). 
cell(i3,b).
cell(a2,b). 
cell(b2,b). 
cell(c2,b). 
cell(d2,b). 
cell(e2,b). 
cell(f2,b). 
cell(g2,b). 
cell(h2,b). 
cell(i2,b).
cell(a1,b). 
cell(b1,b). 
cell(c1,b). 
cell(d1,b). 
cell(e1,b). 
cell(f1,b). 
cell(g1,b).
cell(h1,b).
cell(i1,b).

new_game :-
	retract( cell(a5,A) ),
	retract( cell(b5,B) ),
	retract( cell(c5,C) ),
	retract( cell(d5,D) ),
	retract( cell(e5,E) ),
	retract( cell(f5,F) ),
	retract( cell(g5,G) ),
	retract( cell(h5,H) ),
	retract( cell(i5,I) ),
	retract( cell(a4,J) ),
	retract( cell(b4,K) ),
	retract( cell(c4,L) ),
	retract( cell(d4,M) ),
	retract( cell(e4,N) ),
	retract( cell(f4,O) ),
	retract( cell(g4,P) ),
	retract( cell(h4,Q) ),
	retract( cell(i4,R) ),
	retract( cell(a3,S) ),
	retract( cell(b3,T) ), 
	retract( cell(c3,U) ), 
	retract( cell(d3,V) ), 
	retract( cell(e3,W) ), 
	retract( cell(f3,X) ), 
	retract( cell(g3,Y) ), 
	retract( cell(h3,Z) ), 
	retract( cell(i3,AA) ),
	retract( cell(a2,BB) ), 
	retract( cell(b2,CC) ), 
	retract( cell(c2,DD) ), 
	retract( cell(d2,EE) ), 
	retract( cell(e2,FF) ), 
	retract( cell(f2,GG) ), 
	retract( cell(g2,HH) ), 
	retract( cell(h2,II) ), 
	retract( cell(i2,JJ) ),
	retract( cell(a1,KK) ), 
	retract( cell(b1,LL) ), 
	retract( cell(c1,MM) ), 
	retract( cell(d1,NN) ), 
	retract( cell(e1,OO) ), 
	retract( cell(f1,PP) ), 
	retract( cell(g1,QQ) ),
	retract( cell(h1,RR) ),
	retract( cell(i1,SS) ),
	assert( cell(a5,n) ),
	assert( cell(b5,n) ),
	assert( cell(c5,n) ),
	assert( cell(d5,n) ),
	assert( cell(e5,n) ),
	assert( cell(f5,n) ),
	assert( cell(g5,n) ),
	assert( cell(h5,n) ),
	assert( cell(i5,n) ),
	assert( cell(a4,n) ),
	assert( cell(b4,n) ),
	assert( cell(c4,n) ),
	assert( cell(d4,n) ),
	assert( cell(e4,n) ),
	assert( cell(f4,n) ),
	assert( cell(g4,n) ),
	assert( cell(h4,n) ),
	assert( cell(i4,n) ),
	assert( cell(a3,n) ),
	assert( cell(b3,b) ), 
	assert( cell(c3,n) ), 
	assert( cell(d3,b) ), 
	assert( cell(e3,-) ), 
	assert( cell(f3,n) ), 
	assert( cell(g3,b) ), 
	assert( cell(h3,n) ), 
	assert( cell(i3,b) ),
	assert( cell(a2,b) ), 
	assert( cell(b2,b) ), 
	assert( cell(c2,b) ), 
	assert( cell(d2,b) ), 
	assert( cell(e2,b) ), 
	assert( cell(f2,b) ), 
	assert( cell(g2,b) ), 
	assert( cell(h2,b) ), 
	assert( cell(i2,b) ),
	assert( cell(a1,b) ), 
	assert( cell(b1,b) ), 
	assert( cell(c1,b) ), 
	assert( cell(d1,b) ), 
	assert( cell(e1,b) ), 
	assert( cell(f1,b) ), 
	assert( cell(g1,b) ),
	assert( cell(h1,b) ),
	assert( cell(i1,b) ).
	
	

/* Liaison des cases */
haut(a1, a2).
haut(b1, b2).
haut(c1, c2).
haut(d1, d2).
haut(e1, e2).
haut(f1, f2).
haut(g1, g2).
haut(h1, h2).
haut(i1, i2).
haut(a2, a3).
haut(b2, b3).
haut(c2, c3).
haut(d2,d3).
haut(e2,e3).
haut(f2,f3).
haut(g2,g3).
haut(h2,h3).
haut(i2,i3).
haut(a3,a4).
haut(b3,b4).
haut(c3,c4).
haut(d3,d4).
haut(e3,e4).
haut(f3,f4).
haut(g3,g4).
haut(h3,h4).
haut(i3,i4).
haut(a4,a5).
haut(b4,b5).
haut(c4,c5).
haut(d4,d5).
haut(e4,e5).
haut(f4,f5).
haut(g4,g5).
haut(h4,h5).
haut(i4,i5).

hautGauche(c1,b2).
hautGauche(e1,d2).
hautGauche(g1,f2).
hautGauche(i1,h2).
hautGauche(b2,a3).
hautGauche(d2,c3).
hautGauche(f2,e3).
hautGauche(h2,g3).
hautGauche(c3,b4).
hautGauche(e3,d4).
hautGauche(g3,f4).
hautGauche(i3,h4).
hautGauche(b4,a5).
hautGauche(d4,c5).
hautGauche(f4,e5).
hautGauche(h4,g5).

hautDroite(a1,b2).
hautDroite(c1,d2).
hautDroite(e1,f2).
hautDroite(g1,h2).
hautDroite(b2,c3).
hautDroite(d2,e3).
hautDroite(f2,g3).
hautDroite(h2,i3).
hautDroite(a3,b4).
hautDroite(c3,d4).
hautDroite(e3,f4).
hautDroite(g3,h4).
hautDroite(b4,c5).
hautDroite(d4,e5).
hautDroite(f4,g5).
hautDroite(h4,i5).

droite(a1,b1).
droite(b1,c1).
droite(c1,d1).
droite(d1,e1).
droite(e1,f1).
droite(f1,g1).
droite(g1,h1).
droite(h1,i1).
droite(a2,b2).
droite(b2,c2).
droite(c2,d2).
droite(d2,e2).
droite(e2,f2).
droite(f2,g2).
droite(g2,h2).
droite(h2,i2).
droite(a3,b3).
droite(b3,c3).
droite(c3,d3).
droite(d3,e3).
droite(e3,f3).
droite(f3,g3).
droite(g3,h3).
droite(h3,i3).
droite(a4,b4).
droite(b4,c4).
droite(c4,d4).
droite(d4,e4).
droite(e4,f4).
droite(f4,g4).
droite(g4,h4).
droite(h4,i4).
droite(a5,b5).
droite(b5,c5).
droite(c5,d5).
droite(d5,e5).
droite(e5,f5).
droite(f5,g5).
droite(g5,h5).
droite(h5,i5).

/* Liaison inverse */
gauche(X, Y) :- 
    droite(Y, X).
bas(X, Y) :-
    haut(Y,X).
basGauche(X,Y) :-
    hautDroite(Y,X).
basDroite(X,Y) :-
    hautGauche(Y,X).
	




/* Boucle pour déterminer nombre de pions adverses */
verifPion(X, Y, C) :- 
<<<<<<< Updated upstream
    verifPion_droite(X, Y, C);
    verifPion_bas(X, Y, C);
    verifPion_basDroite(X, Y, C);
    verifPion_basGauche(X, Y, C);
    verifPion_gauche(X, Y, C);
    verifPion_haut(X, Y, C);
    verifPion_hautDroite(X, Y, C);
    verifPion_hautGauche(X, Y, C).


=======
    verifPion_droite(X, Y, C),!;
    verifPion_bas(X, Y, C),!;
    verifPion_basDroite(X, Y, C),!;
    verifPion_basGauche(X, Y, C),!;
    verifPion_gauche(X, Y, C),!;
    verifPion_haut(X, Y, C),!;
    verifPion_hautDroite(X, Y, C),!;
    verifPion_hautGauche(X, Y, C),!.
/* Boucle pour déterminer nombre de pions adverses avec aspiration-collision */
verifPion2(X, Y, C, T) :- 
    verifPion_droite2(X, Y, C, T),!;
    verifPion_bas2(X, Y, C, T),!;
    verifPion_basDroite2(X, Y, C, T),!;
    verifPion_basGauche2(X, Y, C, T),!;
    verifPion_gauche2(X, Y, C, T),!;
    verifPion_haut2(X, Y, C, T),!;
    verifPion_hautDroite2(X, Y, C, T),!;
    verifPion_hautGauche2(X, Y, C, T),!.

head([H|_], H).

find_best_move(Move, Score, Type):-
	find_all_playable_moves(Moves),
    return_best_move(Moves, Move, Score, Type).

%base case : la liste est vide -> retourner 0 comme valeur de BestScore.
return_best_move([], [], 0, 0).
return_best_move([H|T], BestMove, BestScore, Type) :-
    get_score(H, C, Te),
    TempScore = C,
    TempBestMove = H,
    TempType = Te,
    return_best_move(T, NewBestMove, NewBestScore, NewType),
    eval_score(TempBestMove, TempScore, TempType, NewBestMove, NewBestScore, NewType, BestMove, BestScore, Type),!.

% eval_score(+score1, +score2, -BestScore)
eval_score(Move1, Score1, Type1, _, Score2, _, Move1, Score1, Type1) :-
    Score1 > Score2,!.
eval_score(_, Score1, _, Move2, Score2, Type2, Move2, Score2, Type2) :-
    Score1 < Score2,!.
eval_score(Move1, Score1, Type1, Move2, Score2, Type2, Move, Score, Type):-
    Score1 == Score2,
    random(1,10,R),
    get_random_move(R, Move1, Score1, Type1, Move2, Score2, Type2, Move, Score, Type),!.

get_random_move(R, Move1, Score1, Type1, _, _, _, Move1, Score1, Type1):-
    R < 6,!.
get_random_move(_, _, _, _, Move2, Score2, Type2, Move2, Score2, Type2).
    
get_score([H|T], C, Type):-
    head(T, L),!,
    verifPion2(H, L, C, Type).

case_a_compter(X, 0) :-
    cell(X,C),
    C == -,!.
case_a_compter(X, 1) :-
    cell(X,C),
    C \= -,!.

eval_aspiration_vs_collision(C1, C2, C1,c) :-
    C1 >= C2.
eval_aspiration_vs_collision(C1,C2,C2,a) :-
    C2 > C1.
    
>>>>>>> Stashed changes
verifPion_droite(X,Y,C) :- droite(X,Y), cell(X, B), nbrPion_droite(Y, B, C).
verifPion_haut(X,Y,C) :- haut(X,Y), cell(X, B), nbrPion_haut(Y, B, C).
verifPion_gauche(X,Y,C) :- gauche(X,Y), cell(X, B), nbrPion_gauche(Y, B, C).
verifPion_hautGauche(X,Y,C) :- hautGauche(X,Y), cell(X, B), nbrPion_hautGauche(Y, B, C).
verifPion_hautDroite(X,Y,C) :- hautDroite(X,Y), cell(X, B), nbrPion_hautDroite(Y, B, C).
verifPion_bas(X,Y,C) :- bas(X,Y), cell(X, B), nbrPion_bas(Y, B, C).
verifPion_basGauche(X,Y,C) :- basGauche(X,Y), cell(X, B), nbrPion_basGauche(Y, B, C).
verifPion_basDroite(X,Y,C) :- basDroite(X,Y), cell(X, B), nbrPion_basDroite(Y, B, C).

<<<<<<< Updated upstream

nbrPion_droite(X, B, Count) :- compteur_droite(X, B, Count).
compteur_droite(X, C, 0) :- cell(X, B), C \= B.
compteur_droite(X, C, Count) :- !, droite(X,Y), cell(Y, B), C \= B, B \= -, compteur_droite(Y, C, Count2), Count is Count2+1.

nbrPion_gauche(X, B, Count) :- compteur_gauche(X, B, Count).
compteur_gauche(X, C, 0) :- cell(X, B), C \= B.
compteur_gauche(X, C, Count) :- gauche(X,Y), cell(Y, B), C \= B, B \= -, compteur_gauche(Y, C, Count2), Count is Count2+1.

nbrPion_haut(X, B, Count) :- compteur_haut(X, B, Count).
compteur_haut(X, C, 0) :- cell(X, B), C \= B.
compteur_haut(X, C, Count) :- haut(X,Y), cell(Y, B), C \= B, B \= -, compteur_haut(Y, C, Count2), Count is Count2+1.

nbrPion_hautGauche(X, B, Count) :- compteur_hautGauche(X, B, Count).
compteur_hautGauche(X, C, 0) :- cell(X, B), C \= B.
compteur_hautGauche(X, C, Count) :- hautGauche(X,Y), cell(Y, B), C \= B, B \= -, compteur_hautGauche(Y, C, Count2), Count is Count2+1.

nbrPion_hautDroite(X, B, Count) :- compteur_hautDroite(X, B, Count).
compteur_hautDroite(X, C, 0) :- cell(X, B), C \= B.
compteur_hautDroite(X, C, Count) :- hautDroite(X,Y), cell(Y, B), C \= B, B \= -, compteur_hautDroite(Y, C, Count2), Count is Count2+1.

nbrPion_bas(X, B, Count) :- compteur_bas(X, B, Count).
compteur_bas(X, C, 0) :- cell(X, B), C \= B.
compteur_bas(X, C, Count) :- bas(X,Y), cell(Y, B), C \= B, B \= -, compteur_bas(Y, C, Count2), Count is Count2+1.

nbrPion_basGauche(X, B, Count) :- compteur_basGauche(X, B, Count).
compteur_basGauche(X, C, 0) :- cell(X, B), C \= B.
compteur_basGauche(X, C, Count) :- basGauche(X,Y), cell(Y, B), C \= B, B \= -, compteur_basGauche(Y, C, Count2), Count is Count2+1.

nbrPion_basDroite(X, B, Count) :- compteur_basDroite(X, B, Count).
compteur_basDroite(X, C, 0) :- cell(X, B), C \= B.
compteur_basDroite(X, C, Count) :- basDroite(X,Y), cell(Y, B), C \= B, B \= -, compteur_basDroite(Y, C, Count2), Count is Count2+1.

=======
verifPion_droite2(X,Y,C,T) :-
    droite(X,Y), cell(X,B), gauche(X,Z), 
    nbrPion_droite(Y,B,C1), nbrPion_gauche(Z,B,C2),
    eval_aspiration_vs_collision(C1,C2,C,T),!.
verifPion_droite2(X,Y,C,c) :-
    droite(X,Y), cell(X,B), nbrPion_droite(Y,B,C),!.
%verifPion_droite2(X,_,C,a) :-
    %gauche(X,Z), cell(X,B), nbrPion_gauche(Z,B,C),!.

verifPion_haut2(X,Y,C,T) :- 
    haut(X,Y), bas(X,Z), cell(X, B), 
    nbrPion_haut(Y, B, C1), nbrPion_bas(Z,B,C2),
    eval_aspiration_vs_collision(C1,C2,C,T),!.
verifPion_haut2(X,Y,C,c) :- haut(X,Y), cell(X, B), nbrPion_haut(Y, B, C),!. 
%verifPion_haut2(X,_,C,a) :- bas(X,Z), cell(X, B), nbrPion_bas(Z,B,C),!.

verifPion_gauche2(X,Y,C,T) :- 
    gauche(X,Y), droite(X,Z), cell(X, B), 
    nbrPion_gauche(Y, B, C1), nbrPion_droite(Z,B,C2),
    eval_aspiration_vs_collision(C1,C2,C,T),!.
verifPion_gauche2(X,Y,C,c) :- gauche(X,Y), cell(X, B), nbrPion_gauche(Y, B, C),!.
%verifPion_gauche2(X,_,C,a) :- droite(X,Z), cell(X, B), nbrPion_droite(Z,B,C),!.

verifPion_hautGauche2(X,Y,C,T) :-
    hautGauche(X,Y), basDroite(X,Z), cell(X, B), 
    nbrPion_hautGauche(Y, B, C1), nbrPion_basDroite(Z,B,C2), 
    eval_aspiration_vs_collision(C1,C2,C,T),!.
verifPion_hautGauche2(X,Y,C,c) :- hautGauche(X,Y), cell(X, B), nbrPion_hautGauche(Y, B, C),!.
%verifPion_hautGauche2(X,_,C,a) :- basDroite(X,Z), cell(X, B), nbrPion_basDroite(Z,B,C),!.

verifPion_hautDroite2(X,Y,C,T) :-
    hautDroite(X,Y), basGauche(X,Z), cell(X, B), 
    nbrPion_hautDroite(Y, B, C1), nbrPion_basGauche(Z,B,C2), 
    eval_aspiration_vs_collision(C1,C2,C,T),!.
verifPion_hautDroite2(X,Y,C,c) :- hautDroite(X,Y), cell(X, B), nbrPion_hautDroite(Y, B, C),!.
%verifPion_hautDroite2(X,_,C,a) :- basGauche(X,Z), cell(X, B), nbrPion_basGauche(Z,B,C),!.

verifPion_bas2(X,Y,C,T) :-
    bas(X,Y), haut(X,Z), cell(X, B), 
    nbrPion_bas(Y, B, C1), nbrPion_haut(Z,B,C2), 
    eval_aspiration_vs_collision(C1,C2,C,T),!.
verifPion_bas2(X,Y,C,c) :- bas(X,Y), cell(X, B), nbrPion_bas(Y, B, C),!.
%verifPion_bas2(X,_,C,a) :- haut(X,Z), cell(X, B), nbrPion_haut(Z,B,C),!. 

verifPion_basGauche2(X,Y,C,T) :-
    basGauche(X,Y), hautDroite(X,Z), cell(X, B), 
    nbrPion_basGauche(Y, B, C1), nbrPion_hautDroite(Z,B,C2), 
    eval_aspiration_vs_collision(C1,C2,C,T),!.
verifPion_basGauche2(X,Y,C,c) :- basGauche(X,Y), cell(X, B), nbrPion_basGauche(Y, B, C),!.
%verifPion_basGauche2(X,_,C,a) :- hautDroite(X,Z), cell(X, B), nbrPion_hautDroite(Z,B,C),!. 

verifPion_basDroite2(X,Y,C,T) :-
    basDroite(X,Y), hautGauche(X,Z), cell(X, B), 
    nbrPion_basDroite(Y, B, C1), nbrPion_hautGauche(Z,B,C2), 
    eval_aspiration_vs_collision(C1,C2,C,T),!.
verifPion_basDroite2(X,Y,C,c) :- basDroite(X,Y), cell(X, B), nbrPion_basDroite(Y, B, C),!.
%verifPion_basDroite2(X,_,C,a) :- hautGauche(X,Z), cell(X, B), nbrPion_hautGauche(Z,B,C),!.

nbrPion_droite(X, B, Count) :- compteur_droite2(X, B, Count),!.
%base case : la case de départ à la même couleur que la case courante ou il n'y a pas de cases à gauche
compteur_droite2(X, C, 0) :- cell(X, B), B == C,!.
compteur_droite2(X, _, 1) :- not(droite(X,_)),!.
compteur_droite2(X,_,0):- droite(X,Y), cell(Y,-),!. 
compteur_droite2(X,C,Count) :- 
    droite(X,Y),
    compteur_droite2(Y, C, Count2),
    case_a_compter(X, Z),
    Count is Count2 + Z.

nbrPion_gauche(X, B, Count) :- compteur_gauche2(X, B, Count),!.
%base case : la case de départ à la même couleur que la case courante ou il n'y a pas de cases à gauche
compteur_gauche2(X, C, 0) :- cell(X, B), B == C,!.
compteur_gauche2(X, _, 1) :- not(gauche(X,_)),!.
compteur_gauche2(X,_,0):- gauche(X,Y), cell(Y,-),!.
compteur_gauche2(X,C,Count) :- 
    gauche(X,Y),
    compteur_gauche2(Y, C, Count2),
    case_a_compter(X, Z),
    Count is Count2 + Z.

nbrPion_haut(X, B, Count) :- compteur_haut2(X, B, Count).
%base case : la case de départ à la même couleur que la case courante ou il n'y a pas de cases plus haut à gauche
compteur_haut2(X, C, 0) :- cell(X, B), B == C,!.
compteur_haut2(X,_,1) :- not(haut(X,_)),!.
compteur_haut2(X,_,0):- haut(X,Y), cell(Y,-),!. 
compteur_haut2(X,C,Count) :- 
    haut(X,Y),
    compteur_haut2(Y, C, Count2),
    case_a_compter(X, Z),
    Count is Count2 + Z.

nbrPion_hautGauche(X, B, Count) :- compteur_hautGauche2(X, B, Count).
%base case : la case de départ à la même couleur que la case courante ou il n'y a pas de cases plus haut
compteur_hautGauche2(X, C, 0) :- cell(X, B), B == C,!.
compteur_hautGauche2(X,_,1) :- not(hautGauche(X,_)),!.
compteur_hautGauche2(X,_,0):- hautGauche(X,Y), cell(Y,-),!. 
compteur_hautGauche2(X,C,Count) :- 
    hautGauche(X,Y),
    compteur_hautGauche2(Y, C, Count2),
    case_a_compter(X, Z),
    Count is Count2 + Z.

nbrPion_hautDroite(X, B, Count) :- compteur_hautDroite2(X, B, Count).
%base case : la case de départ à la même couleur que la case courante ou il n'y a pas de cases plus haut
compteur_hautDroite2(X, C, 0) :- cell(X, B), B == C,!.
compteur_hautDroite2(X,_,1) :- not(hautDroite(X,_)),!.
compteur_hautDroite2(X,_,0):- hautDroite(X,Y), cell(Y,-),!. 
compteur_hautDroite2(X,C,Count) :- 
    hautDroite(X,Y),
    compteur_hautDroite2(Y, C, Count2),
    case_a_compter(X, Z),
    Count is Count2 + Z.

nbrPion_bas(X, B, Count) :- compteur_bas2(X, B, Count).
%base case : la case de départ à la même couleur que la case courante ou il n'y a pas de cases plus haut
compteur_bas2(X, C, 0) :- cell(X, B), B == C,!.
compteur_bas2(X, _, 1) :- not(bas(X,_)),!.
compteur_bas2(X,_,0):- bas(X,Y), cell(Y,-),!. 
compteur_bas2(X,C,Count) :- 
    bas(X,Y),
    compteur_bas2(Y, C, Count2),
    case_a_compter(X, Z),
    Count is Count2 + Z.

nbrPion_basGauche(X, B, Count) :- compteur_basGauche2(X, B, Count).
%base case : la case de départ à la même couleur que la case courante ou il n'y a pas de cases plus haut
compteur_basGauche2(X, C, 0) :- cell(X, B), B == C,!.
compteur_basGauche2(X, _, 1) :- not(basGauche(X,_)),!.
compteur_basGauche2(X,_,0):- basGauche(X,Y), cell(Y,-),!. 
compteur_basGauche2(X,C,Count) :- 
    basGauche(X,Y),
    compteur_basGauche2(Y, C, Count2),
    case_a_compter(X, Z),
    Count is Count2 + Z.

nbrPion_basDroite(X, B, Count) :- compteur_basDroite2(X, B, Count).
%base case : la case de départ à la même couleur que la case courante ou il n'y a pas de cases plus haut
compteur_basDroite2(X, C, 0) :- cell(X, B), B == C,!.
compteur_basDroite2(X, _, 1) :- not(basDroite(X,_)),!.
compteur_basDroite2(X,_,0):- basDroite(X,Y), cell(Y,-),!. 
compteur_basDroite2(X,C,Count) :- 
    basDroite(X,Y),
    compteur_basDroite2(Y, C, Count2),
    case_a_compter(X, Z),
    Count is Count2 + Z.
>>>>>>> Stashed changes

/* fonction déterminant si un coup est permis */
can_play(X, Y) :-
	cell(X, b),
	cell(Y, -),
	droite(X,Y);
	gauche(X,Y); 
	hautDroite(X,Y); 
	basDroite(X,Y); 
	bas(X,Y); 
	haut(X,Y); 
	hautGauche(X,Y); 
	basGauche(X,Y).
	

	
/* fonction déterminant si un coup est permis pour le joueur ordinateur */
can_playComp(X, Y) :-
	cell(X, n),
	cell(Y, -),
	droite(X,Y); 
	gauche(X,Y); 
	hautDroite(X,Y); 
	basDroite(X,Y); 
	bas(X,Y); 
	haut(X,Y); 
	hautGauche(X,Y); 
	basGauche(X,Y).

/* fonction prenant en charge l'élimination de cellules */	
eliminate(X, A) :-
	retract( cell(X, A) ),
	assert( cell(X, -) ).
	
/* Liaison d'élimination */
elim_droite(X, C) :- droite(X,Y), cell(Y, B), C \= B, B \= -, eliminate(Y, B), elim_droite(Y, C).
elim_gauche(X, C) :- gauche(X,Y), cell(Y, B), C \= B, B \= -, eliminate(Y, B), elim_gauche(Y, C).
elim_haut(X, C) :- haut(X,Y), cell(Y, B), C \= B, B \= -, eliminate(Y, B), elim_haut(Y, C).
elim_hautGauche(X, C) :- hautGauche(X,Y), cell(Y, B), C \= B, B \= -, eliminate(Y, B), elim_hautGauche(Y, C).
elim_hautDroite(X, C) :- hautDroite(X,Y), cell(Y, B), C \= B, B \= -, eliminate(Y, B), elim_hautDroite(Y, C).
elim_bas(X, C) :- bas(X,Y), cell(Y, B), C \= B, B \= -, eliminate(Y, B), elim_bas(Y, C).
elim_basGauche(X, C) :- basGauche(X,Y), cell(Y, B), C \= B, B \= -, eliminate(Y, B), elim_basGauche(Y, C).
elim_basDroite(X, C) :- basDroite(X,Y), cell(Y, B), C \= B, B \= -, eliminate(Y, B), elim_basDroite(Y, C).

%verif_droite(X, Y) :- droite(X,Y), cell(Y, B), not(elim_gauche(X,B)), elim_droite(Y, B). ------ Version destruction avant et arriere
jouer_coup(X,Y) :-
    jouer_bas(X,Y);
    jouer_basDroite(X,Y);
    jouer_basGauche(X,Y);
    jouer_droite(X,Y);
    jouer_gauche(X,Y);
    jouer_haut(X,Y);
    jouer_hautDroite(X,Y);
    jouer_hautGauche(X,Y).

jouer_droite(X, Y) :- droite(X,Y), cell(Y, B), elim_droite(Y, B).
jouer_gauche(X, Y) :- gauche(X,Y), cell(Y, B), elim_gauche(Y, B).
jouer_haut(X, Y) :- haut(X,Y), cell(Y, B), elim_haut(Y, B).
jouer_hautGauche(X, Y) :- hautGauche(X,Y), cell(Y, B), elim_hautGauche(Y, B).
jouer_hautDroite(X, Y) :- hautDroite(X,Y), cell(Y, B), elim_hautDroite(Y, B).
jouer_bas(X, Y) :- bas(X,Y), cell(Y, B), elim_bas(Y, B).
jouer_basGauche(X, Y) :- basGauche(X,Y), cell(Y, B), elim_basGauche(Y, B).
jouer_basDroite(X, Y) :- basDroite(X,Y), cell(Y, B), elim_basDroite(Y, B).

destruction(X,Y,c) :-
	retract( cell(X, H) ),
	assert( cell(X, -) ),
	retract( cell(Y, -) ),
	assert( cell(Y, H) ),
	not(jouer_coup(X,Y)).
	
destruction(X,Y,a) :-
	not(jouer_coup(Y, X)),
	retract( cell(X, H) ),
	assert( cell(X, -) ),
	retract( cell(Y, -) ),
	assert( cell(Y, H) ).

/* fonctions d'action dans le jeu */
play(X, Y) :-	
	can_play(X,Y),
	(not(verifPion2(X, Y, C, T))); (verifPion2(X, Y, C, T)),
	destruction(X,Y,T),
	not(show_game),
	find_best_move(Move, Score, Type),
	nl,
	nl,
	write(Move),
	nl,
	write(Score),
	nl,
	write(Type),
	nl,
	nl,
	Move = [H|D],
	D = [Z|_],
	play_comp(H,Z).
		
play_comp(X, Y) :-	
	can_playComp(X, Y),
	(not(verifPion2(X, Y, C, T))); (verifPion2(X, Y, C, T)),
	write(X + Y + T),
	destruction(X,Y,T),
	show_game.
	
/* fonctions d'affichage du jeu */
show_game :-

	% line 1
	cell(a1, A1), 
	cell(b1, B1), 
	cell(c1, C1), 
	cell(d1, D1), 
	cell(e1, E1), 
	cell(f1, F1),
	cell(g1, G1), 
	cell(h1, H1), 
	cell(i1, I1),
	
	% line 2
	cell(a2, A2), 
	cell(b2, B2), 
	cell(c2, C2), 
	cell(d2, D2), 
	cell(e2, E2), 
	cell(f2, F2),
	cell(g2, G2), 
	cell(h2, H2), 
	cell(i2, I2),
	
	% line 3
	cell(a3, A3), 
	cell(b3, B3), 
	cell(c3, C3), 
	cell(d3, D3), 
	cell(e3, E3), 
	cell(f3, F3),
	cell(g3, G3), 
	cell(h3, H3), 
	cell(i3, I3),
	
	% line 4
	cell(a4, A4), 
	cell(b4, B4), 
	cell(c4, C4), 
	cell(d4, D4), 
	cell(e4, E4), 
	cell(f4, F4),
	cell(g4, G4), 
	cell(h4, H4), 
	cell(i4, I4),
	
	% line 5
	cell(a5, A5), 
	cell(b5, B5), 
	cell(c5, C5), 
	cell(d5, D5), 
	cell(e5, E5), 
	cell(f5, F5),
	cell(g5, G5), 
	cell(h5, H5), 
	cell(i5, I5),

	L5 = ["5| ", A5, -, B5, -, C5, -, D5, -, E5, -, F5, -, G5, -, H5, -, I5],
	L4 = ["4| ", A4, -, B4, -, C4, -, D4, -, E4, -, F4, -, G4, -, H4, -, I4],
	L3 = ["3| ", A3, -, B3, -, C3, -, D3, -, E3, -, F3, -, G3, -, H3, -, I3],
	L2 = ["2| ", A2, -, B2, -, C2, -, D2, -, E2, -, F2, -, G2, -, H2, -, I2],
	L1 = ["1| ", A1, -, B1, -, C1, -, D1, -, E1, -, F1, -, G1, -, H1, -, I1],
	L0 = ["0  ", "A",  -, "B",  -, "C",  -, "D",  -, "E",  -, "F",  -, "G",  -, "H",  -, "I"],

	nl,	
	nl,
	write(L5),
	nl,
	ansi_format([bold,fg(green)], '~w', ['  |  | \\ | / | \\ | / | \\ | / | \\ | / |']),
	nl,
	write(L4),
	nl,
	ansi_format([bold,fg(green)], '~w', ['  |  | / | \\ | / | \\ | / | \\ | / | \\ |']),
	nl,
	write(L3),
	nl,
	ansi_format([bold,fg(green)], '~w', ['  |  | \\ | / | \\ | / | \\ | / | \\ | / |']),
	nl,
	write(L2),
	nl,
	ansi_format([bold,fg(green)], '~w', ['  |  | / | \\ | / | \\ | / | \\ | / | \\ |']),
	nl,
	write(L1),
	nl,
	write('  |____________________________________'),
	nl,
	write(L0),
	nl,
	nl,
	write('Your opponent has played!'),
	nl,	
	nl,
	
	
	/* On vérifie ici si la partie est terminée */
	findall( C, cell(C, b), R ), /* On compile une liste de tout les occurences de cellules appartenant à b */
	length(R, N), /* la longueur de cette liste est ici calculé*/
	findall( B, cell(B, n), Q ), /* On compile une liste de tout les occurences de cellules appartenant à n */
	length(Q, F), /* la longueur de cette liste est ici calculé*/
	write('La partie est fini?'),
	(F =:= 0;N =:= 0), /* Si la partie est fini, cette condition est vrai et le programme continue*/
	nl,
	write('True'),
	nl,
	write('Vous avez gagne?'),
	(F =:= 0), /* Cette condition affichera false si vous avez perdu */
	nl,
	write('True').
	
	

	
		





% https://www.youtube.com/watch?v=SykxWpFwMGs&t=415s
% timestamp 30:00 pour vertical / horizontal
% timestamp 39:30 pour opérations math
% timestamp 20:00 format de texte
