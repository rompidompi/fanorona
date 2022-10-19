/* positionnement initial */
[cell(a5,n), cell(b5,n), cell(c5,n), cell(d5,n), cell(e5,n), cell(f5,n), cell(g5,n), cell(h5,n), cell(i5,n),
cell(a4,n), cell(b4,n), cell(c4,n), cell(d4,n), cell(e4,n), cell(f4,n), cell(g4,n), cell(h4,n), cell(i4,n),
cell(a3,n), cell(b3,b), cell(c3,n), cell(d3,b), cell(e3,-), cell(f3,n), cell(g3,b), cell(h3,n), cell(i3,b),
cell(a2,b), cell(b2,b), cell(c2,b), cell(d2,b), cell(e2,b), cell(f2,b), cell(g2,b), cell(h2,b), cell(i2,b),
cell(a1,b), cell(b1,b), cell(c1,b), cell(d1,b), cell(e1,b), cell(f1,b), cell(g1,b), cell(h1,b), cell(i1,b)].

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
basDroit(X,Y) :-
    hautGauche(Y,X).


joueur(blanc).
joueur(noir).


setup_game :-

	write('Do you want to play the white or the black pieces? '),
	read(A),
	write('Ok, you will play as '),
	write(A).
	
game_loop :-

	write('Where do you want to play? '),
	read(A),
	nl,
	write('Ok, playing at '),
	write(A),
	nl,	
	nl,
	L0 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
	L1 = [1, n, n, n, n, n, n, n, n, n],
	L2 = [2, n, n, n, n, n, n, n, n, n],
	L3 = [3, n, b, n, b, 0, n, b, n, b],
	L4 = [4, b, b, b, b, b, b, b, b, b],
	L5 = [5, b, b, b, b, b, b, b, b, b],
	
	write(L0),
	nl,
	write(L1),
	nl,
	write(L2),
	nl,
	write(L3),
	nl,
	write(L4),
	nl,
	write(L5),
	nl,
	nl,
	write('Your opponent as played!'),
	nl,
	nl,
	game_loop.


contient(ligne_1, (0,0), (0,1)).


% https://www.youtube.com/watch?v=SykxWpFwMGs&t=415s
% timestamp 30:00 pour vertical / horizontal
% timestamp 39:30 pour opérations math
% timestamp 20:00 format de texte



