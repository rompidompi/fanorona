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



