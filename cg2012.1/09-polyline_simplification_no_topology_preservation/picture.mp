beginfig(1)
	u := 1cm; % Масштаб
	
	% Задание цепи
	pair A[];
	A[0] := (0, 0);
	A[1] := (3u, 2u);
	A[2] := (5u, 4u);
	A[3] := (2u, 5u);
	A[4] := (0u, 8u);

	% Линии от точек к осям
	for i=0 step 1 until 4:
		draw A[i]--(xpart A[i], 0) dashed evenly withcolor(.5white);
		draw A[i]--(0, ypart A[i]) dashed evenly withcolor(.5white);
	endfor;

    % Координатные оси
	xmax := 7u;
	ymax := 9u;
	drawarrow (0,0)--(xmax, 0);
	drawarrow (0,0)--(0, ymax);
	for i=0 step u until xmax-u:
		draw (i,u/20)--(i,-u/20);
	endfor;
	for i=0 step u until ymax-u:
	    draw (u/20,i)--(-u/20,i);
	endfor;

	% Опускаем перпендикуляр из точек A[1] и A[3] на A[0]--A[2] и A[2]--A[4] соответственно
	pair B, H, C, T;
	% H и T - основания перпедикуляров
	B := A[1] + (A[2] - A[0]) rotated 90;
	H = whatever [B, A[1]];
	H = whatever [A[0], A[2]];
	C := A[3] + (A[2] - A[4]) rotated 90;
	T = whatever [C, A[3]];
	T = whatever [A[4], A[2]];
    draw A[1]--H withcolor (red) withpen pencircle scaled 1.3bp dashed evenly;
	draw A[3]--T withcolor (red) withpen pencircle scaled 1.3bp dashed evenly;

	% Цепь
	draw A[0]--A[1]--A[2]--A[3]--A[4] withpen pencircle scaled 2bp;
	% Упрощенная цепь
  	draw A[0]--A[2]--A[4] dashed evenly withpen pencircle scaled 1.5bp;

	% Подписи вершин
	label.lft(btex 0 etex, A[0]);
	label.lrt(btex 1 etex, A[1]);
	label.rt(btex 2 etex, A[2]);
	label.llft(btex 3 etex, A[3]);
	label.lft(btex 4 etex, A[4]);

endfig;
end;
