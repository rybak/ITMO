%verbatimtex
%\documentclass[12pt]{article}
%\usepackage[T1]{fontenc}
%\begin{document}
%etex;

beginfig(1)
    pair A[];
	A[0] := (0cm, 0cm);
	A[1] := (3cm, 2cm);
	A[2] := (5cm, 4cm);
	A[3] := (2cm, 5cm);
	A[4] := (0cm, 8cm);
	pair H, B, C, T;
	B := A[1] + (A[2] - A[0]) rotated 90;
	H = whatever [B, A[1]];
	H = whatever [A[0], A[2]];
	C := A[3] + (A[2] - A[4]) rotated 90;
	T = whatever [C, A[3]];
	T = whatever [A[4], A[2]];
	
	u:=1cm;
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
	
	for i=0 step 1 until 4:
		draw A[i]--(xpart A[i], 0) dashed evenly withcolor(.5white);
		draw A[i]--(0, ypart A[i]) dashed evenly withcolor(.5white);
	endfor;

	label.lft(btex 1 etex, A[0]);
	label.lrt(btex 2 etex, A[1]);
	label.rt(btex 3 etex, A[2]);
	label.llft(btex 4 etex, A[3]);
	label.lft(btex 5 etex, A[4]);

	
    draw A[1]--H withcolor (red) withpen pencircle scaled 1.3bp dashed evenly;
	draw A[3]--T withcolor (red) withpen pencircle scaled 1.3bp dashed evenly;
	draw A[0]--A[1]--A[2]--A[3]--A[4] withpen pencircle scaled 2bp;
  	draw A[0]--A[2]--A[4] dashed evenly withpen pencircle scaled 1.5bp;

endfig;
end;
