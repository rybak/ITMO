
verbatimtex
&latex
\documentclass{article}
\begin{document}
etex
u := 0.7cm;
h := 3u;
vardef dtr(expr a,b,c) =
    draw a--b--c--cycle;
enddef;
qu := 0.5;
vardef dfull(expr x, d) =
    pair L,R;
    L = x + (-1u, -3u);
    R = x + ( 1u, -3u);
    pair Lx, Rx;
    q := (0.5 * d * u) / h;
    Lx = (q * x + (1 - q) * L);
    Rx = (q * x + (1 - q) * R);
    dtr(x,Lx,Rx);
    fill x--Lx--Rx--cycle withcolor .5white;
enddef;

vardef dalmost(expr x, d) =
    pair L, R, C, Cx, Rx;
    L = x + (-1u, -h);
    R = x + ( 1u, -h);
    C = 0.5 * (L + R);
    q := (0.5 * d * u) / h;
    Rx = (q * x + (1 - q) * R);
    Cx = (q * x + (1 - q) * C);
    pair xx;
    xx = (h*0.07, 0);
    fill x--L--(C+xx)--(Cx+xx)--Rx--cycle withcolor .5white;
    dtr(x,L,R);
enddef;
pair lshift, rshift;
xs = 1.2u;
ys = -0.5u;
lshift = (-xs, ys);
rshift = ( xs, ys);
vardef dedges(expr x) =
    draw x withpen pencircle scaled 2;
    pickup pencircle scaled 0.3;
    draw x--(x+lshift);
    draw x--(x+rshift);
enddef;

y = 3u;
pair NF, ND, NL, NR;
pair xshift , yshift;
xshift = (6u, 0);
yshift = (0 , -5u);
NF = (2.5u, y);
ND = NF;
NL = NF;
NR = NL;
picture lnf, lnd, lnl, lnr;
pair labelshift;
labelshift := (0, 0.4u);
color mycolor;
mycolor := (0,0.545,0);

beginfig(1)
    dedges(NF);
    dfull(NF + lshift, 0);
    dfull(NF + rshift, 0);
    lnf := thelabel("nf", NF shifted labelshift);
    draw lnf withcolor mycolor;
endfig;

beginfig(2)
    dedges(ND);
    dfull(ND + lshift, 0);
    dfull(ND + rshift, 1);
    lnd := thelabel("nd", ND shifted labelshift);
    draw lnd withcolor mycolor;
endfig;

beginfig(3)
    dedges(NL);
    dalmost(NL + lshift, 1);
    dfull(NL + rshift, 1);
    lnl := thelabel("nl", NL shifted labelshift);
    draw lnl withcolor mycolor;
endfig;

beginfig(4)
    dedges(NR);
    dfull(NR + lshift, 0);
    dalmost(NR + rshift, 1);
    lnr := thelabel("nr", NR shifted labelshift);
    draw lnr withcolor mycolor;
endfig;
end;
