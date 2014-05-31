
verbatimtex
&latex
\documentclass{article}
\begin{document}
etex

beginfig(1)
    u := 0.5cm;
    vardef dtr(expr a,b,c) =
        draw a--b--c--cycle;
    enddef;
    qu := 0.5;
    vardef dfull(expr x, d) =
        pair L,R;
        L = x + (-1u,-3u + d * 0.5u);
        R = x + ( 1u,-3u + d * 0.5u);
        dtr(x,L,R);
        fill x--L--R--cycle withcolor .5white;
    enddef;

    vardef dalmost(expr x, d) =
        pair L, R, C, Cx, Rx;
        h := 3u;
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
    xshift = (5u, 0);
    yshift = (0 , -5u);
    NF = (2.5u, y);
    ND = NF + xshift;
    NL = NF + yshift;
    NR = NL + xshift;
    dedges(NF);
    dfull(NF + lshift, 0);
    dfull(NF + rshift, 0);

    dedges(ND);
    dfull(ND + lshift, 0);
    dfull(ND + rshift, 1);

    dedges(NL);
    dalmost(NL + lshift, 1);
    dfull(NL + rshift, 1);

    dedges(NR);
    dfull(NR + lshift, 0);
    dalmost(NR + rshift, 1);
    picture lnf, lnd, lnl, lnr;
    pair labelshift;
    labelshift := (0, 0.4u);
    lnf := thelabel("nf", NF shifted labelshift);
    lnd := thelabel("nd", ND shifted labelshift);
    lnl := thelabel("nl", NL shifted labelshift);
    lnr := thelabel("nr", NR shifted labelshift);
    color mycolor;
    mycolor := (0,0.545,0);
    draw lnf withcolor mycolor;
    draw lnd withcolor mycolor;
    draw lnl withcolor mycolor;
    draw lnr withcolor mycolor;
%    label.top(btex $ {nf} $ etex, NF);
%    label.top(btex $ {nd} $ etex, ND);
%    label.top(btex $ {nl} $ etex, NL);
%    label.top(btex $ {nr} $ etex, NR);
endfig;
end;
