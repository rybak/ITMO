set xlabel "x"
set ylabel "y"
set zlabel "z"
n=NUM
sn=sprintf("%.2f", n)
t=sprintf(" r = %.2f", n)
set title sprintf("%s %s", m, t)
fn=sprintf("%s%s", p, sn)
splot fn with lines
