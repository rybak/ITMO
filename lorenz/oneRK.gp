load "common.gp"
set title sprintf("RK r = %.2f", n)
fn=sprintf("rk%.1f", n)
splot fn with lines
