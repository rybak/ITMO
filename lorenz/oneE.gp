load common.gp
n=NUM
set title sprintf("r = %.2f", n)
set xlabel
fn=sprintf("euler%.1f", n)
splot fn
