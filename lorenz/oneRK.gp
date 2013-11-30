load "common.gp"
set title sprintf("RK %s", t)
fn=sprintf("rk%s", sn)
splot fn with lines
