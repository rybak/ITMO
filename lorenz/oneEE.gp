load "common.gp"
set title sprintf("EE r = %.2f", n)
fn=sprintf("euler%.1f", n)
splot fn with lines
