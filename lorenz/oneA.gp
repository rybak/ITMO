load "common.gp"
set title sprintf("A r = %.2f", n)
fn=sprintf("adams%.1f", n)
splot fn with lines
