load "common.gp"
set title sprintf("IE r = %.2f", n)
fn=sprintf("implicit%.1f", n)
splot fn with lines
