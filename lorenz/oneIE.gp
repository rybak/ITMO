load "common.gp"
set title sprintf("IE %s", t)
fn=sprintf("implicit%s", sn)
splot fn with lines
