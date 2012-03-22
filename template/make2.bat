mpost -tex=tex picture.mp
del picture.eps
ren picture.1 picture.eps
latex template.tex
dvips template.dvi
del template2.ps
ren template.ps template2.ps
ps2pdf template2.ps template2.pdf
