call metapost
mv picture.1 picture.eps
latex statement.tex
dvipdfmx -p a4 statement.dvi
rem pdflatex statement.tex
rem taskkill /IM "Foxit Phantom.exe"
rem start "" statement.pdf