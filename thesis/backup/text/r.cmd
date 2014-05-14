@echo off

if "%~1" == "clean" goto clean

:build

cd pic
for %%i in (*.mp) do (
    mpost %%i
)
cd ..

pdflatex thesis.tex
biber thesis
pdflatex thesis.tex
pdflatex thesis.tex

exit /b

:clean

cd pic

for %%a in (1 2 3 4 5 6 mpx log) do (
    if exist *.%%a del *.%%a
)
    
cd ..

for %%a in (aux bbl bcf blg dvi log pdf ps toc run.xml) do (
    if exist *.%%a del *.%%a
)

exit /b