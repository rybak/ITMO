#!/bin/sh


banner "r.sh"
echo "first arg == x $1 x"

if [ "x$1" = "xclean" ];
then
    rm -rf *.aux *.bbl *.bcf *.blg *.dvi *.log *.pdf *.ps *.toc *.run.xml
    cd pic
    rm -rf *.log *.mpx
    for  i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    do
        for j in *.$i
        do
            if [ -f "$j" ]
            then
                rm "$j"
            fi
        done
    done
else
    export TEXINPUTS="$TEXINPUTS:./sty:"
    agda --latex Agda1.lagda
    cd pic
    for i in *.mp
    do
        if [[ -f "$i" ]]
        then
            mpost $i
        fi
    done
    cd ..

    pdflatex thesis.tex
    biber thesis
    pdflatex thesis.tex
    pdflatex thesis.tex
fi

