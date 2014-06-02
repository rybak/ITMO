#!/bin/sh

run_latex()
{
    pdflatex -shell-escape -output-directory ../ $1
}
run_one()
{
    run_latex thesis.tex
}
make_thesis()
{
    run_one
    biber --bblsafechars ../thesis
    run_one
    run_one
}

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
    if [ "x$1" = "xagda" ];
    then
        for i in `cat lagda.txt`;
        do
            cd lagda
            agda --latex-dir "../latex" --latex "$i.lagda" --no-termination-check

            # mv latex/* ../latex
            cd ../
        done
    fi
    . ./m.sh
    make_thesis
fi

