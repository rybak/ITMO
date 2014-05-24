#!/bin/bash

function run_latex()
{
    pdflatex -output-directory ../ $1
}
function run_one()
{
    run_latex thesis.tex
}
function make_thesis()
{
    run_one
    biber thesis
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
    for i in `cat lagda.txt`;
    do
        pushd lagda
            agda --latex-dir "../latex" --latex "$i.lagda"
            # mv latex/* ../latex
        popd
    done
##    cd pic
##    for i in *.mp
##    do
##        if [[ -f "$i" ]]
##        then
##            mpost $i
##        fi
##    done
##    cd ..
    make_thesis
fi

