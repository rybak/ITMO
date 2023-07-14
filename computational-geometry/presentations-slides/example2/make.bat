for %%i in (analysis-slides.tex) do (
  latex %%~ni.tex
  dvips %%~ni.dvi 2>  nul
  dvipdfmx %%~ni.dvi 2> nul
)