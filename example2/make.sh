#!/bin/bash

for f in analysis-slides ; do
  latex $f || exit 1
  latex $f || exit 1
  dvipdfmx $f.dvi || exit 1
done
