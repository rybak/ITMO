#!/usr/bin/perl

while (<>) {
    $str .= $_;
}
print $str
    =~ s/\\AgdaHide\{((\{.*?\}|.)*?)\}/\1/sgr
    =~ s/\\end\{code\}\s*\\begin\{code\}//mgr
    =~ s/HeapTries/HeapModule/r

