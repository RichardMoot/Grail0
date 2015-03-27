#! /bin/sh

sics='sicstus -l'
swi='swipl -c'

#prolog=$sics
prolog=$swi

sh texcleanup.sh
$prolog output_test.pl
if [ -f proofs1.tex ]; then
        pdflatex proofs1.tex && open proofs1.pdf
fi



