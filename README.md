
# The Grail Theorem Prover

Grail is a bare-bones but flexible and robust parser/automated theorem prover
for multimodal categorial grammars. It is designed to allow students
and researchers to design and experiment with their grammars.

On of the advantages over the more modern, efficient and interactive
[Grail 3](https://github.com/RichardMoot/Grail) is that it produces
natural deduction proofs.

The current version of Grail 0 is based on source code developed
by Richard Moot, Xander Schrijen, Gert Jan Verhoog with additional
code by Michael Moortgat.

# Requirements

The current distribution was last modified at 22 February 2015 and
has been verified to work with SWI Prolog 6.6.6. A version of
pdflatex (or other LaTeX version) is required for the LaTeX output.

# Usage

The file `grail/fragments/example.pl` provides an empty example grammar
with comments on how to modify it for your own use. Extend this file with
your own lexicon and structural rules.

The man source code is the file `grail/sources/grail.pl`. 

You can start Grail as follows.

```
  cd grail/sources
  swipl
  [grail].
```

Type ``grail_help.`` for an overview of the available commands. 

You can load a grammar as follows.

```
  load_fragment('../fragments/q.pl').
```

You can specify the natural deduction output format as follows
(here Prawitz-style natural deduction).

```
  load_output_module(prawitz_tex).
```

Finally, you can parse a sentence as follows.

```
  parseall([who,john,talks,to],rel).
```

This generates a LaTeX file `proofs1.tex` containing the natural
deduction version of any proofs found. In a separate shell, use

```
  pdflatex proofs1.tex
```

to obtain the pdf file of the proof.

# Formulas

Lexical entries are of the form.

```
  lex(Word, Formula, Semantics).
```

Where `Word` is a Prolog atom, `Formula` is a multimodal formula
and `Semantics` is a lexical lambda term, in the formats shown
below.

Multimodal formulas are represented as follows (``I`` is a Prolog term
representing a mode, typically an atom)

Formula | Prolog Term
------|------------
*atom* | any Prolog atom
`<>`*A*      |  `dia(I,A)`
`[]`*A*       | `box(I,A)`
*A*`/`*B*      | `dr(I,A,B)`
*B*`\`*A*      |  `dl(I,B,A)`
*A*`*`*B*      |  `p(I,A,B)`
