%:-compile(grail),load_output_module(prawitz_tex).
:-compile(grail),load_output_module(text).

make_savedstate:-
	save_program(text,startup).
	
startup:-
	prolog_flag(argv,[Str]),
	name(Str,Name),
	tokenize(Name,[Fragment|Rest]),
	load_fragment(Fragment),
    polish(Goal,Rest,Expr),
    parseall(Expr,Goal,Results),
    generate_output(Results),
    halt.

polish(dia(Mode,Type))-->
	[dia,Mode],polish(Type).
polish(box(Mode,Type))-->
	[box,Mode],polish(Type).
polish(p(Mode,Type1,Type2))-->
	[p,Mode],polish(Type1),polish(Type2).
polish(dr(Mode,Type1,Type2))-->
	[dr,Mode],polish(Type1),polish(Type2).
polish(dl(Mode,Type1,Type2))-->
	[dl,Mode],polish(Type1),polish(Type2).
%polish(lit(Lit))-->
%	[Lit].
polish(Atom)-->
 	[Atom],
 	{atomic(Atom),\+(conn(Atom))}.
 	
conn(dia).
conn(box).
conn(p).
conn(dr).
conn(dl).
