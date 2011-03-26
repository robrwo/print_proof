% -*-mode: prolog-*-
% term_to_tex.pl 0.1.3 (10 April 2007)
% Robert Rothenberg <rr@cs.st-andrews.ac.uk>
%
% Prolog code to translate a term representing logical formulae,
% sequents and hypersequents into LaTeX code.
%
% This has been written for SWI Prolog <http://www.swi-prolog.org>.
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License ( version 2) as
% published by the Free Software Foundation; version 2 of the License.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

:- module(term_to_tex, [def_func_to_tex/3,
			term_to_tex/2,
			arg_list/2,
			string_concat_list/2, string_concat_list/3]).

:- op( 1070, xfx,  => ).
%  op( 1050, xfy,  -> ) is assumed

def_func_to_tex( Functor, Arity, Tex ) :-
  Arity >= 0,
  ( current_predicate( func_to_tex/3 ) ->
    ( func_to_tex( Functor, Arity, _ ) ->
      retract( func_to_tex( Functor, Arity, _ ) )
    ; true )
  ; true ),
  assert( func_to_tex( Functor, Arity, Tex ) ).

:- def_func_to_tex( '.', 2, ',' ).
:- def_func_to_tex( =>,  2, '\\Rightarrow' ).
:- def_func_to_tex( ->,  2, '\\supset' ).
:- def_func_to_tex( '|',  2, ' ~|~ ' ).     % pseudo-functor for hypersequents

% define special symbol names

:- forall( member( Special, [
    'alpha','beta','gamma','delta','epsilon','varepsilon','zeta','eta',
    'theta','vartheta', 'iota','kappa','lambda','mu','nu','xi', 'pi', 'varpi',
    'rho','varrho','sigma','varsigma','tau','upsilon',
    'phi','varphi','chi','psi','omega',
    'bot', 'top'
  ] ),
	  ( string_concat('\\', Special, Tex),
	    def_func_to_tex( Special, 0, Tex) ) ).

term_to_tex( Term, Tex ) :-
  term_to_tex(0, Term, Tex).

% The base cases are exceptions to the list concatenation operator '.' 
% so that we don't have an empty list following the term.

% Special case for hypersequents and hypersequents of relations

term_to_tex( _, [C | H], Tex ) :-
  C =.. [Rel, _, _],
  member(Rel, ['=>', '<', '=<']),
  !,
  term_to_tex( 0, C, Tex1 ),
  length(H, L),
  ( L = 0 ->
    Tex = Tex1
  ; ( term_to_tex( 0, H, Tex2 ),
      ( func_to_tex( '|', 2, FuncTex )
      ; FuncTex = '|' ),
      string_concat_list( [Tex1, Tex2], FuncTex, Tex)
    )
  ).

term_to_tex( _, [ ], '' ).

term_to_tex( _, [Term], Tex ) :-
  term_to_tex( 0, Term, Tex).

% Functors of arity 0 (atoms) are translated here. If there is no
% translation defined, then we convert it to uppercase.

term_to_tex( _, Term, FuncTex ) :-
  functor( Term, Term, 0 ),
  ( func_to_tex( Term, 0, FuncTex )
  ; term_subscript( Term, FuncTex) ).

% Functors of arity 1 which are defined operators are translated here
% as prefix operators.

term_to_tex( Depth, Term, TexP ) :-
  functor( Term, Functor, 1 ),
  current_op(_, _, Functor),
  ( func_to_tex( Functor, 1, FuncTex )
  ; FuncTex = Functor ),
  arg_list(Term, Args),
  Depth1 is Depth + 1,
  maplist( term_to_tex(Depth1), Args, TexArgs ),
  string_concat_list( [ FuncTex | TexArgs ], Tex ),
  apply_parens( Functor, Depth, Tex, TexP ).

% This clause is a workaround, since the concatenation operator does
% not show up with current_op()

term_to_tex( _, Term, Tex ) :-
  functor( Term, '.', 2 ),
  !,
  ( func_to_tex( '.', 2, FuncTex )
  ; FuncTex = ',' ),
  arg_list(Term, Args),
  maplist( term_to_tex(0), Args, [Left | TexArgs] ),
  string_concat_list( [ Left, FuncTex | TexArgs ], Tex ).


% Functors of arity 2 which are defined operators are translated as
% infix in this clause.

term_to_tex( Depth, Term, TexP ) :-
  functor( Term, Functor, 2 ),
  current_op(_, _, Functor),
  ( func_to_tex( Functor, 2, FuncTex )
  ; FuncTex = Functor ),
  arg_list(Term, Args),
  Depth1 is Depth + 1,
  maplist( term_to_tex(Depth1), Args, [Left | TexArgs] ),
  string_concat_list( [ Left, FuncTex | TexArgs ], Tex ),
  apply_parens( Functor, Depth, Tex, TexP ).

% Other functors which are not defined operators are translated as
% arity-0 functors with the arguments as subscripts. So "p(0)" is
% translated to "P_{0}". We note that the arguments are also
% translated, so that "p(a+b)" becomes "P_{A+B}".

term_to_tex( _, Term, TexP ) :-
  functor( Term, Functor, Arity ),
  ( func_to_tex( Functor, Arity, FuncTex )
  ; func_to_tex( Functor, 0,     FuncTex )
  ; upcase_atom(Functor, FuncTex) ),
  arg_list(Term, Args),
  maplist( term_to_tex(0), Args, TexArgs ),
  string_concat_list( TexArgs, ', ', Tex ),
  string_concat_list( [ FuncTex, '_{', Tex, '}' ], '',  TexP ).

term_subscript(Term, TermTex) :-
  sub_string(Term, I, 1, IA, '_'),
  !,
  sub_string(Term, 0, I, _, T1),
  upcase_atom(T1, T2),

  succ(I, I1),
  sub_string(Term, I1, IA, _, Sub),
  term_to_tex(Sub, SubTeX),
  string_concat_list( [ T2, '_{', SubTeX, '}' ], '', TermTex).

term_subscript(Term, TermTeX) :-
  \+ sub_string(Term, _, 1, _, '_'),
  !,
  upcase_atom(Term, TermTeX).


% arg_list(+Term, ?Args) succeeds is Args is the list of arguments for Term.

arg_list(Term, Args) :-
  arg_list(Term, 1, Args).

arg_list(Term, 0, [Term]).

arg_list(Term, Arity, [Arg]) :-
  functor( Term, _, Arity ),
  !,
  arg(Arity, Term, Arg).

arg_list(Term, Num, [Arg | Args]) :-
  arg(Num, Term, Arg),
  Num1 is Num + 1,
  arg_list(Term, Num1, Args).

arg_list(Term, Num, _) :-
  functor( Term, _, Arity ),
  Num > Arity,
  !,
  fail.

apply_parens(_, 0, Str, Str).

apply_parens(-, _, Str, Str).

apply_parens(_, _, Str, Str2) :-
  string_concat('( ', Str,  Str1),
  string_concat(Str1, ' )', Str2).

string_concat_list(List, String) :-
  string_concat_list(List, ' ', String).

string_concat_list([Str], _, Str).

string_concat_list( [Str | Strs], Separator, Str1) :-
  string_concat_list(Strs, Separator, ConStrs),
  string_length(ConStrs, 0),
  !,
  string_concat(Str, Separator, Str1).
  
string_concat_list( [Str | Strs], Separator, ConStr) :-
  string_concat_list(Strs, Separator, ConStrs),
  string_concat(Str, Separator, Str1),
  string_concat(Str1,ConStrs, ConStr).

/*
  
smart_concat(Str1, Str2, Str3) :-
  sub_string(Str2, 0, 1, _, '\\'),
  !,
  string_concat(Str1, Str2, Str3).

smart_concat(Str1, Str2, Str3) :-
  string_concat(Str1, ' ', Str4),
  string_concat(Str4, Str2, Str3).

*/


