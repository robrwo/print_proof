% -*-mode: prolog-*-
% bussproofs.pl 0.1.1 (10 April 2007)
% Robert Rothenberg <rr@cs.st-andrews.ac.uk>
%
% Markup proofs in LaTeX for bussproofs.sty package.
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

:- module(bussproofs, [ ]).

:- use_module(term_to_tex).

print_rule( Callback, Rule, Root, Children ) :-
  print_rule( Callback, 0, Rule, Root, Children ).

print_rule( _, Depth, _, Root, [ ] ) :-
  term_to_tex(Root, RootTex),
  tab(Depth),
  write_list(['\\AxiomC{$', RootTex, '$}']),
  nl.

print_rule( Callback, Depth, Rule, Root, Children ) :-
  term_to_tex(Rule, RuleTex),
  term_to_tex(Root, RootTex),
  succ(Depth, Depth1),
  
  print_children( Callback, Depth1, Children ),

  tab(Depth),
  write_list([ '\\LeftLabel{$', RuleTex, '$}' ]),
  nl,

  length(Children, NumChild),
  tag_name(NumChild, Tag),

  tab(Depth),
  write_list([ Tag, '{$', RootTex, '$}' ]),
  nl.


print_children( Callback, Depth, [Child] ) :-
  call( Callback, Child, ChildRule, ChildNode, Grandchildren ),
  print_rule( Callback, Depth, ChildRule, ChildNode, Grandchildren ).

print_children( Callback, Depth, [Child | Siblings] ) :-
  call( Callback, Child, ChildRule, ChildNode, Grandchildren ),
  print_rule( Callback, Depth, ChildRule, ChildNode, Grandchildren ),
  print_children( Callback, Depth, Siblings ).


tag_name(1, '\\UnaryInfC').
tag_name(2, '\\BinaryInfC').
tag_name(3, '\\TrinaryInfC').

write_list([ ]).

write_list([E | R]) :-
  write(E),
  write_list(R).
