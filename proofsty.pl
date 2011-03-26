% -*-mode: prolog-*-
% proofsty.pl 0.0.2 (14 December 2007)
% Robert Rothenberg <rr@cs.st-andrews.ac.uk>
%
% Markup proofs in LaTeX for proofsty.sty package.
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

:- module(proofsty, [ ]).

:- use_module(term_to_tex).

print_rule( Callback, Rule, Root, Children ) :-
  print_rule( Callback, 0, Rule, Root, Children ).

print_rule( _, Depth, _, Root, [ ] ) :-
  term_to_tex(Root, RootTex),
  tab(Depth),
  write_list([RootTex]),
  nl.

print_rule( Callback, Depth, Rule, Root, Children ) :-
  tab(Depth),
  begin_tag(Depth, StartTag),
  write_list([ StartTag ]),
  nl,
  
  term_to_tex(Rule, RuleTex),
  term_to_tex(Root, RootTex),
  succ(Depth, Depth1),
  
  print_children( Callback, Depth1, Children ),

  tab(Depth),
  writeln('\\justifies'),
  tab(Depth1),
  writeln(RootTex),
  
  tab(Depth),
  writeln('\\using'),
  tab(Depth1),
  write_list([ '{', RuleTex, '}' ]),
  nl,

  tab(Depth),
  end_tag(Depth, EndTag),
  write_list([ EndTag ]),
  nl.

begin_tag(0, '\\begin{prooftree}') :- !.
begin_tag(_, '\\[').

end_tag(0, '\\end{prooftree}') :- !.
end_tag(_, '\\]').


print_children( Callback, Depth, [Child] ) :-
  call( Callback, Child, ChildRule, ChildNode, Grandchildren ),
  print_rule( Callback, Depth, ChildRule, ChildNode, Grandchildren ).

print_children( Callback, Depth, [Child | Siblings] ) :-
  call( Callback, Child, ChildRule, ChildNode, Grandchildren ),
  print_rule( Callback, Depth, ChildRule, ChildNode, Grandchildren ),
  print_children( Callback, Depth, Siblings ).


write_list([ ]).

write_list([E | R]) :-
  write(E),
  write_list(R).
