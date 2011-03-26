% -*-mode: prolog-*-
% plain.pl 0.1.1 (10 April 2007)
% Robert Rothenberg <rr@cs.st-andrews.ac.uk>
%
% Display proofs in plain text (as raw Prolog terms).
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

:- module(plain, [ ]).

:- use_module(term_to_tex).

print_rule( Callback, Rule, Root, Children ) :-
  print_rule( Callback, 0, Rule, Root, Children ).

print_rule( Callback, Depth, Rule, Root, Children ) :-
  tab(Depth),
  write_list([ Root, ' ', Rule ]),
  nl,
  succ(Depth, Depth1),
  
  forall( member( Child, Children ), (
    call( Callback, Child, ChildRule, ChildNode, Grandchildren ),
    print_rule( Callback, Depth1, ChildRule, ChildNode, Grandchildren ) ) ).

write_list([ ]).

write_list([E | R]) :-
  write(E),
  write_list(R).
