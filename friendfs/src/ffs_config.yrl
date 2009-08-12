Nonterminals body config.
Terminals '<' '>' '</' key value comment.
Rootsymbol config.
config -> key value: {value_of('$1'),value_of('$2')}.
config -> key value config:[{value_of('$1'),value_of('$2')}|'$3'].
config -> comment.
config -> comment config:'$2'.
config -> '<' key value '>' body '</' key '>' : [{value_of('$2'), value_of('$3'), '$5'}].
config -> '<' key value '>' body '</' key '>' config : [{value_of('$2'), value_of('$3'),'$5'} | '$9'].
config -> '<' key '>' body '</' key '>' : [{value_of('$2'), undefined, '$4'}].
config -> '<' key '>' body '</' key '>' config : [{value_of('$2'), undefined,'$4'} | '$8'].
body -> config:'$1'.

Erlang code.
value_of(Token) ->
    element(3, Token).
