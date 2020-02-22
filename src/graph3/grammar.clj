(ns graph3.grammar)

(defn parse-grammar []
  "EXP= op f op

<f> = basic-fn | f-op | left op f op right

<f-op> = f-add | f-minus | f-mul | f-div | f-negate
f-negate = <minus> op f
f-div = f op <div> op f
f-mul = f op <mul> op f
f-minus = f op <minus> op f
f-add = f op <add> op f

<basic-fn> = const | identity | special-fn | exponential
special-fn = special-fn-name op left op f op right
<special-fn-name> = 'sin' | 'cos' | 'tan' | 'exp' | 'log'
identity = var
exponential = f op <power> op number
const = number

<left> = <'('>
<right> = <')'>

<power> = <'**'>
<var> = <'x'>

<add> = '+'
<mul> = '*'
<div> = '/'
<minus> = '-'

<op> = <whitespace*>
whitespace = #'\\s+'
word = letter+

number =  digit+
pos-int = #'[1-9]'
<letter> = #'[a-zA-Z]'
<digit> = #'[0-9]'

")
