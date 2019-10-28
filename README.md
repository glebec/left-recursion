# Elimination of Left Recursion

Parser combinators are expressive and easy to embed and reuse within an application. However, they implement _recursive descent_ parsing algorithms, which cannot parse _left-recursive_ grammars. Thankfully, there exists a simple technique to _eliminate_ left recursion in most grammars.

These concepts are detailed [here](https://www.csd.uwo.ca/~moreno/CS447/Lectures/Syntax.html/node8.html) and elsewhere, but typically in the academic jargon of context-free grammars and parsing theory. In contrast, this codebase aims to demonstrate the problem and fix for those familiar with Haskell fundamentals.

## The Setup

Imagine we have a [small data structure](src/Expr.hs) representing a potentially recursive tree of subtraction expressions (similar to [Hutton's Razor](http://www.cs.nott.ac.uk/~pszgmh/semantics.pdf)).

```hs
data Expr = Lit Int | Sub Expr Expr

exampleExpr = Sub (Lit 4) (Sub (Lit 3) (Lit 0))
```

The _string language_ we may want to parse _into_ this data structure could consist of digits, parens, subtraction symbols and so on:

```hs
str1 = "1"
str2 = "1-3"
str3 = "(9)"
str4 = "0-(3-8)-(((2))-(2-1))"
```

This is just a toy example, but it demonstrates the idea that our _language_ (the set of legal strings) may include more tokens than are explicitly represented in our _target_ (the result of parsing).

### Grammars

To organize our thoughts we might try to draft a _grammar_, describing the _production rules_ that can generate all legal strings. Grammars are often written in [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) or [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) syntax, but this example is hopefully understandable without prior knowledge:

```ebnf
EXPR  = SUB | GROUP | LIT
SUB   = EXPR, "-", EXPR
GROUP = "(", EXPR, ")"
LIT   = "0" | "1" | "2" | ... | "9"
```

In other words,

- "An `EXPR`ession is either a `SUB`traction, `GROUP`, or `LIT`eral"
- "A `SUB`traction is an `EXPR`ession, followed by '-', followed by an `EXPR`ession"
- "A `GROUP` is '(', followed by an `EXPR`ession, followed by ')'"
- "A `LIT`eral is either '0', or '1', or... (etc.)"

Grammars consist of _terminal_ symbols like "-" and "3", which actually appear in the language strings, and _nonterminal_ placeholders like `LIT`, which do not. To build an arbitrary legal string by hand, start from the `EXPR` placeholder, and replace it with anything on the right side of its corresponding rule (`SUB`, `GROUP`, or `LIT`). Proceed with replacing nonterminal placeholders with permitted substitutions until your string consists solely of terminal symbols. For example:

```
EXPR
LIT
4
```

Or:

```
EXPR
GROUP
(EXPR)
(SUB)
(EXPR-EXPR)
(LIT-EXPR)
(5-EXPR)
(5-LIT)
(5-2)
```

### Grammars and Parser Combinators

Remarkably, defining a valid _grammar_ for a language (that is, the set of rules that can generate any legal string in the language) is almost the same as defining a working set of _parsers_ for the language (that is, the functions which can analyze an existing string for its structure). Even though these activities (generating vs. consuming strings) are in some ways opposite, their forms are comparable.

So, the context-free _production rule_:

```ebnf
GROUP = "(", EXPR, ")"
```

Corresponds directly to the Haskell (via `trifecta`) _parser_:

```hs
group :: Parser Expr
group = char '(' *> expr <* char ')'
```

(Or in monadic style with `do` notation, if you prefer:)

```hs
group :: Parser Expr
group = do
  char '('
  e <- expr
  char ')'
  pure e
```

## The Problem

The grammar shown earlier is in fact a 100% valid grammar for the expression language we wish to parse. That is, the grammar is capable of _producing any arbitrary string_ of the language, including examples like `"0-(3-8)-(((2))-(2-1))"`.

We want to go backwards – analyze an existing string. In [`src/Broken.hs`](src/Broken.hs), we attempt to structure our parser combinator outline according to this grammar. However, if you attempt to use that parser (not recommended!) on a simple string like "1", it will result in an infinite loop. Why? Let's review the first two lines of the grammar, and their corresponding parsers:

```ebnf
EXPR = SUB | GROUP | LIT
SUB  = EXPR, "-", EXPR
```

```hs
-- Grammar rule: EXPR = SUB | GROUP | LIT
expr :: Parser Expr
expr = sub <|> group <|> lit -- first try `sub`...

-- Grammar rule: SUB = EXPR, "-", EXPR
sub :: Parser Expr
sub = do
  e1 <- expr -- now do `expr`. WARNING: infinite recursion!
  char '-'
  e2 <- expr
  pure $ Sub e1 e2
```

The sequence of events when parsing a string like "1" via the `expr` parser is as follows:

1. Hm, an `expr` might be a `sub`, let's try that parser.
2. Ok, a `sub` begins with an `expr`, let's try that parser. (GOTO 1)

At this point the issue becomes quite clear! Even though this grammar is a valid one for _producing_ arbitrary strings, it is not a useful one for _parsing_ strings via recursive descent; it immediately enters into an infinite loop. This is because the grammar is _left-recursive_. Informally, a left-recursive grammar:

- features a production rule of the form `A = A ... | ...` which loops on itself immediately, or...
- a set of production rules `A = B ... | ...`, `B = C ... | ...`, `C = A ... | ...` which loop around eventually.

Parsers are allowed to be recursive, so long as there exists the possibility for the parser to exit the loop. A parser cannot **unconditionally** recurse on itself – that is recursion without a base case, a classic programming error.

### Attempting a Fix

A naive attempt at solving the problem might just change the _order_ of rules without modifying their structure. For example, perhaps we place the `SUB` rule at the end of `EXPR`?

```ebnf
EXPR  = GROUP | LIT | SUB
GROUP = "(", EXPR, ")"
LIT   = "0" | "1" | "2" | ... | "9"
SUB   = EXPR, "-", EXPR
```

```hs
expr :: Parser Expr
expr = group <|> lit <|> sub -- first try `group`...
```

This is again a valid grammar, but does us no good for parsing. A string like `"(1)-2"` would be parsed as the group `"(1)"` yielding `Lit 1`, and then stop - failing to consume the remaining `"-2"` string. Our parser now terminates, but without ever attempting the recursive case! We will need a different approach.

## The Solution

The technique, which will work in most cases, is to identify the left-recursive path `A => A ...` and split it up into two stages: a "start" and "end" step. The "start" step will be mandatory; the "end" step will be effectively optional, by _allowing empty results_.

Before:

```ebnf
EXPR  = SUB | GROUP | LIT
SUB   = EXPR, "-", EXPR
...
```

After:

```ebnf
EXPR  = START, END
START = GROUP | LIT
END   = "-", EXPR | NOTHING
...
```

(This "NOTHING" result is typically written in grammars using the Greek letter epsilon `𝜀`, and it corresponds to the empty string.)

Notice that the misbehaving `SUB` rule disappears entirely! It has instead been _split up_ across the `START` rule (which parses a chunk of information) and the `END` rule (which **might** parse the continuation of a subtraction, with recursive right-hand expression, or might give up).

In Haskell, we can represent this "successful parse of nothing" using the famous `Maybe` datatype.

```hs
-- Grammar rule: EXPR = START, END
expr :: Parser Expr
expr = do
  e1 <- start
  mE2 <- end
  case mE2 of
    Nothing -> pure e1
    Just e2 -> pure $ Sub e1 e2

-- Grammar rule: START = GROUP | LIT
start :: Parser Expr
start = group <|> lit

-- Grammar rule: END = "-", EXPR | NOTHING
end :: Parser (Maybe Expr)
end = getEnd <|> pure Nothing where
  getEnd = do
    char '-'
    e <- expr
    pure $ Just e
```

Because `end` is recursive – the `expr` it parses itself consists of a new `start` and `end` – you can keep parsing an indefinite chain of subtractions, exactly analogous to a cons list. And just like the famous cons list, that chain of nested parses ends when you hit an empty case (`<|> pure Nothing`, when no `-` symbol is encountered).

Bubbling the information back up, our `expr` parser has to now react to both possibilities:

- Either no `end` was encountered (i.e., no `-` symbol), meaning this is NOT a subtraction expression; or,
- An `end` was built, in case this WAS a subtraction expression.

### Step-by-Step

Let's trace through parsing the string "1" again:

1. Hm, an `expr` begins with `start`
2. The `start` is either a `group` (nope) or a `lit` (yep!)
3. Continuing where we left off, the `expr` ends with `end`
4. `end` either begins with "-" (nope) or it's nothing (yep!)
5. So we have a successful `e1` expression, and `Nothing` for `e2`; guess we just return `e1` (which is `Lit 1`).

What about parsing a subtraction like "1-1"?

1. Hm, an `expr` begins with `start`
2. The `start` is either a `group` (nope) or a `lit` (yep!)
3. Continuing where we left off, the `expr` ends with `end`
4. `end` either begins with "-" (yep!) or it's nothing (nope)
5. Since we matched "-", `end` now continues on with a new `expr`
6. RECURSE: The new `expr` follows the same path as "1" above
7. We have a successful `e1` expression, and also a successful `e2` expression; time to return a `Sub e1 e2`.

## Conclusion

This is meant as a Haskeller-approachable introduction to the _elimination of left recursion_ for recursive descent parsers. The full set of techniques as explained [here](https://www.csd.uwo.ca/~moreno/CS447/Lectures/Syntax.html/node8.html) includes additional examples and variations. I hope you find it helpful, and please let me know if I've made any mistakes.
