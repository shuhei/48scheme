# Questions

- Monad
- data
- let
- let ... in
- case ... of
- >>
- >>=
- instance ... ... where ... = ...
- -fglasgow-exts is deprecated: Use individual extensions instead

# Answers

## maybe
Try a Maybe value and returns a default value if it's Nothing or applies a function if it's not.

## $ and .

The $ operator is for avoiding parenthesis. Anything appearing after it will take precedence over anything that comes before.

```hs
putStrLn (show (1 + 1))
putStrLn (show $ 1 + 1)
putStrLn $ show $ 1 + 1
```

The primary purpose of the . operator is not to avoid parenthesis, but to chain functions. It lets you tie the output of whatever appears on the right to the input of whatever appears on the left. This usually also results in fewer parenthesis, but works differently.

```hs
putStrLn (show (1 + 1))
(putStrLn . show) (1 + 1)
putStrLn . show $ 1 + 1
```

http://stackoverflow.com/questions/940382/haskell-difference-between-dot-and-dollar-sign

## data

Defines a new data type.

```hs
data {type} = {constructor1} | {constructor2} | {constructor3} | ...
```

http://learnyouahaskell.com/making-our-own-types-and-typeclasses

Looks similar to Scala's case class.

