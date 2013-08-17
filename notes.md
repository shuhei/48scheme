# Questions

- Monad
- let
- let ... in
- instance ... ... where ... = ...
- -fglasgow-exts is deprecated: Use individual extensions instead
- liftM

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

## case ... of

Pattern matching.

```hs
readExpr input = case parse symbol "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value."
```

## >>, >>=

Bind. Combines lines of a do-block. `bind` has completely different semantics depending on the Monad.

In general, use `>>` if the actions don't return a value, `>>=` if you'll be immediately passing that value into the next action, and do-notation otherwise.

## LiftM

Operates on the value inside the monad, giving us back a monad of the operated value.

```hs
x = liftM func mnd
```

is equivalent to...

```hs
x = do
  val <- mnd
  return $ func val
```

## Parsec

- `many` 0 or more
- `many1` 1 or more
- `skipMany1` 1 or more skipping its result

# Exercises

## Parsing

### 1

```hs
parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
  num <- many1 digit
  return $ Number $ read num
```