{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, TupleSections #-}

module Database.Sql.Oracle.Scanner where


import Data.Int (Int64)
import Data.Char (isAlphaNum, isAlpha, isSpace, isDigit)
import Data.List (sortBy)

import Data.String.Conversions (cs)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

import Data.Foldable (asum)
import Control.Applicative (liftA2, liftA3)
import Control.Monad.State (State, runState, state, gets, put, modify)


import Database.Sql.Oracle.Token
import Database.Sql.Position
  (Position(..), Range(..), advance, advanceHorizontal, advanceVertical)

tokenize :: T.Text -> [(Token, Position, Position)]
tokenize = go (Position 1 0 0)
  where
    go :: Position -> T.Text -> [(Token, Position, Position)]
    go _ "" = []
    go p t = case T.head t of
      c | isAlpha c || c == '_' || c == '"' ->
          case tokName p t of
            Left token -> [token]
            Right (name, quoted, rest, p') -> (TokWord quoted name, p, p') : go p' rest
      c | isDigit c ->
          let ((token, len), rest) = parseNumber t
              p' = advanceHorizontal len p
            in (token, p, p') : go p' rest
      '$' | "${" `T.isPrefixOf` t ->
          let ((token, len), rest) = parseVariable t
              p' = advanceHorizontal len p
            in (token, p, p') : go p' rest
      '`' ->
        case tokQuotedWord p t of
          Left p' -> [(TokError "end of input inside name", p, p')]
          Right (name, rest, p') -> (TokWord True name, p, p') : go p' rest
      c | (== '\n') c -> let (newlines, rest) = T.span (== '\n') t
                             p' = advanceVertical (T.length newlines) p
                             in go p' rest
      c | (liftA2 (&&) isSpace (/= '\n')) c ->
            let (spaces, rest) = T.span (liftA2 (&&) isSpace (/= '\n')) t
                p' = advanceHorizontal (T.length spaces) p
             in go p' rest
      '-' | "--" `T.isPrefixOf` t ->
          let (comment, rest) = T.span (/= '\n') t
              p' = advanceVertical 1 (advanceHorizontal (T.length comment) p)
           in go p' (T.drop 1 rest)
      '/' | "/*" `T.isPrefixOf` t ->
          case T.breakOn "*/" t of
              (comment, "") ->
                let p' = advance comment p
                 in [(TokError "unterminated join hint", p, p')]
              (comment, rest) ->
                let p' = advance (T.append comment "*/") p
                 in go p' $ T.drop 2 rest
      c | c == '\'' ->
          case tokExtString c p t of
            Left (tok, p') -> [(tok, p, p')]
            Right (string, rest, p') -> (TokString string, p, p') : go p' rest
      '.' ->
          let p' = advanceHorizontal 1 p
           in (TokSymbol ".", p, p') : go p' (T.tail t)
      c | isOperator c -> case readOperator t of
          Just (sym, rest) -> let p' = advanceHorizontal (T.length sym) p
                               in (TokSymbol sym, p, p') : go p' rest
          Nothing ->
              let opchars = T.take 5 t
                  p' = advance opchars p
                  message = unwords
                      [ "unrecognized operator starting with"
                      , show opchars
                      ]
               in [(TokError message, p, p')]
      c ->
          let message = unwords
                  [ "unmatched character ('" ++ show c ++ "') at position"
                  , show p
                  ]
           in [(TokError message, p, advanceHorizontal 1 p)]

isWordBody :: Char -> Bool
isWordBody = liftA3 (\x y z -> x || y || z) isAlphaNum (== '_') (== '$')

operators :: [T.Text]
operators = sortBy (flip compare)
  [ "+", "-", "*", "/", "%"
  , "||"
  , "&", "|", "^", "~"
  , "!"
  , ":", ":="
  , "!=", "<>", ">", "<", ">=", "<=", "<=>", "=", "=="
  , "(", ")", "[", "]", ",", ";"
  ]
isOperator :: Char -> Bool
isOperator c = elem c $ map T.head operators

readOperator t = asum $ map (\ op -> (op,) <$> T.stripPrefix op t) operators

tokName :: Position -> T.Text -> Either (Token, Position, Position) (T.Text, Bool, T.Text, Position)
tokName pos = go pos [] False
  where
    go :: Position -> [T.Text] -> Bool -> T.Text -> Either (Token, Position, Position) (T.Text, Bool, T.Text, Position)
    go p [] _ "" = error $ "parse error at " ++ show p
    go p ts seen_quotes "" = Right (T.concat $ reverse ts, seen_quotes, "", p)
    go p ts seen_quotes input = case T.head input of
      c | isWordBody c ->
        let (word, rest) = T.span isWordBody input
            p' = advanceHorizontal (T.length word) p
         in go p' (T.toLower word:ts) seen_quotes rest
      c | c == '"' ->
        case tokString p '"' input of
            Left p' -> Left (TokError "end of input inside string", p, p')
            Right (quoted, rest, p') -> Right (quoted, True, rest, p')
      _ -> case ts of
        [] -> error "empty token"
        _ -> Right (T.concat $ reverse ts, seen_quotes, input, p)

parseVariable :: T.Text -> ((Token, Int64), T.Text)
parseVariable = runState $ do
  let endOfInput = "end of input inside variable substitution"
      missingNamespaceOrName = "variable substitutions must have a namespace and a name"
  modify $ T.drop 2
  namespace <- state (T.break (== ':'))

  gets (T.take 1) >>= \case
    "" ->
      let varLen = 2 + T.length namespace
       in if (not $ T.null namespace) && (T.last namespace == '}')
           then pure (TokError missingNamespaceOrName, varLen)
           else pure (TokError endOfInput, varLen)
    _ -> do
      modify $ T.drop 1
      gets (T.take 2) >>= \case
        "${" -> do
          ((subName, subLen), rest) <- gets parseVariable
          _ <- put rest
          gets (T.take 1) >>= \case
              "}" -> do
                modify $ T.drop 1
                let varLen = 2 + T.length namespace + 1 + subLen + 1
                    varName = TokVariable namespace $ DynamicName subName
                pure $ liftInnerErrors $ enforceNamespaces $ (varName, varLen)
              _ -> do
                let varLen = 2 + T.length namespace + 1 + subLen
                pure (TokError endOfInput, varLen)
                
        _ -> do
             name <- state (T.break (== '}'))
             gets (T.take 1) >>= \case
               "" ->
                 let varLen = 2 + T.length namespace + 1 + T.length name
                   in pure (TokError endOfInput, varLen)
               _ -> do
                 modify $ T.drop 1
                 let varLen = 2 + T.length namespace + 1 + T.length name + 1
                 if T.null name
                 then pure (TokError missingNamespaceOrName, varLen)
                 else pure (TokError endOfInput, varLen)
  where
    enforceNamespaces tok@(TokVariable ns _, len) =
      let allowedNamespaces = ["hiveconf", "system", "env", "define", "hivevar"]
          permitted = (`elem` allowedNamespaces)
       in if permitted ns
          then tok
          else (TokError $ "bad namespace in variable substitution: " ++ show ns, len)
    enforceNamespaces x = x
    liftInnerErrors (TokVariable _ (DynamicName (TokError msg)), len) = (TokError msg, len)
    liftInnerErrors x = x

parseNumber :: T.Text -> ((Token, Int64), T.Text)
parseNumber = runState $ do
  ipart <- state $ T.span isDigit
  gets (T.take 1) >>= \case
    "" -> pure (TokNumber ipart, T.length ipart)
    "." -> do
      modify $ T.drop 1
      fpart <- state $ T.span isDigit
      gets (T.take 1) >>= \case
        "e" -> do
          modify $ T.drop 1
          sign <- gets (T.take 1) >>= \case
            s | elem s ["+", "-"] -> modify (T.drop 1) >> pure s
            _ -> pure ""
          state (T.span isDigit) >>= \ epart ->
            let number = T.concat [ipart, ".", fpart, "e", sign, epart]
              in pure (TokNumber number, T.length number)
        _ ->
          let number = T.concat [ipart, ".", fpart]
            in pure (TokNumber number, T.length number)
    "e" -> do
      modify $ T.drop 1
      sign <- gets (T.take 1) >>= \case
        s | elem s ["+", "-"] -> modify (T.drop 1) >> pure s
        _ -> pure ""
      epart <- state $ T.span isDigit
      gets (T.take 1) >>= \case
        c | liftA2 (&&) (not . T.null) (isWordBody . T.head) c || T.null epart -> do
              rest <- state $ T.span isWordBody
              let word = T.concat [ipart, "e", sign, epart, rest]
              pure (TokWord False word, T.length word)
          | otherwise ->
            let number = T.concat [ipart, "e", sign, epart]
             in pure (TokNumber number, T.length number)
    c | liftA2 (||) isAlpha (== '_') (T.head c) -> do
          rest <- state $ T.span (liftA2 (||) isAlpha (== '_'))
          let word = T.concat [ipart, rest]
          pure (TokWord False word, T.length word)
    _ -> pure (TokNumber ipart, T.length ipart)

tokUnquotedWord :: Position -> T.Text -> (T.Text, T.Text, Position)
tokUnquotedWord pos input =
  case T.span (liftA2 (||) isAlphaNum (== '_')) input of
    (word, rest) -> (T.toLower word, rest, advanceHorizontal (T.length word) pos)

tokQuotedWord :: Position -> T.Text -> Either Position (T.Text, T.Text, Position)
tokQuotedWord pos = go (advanceHorizontal 1 pos) [] . T.tail
  where
    go p _ "" = Left p
    go p ts input = case T.head input of
      c | c == '`' ->
        let (quotes, rest) = T.span (== '`') input
            len = T.length quotes
         in if len `mod` 2 == 0
             then go (advanceHorizontal len p)
                     (T.take (len `div` 2) quotes : ts)
                     rest
             else Right (T.concat $ reverse (T.take (len `div` 2) quotes : ts)
                        , rest
                        , advanceHorizontal len p)
      _ -> let (t, rest) = T.span (/= '`') input
            in go (advance t p) (t:ts) rest

halve txt = T.take (T.length txt `div` 2) txt

tokString :: Position -> Char -> T.Text -> Either Position (T.Text, T.Text, Position)
tokString pos d = go (advanceHorizontal 1 pos) [] . T.tail
  where
    go p _ "" = Left p
    go p ts input = case T.head input of
        c | c == d ->
            let (quotes, rest) = T.span (== d) input
                len = T.length quotes
                t = T.take (len `div` 2) quotes
             in if len `mod` 2 == 0
                 then go (advanceHorizontal len p) (t:ts) rest
                 else let str = T.concat $ reverse $ t:ts
                          p' = advanceHorizontal len p
                       in Right (str, rest, p')
        _ -> let (t, rest) = T.span (/= d) input
              in go (advance t p) (t:ts) rest

tokExtString :: Char -> Position -> T.Text -> Either (Token, Position) (ByteString, T.Text, Position)
tokExtString quote pos = go (advanceHorizontal 1 pos) [] . T.drop 1
  where
    go p ts input = case T.span (not . (`elem` [quote, '\\']))  input of
      (cs, "") -> Left (TokError "end of input inside string", advance cs p)
      ("", rest) -> handleSlashes p ts rest
      (cs, rest) -> handleSlashes (advance cs p) (cs:ts) rest
    handleSlashes p ts input = case T.span (== '\\') input of
      (cs, "") -> Left (TokError "end of input inside string", advance cs p)
      ("", _) -> handleQuote p ts input
      (slashes, rest) ->
        let len = T.length slashes
         in if len `mod` 2 == 0
             then go (advanceHorizontal len p) (halve slashes:ts) rest
             else case T.splitAt 1 rest of
               (c, rest')
                 | c == "a" -> go (advanceHorizontal (len + 1) p) ("\a": halve slashes :ts) rest'
                 | c == "b" -> go (advanceHorizontal (len + 1) p) ("\BS": halve slashes :ts) rest'
                 | c == "f" -> go (advanceHorizontal (len + 1) p) ("\FF": halve slashes :ts) rest'
                 | c == "n" -> go (advanceHorizontal (len + 1) p) ("\n": halve slashes :ts) rest'
                 | c == "r" -> go (advanceHorizontal (len + 1) p) ("\r": halve slashes :ts) rest'
                 | c == "t" -> go (advanceHorizontal (len + 1) p) ("\t": halve slashes :ts) rest'
                 | c == "v" -> go (advanceHorizontal (len + 1) p) ("\v": halve slashes :ts) rest'
                 | c == "'" -> go (advanceHorizontal (len + 1) p) ("'": halve slashes :ts) rest'
                 | c == "\"" -> go (advanceHorizontal (len + 1) p) ("\"": halve slashes :ts) rest'
                 | otherwise -> go (advanceHorizontal (len + 1) p) (c:"\\":halve slashes :ts) rest'

    handleQuote p ts input = case T.splitAt 1 input of
      (c, rest) | c == T.singleton quote ->
        Right ( T.encodeUtf8 $ T.concat $ reverse ts
              , rest
              , advanceHorizontal 1 p
              )
      x -> error $ "this shouldn't happend: handleQuote splitInput got " ++ show x
