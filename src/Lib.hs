{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lib where

import Data.Text (Text)
import Data.Bifunctor
import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Classes
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Applicative

newtype CanFail a = CanFail { getCanFail :: Either Text a } deriving (Eq, Ord, Show, Functor, Applicative, Monad)

-- fileLines :: FilePath -> IO

-- Initial
-- MaybeT IO (Cofree (MaybeT IO) Text)

newtype EmptyLineT m a = EmptyLineT { getEmptyLineT :: MaybeT m a } deriving (Eq, Eq1, Ord, Ord1, Show, Show1, Functor, Applicative, Monad, MonadTrans, MonadIO)

-- emptyLineT :: Monad m => Text -> EmptyLineT m Text

-- Empty Lines
-- Cofree m Text -> Cofree (EmptyLineT m) Text

-- No successive pairs of empty lines
unpairEmptyLinesT ::
     Monad m => Cofree (EmptyLineT m) a -> Cofree (EmptyLineT m) a
unpairEmptyLinesT ~(x :< xs) =
  (x :<) $ do
    (error "unimplemented" :: EmptyLineT m (Cofree (EmptyLineT m) a) -> EmptyLineT m (Cofree (EmptyLineT m) a))
      xs

-- pairCofree :: Monad m => Cofree (MaybeT m) a -> MaybeT m (Cofree (MaybeT m) (Pair a))
-- pairCofree (x :< xs) = do
--   maybeT (return (pure x :< empty)) $ \(y :< ys) ->
--     return $ Pair (Right (x, y)) :< fmap pairCofree ys


-- Type errors:
-- pairList :: [a] -> ([(a, a)], Maybe a)
-- pairList []       = ([], Nothing)
-- pairList [x]      = ([], Just x)
-- pairList (x:y:zs) = (: (x, y)) `first` pairList zs


newtype Pair a = Pair { getPair :: Either a (a, a) } deriving (Eq, Ord, Show)



instance Functor Pair where
  fmap f (Pair (Left x)) = Pair (Left (f x))
  fmap f ~(Pair (Right (x, y))) = Pair (Right (f x, f y))


instance Applicative Pair where
  pure = Pair . Left

  (<*>) :: Pair (a -> b) -> Pair a -> Pair b
  Pair (Left f) <*> Pair (Left x) = Pair (Left (f x))
  Pair (Left f) <*> Pair (Right (x, y)) = Pair (Right (f x, f y))
  Pair (Right (fx, fy)) <*> Pair (Left x) = Pair (Right (fx x, fy x))
  Pair (Right (fx, fy)) <*> Pair (Right (x, y)) = Pair (Right (fx x, fy y))


instance Comonad Pair where
  extract (Pair (Left x)) = x
  extract (Pair (Right (x, _))) = x

  duplicate p@(Pair (Left _)) = Pair (Left p)
  duplicate   (Pair (Right (x, y))) = Pair (Right (pure x, pure y))


newtype PairT m a = PairT { getPairT :: m (Pair a) }

newtype LineComment = LineComment { getLineComment :: Text } deriving (Eq, Ord, Show)

newtype LineCommentT m a = LineCommentT { getLineCommentT :: ExceptT LineComment m a } deriving (Eq, Eq1, Ord, Ord1, Show, Show1, Functor, Applicative, Monad, MonadTrans, MonadIO)

-- lineCommentT :: Monad m => Text -> LineCommentT m Text


-- Line comments

-- Cofree m (Either (Either EmptyLine LineComment) Indented)

-- Indentation
-- Cofree m (Either EmptyLine Indented)



-- Blocks
-- Cofree m (Block  )


data Indented = Indented { getIndentLevel :: !Int
                         , getIndented    :: !Text
                         } deriving (Eq, Ord, Show)

newtype Indent = Indent (Cofree [] Text)

-- Cofree (MaybeT m) (Int, Text, Maybe Annotation) -> Cofree (MaybeT m) (Cofree [] (Maybe Annotation, Text))


-- 1. parse indents
-- 2. parse line comments
-- 3. parse blocks (all line comments before block := block annotation)
-- 4. lex blocks into:
--      - Paired: () [] {} "" ''
--      - spaces
--      - Upper
--      - lower
--      - digits
--      - symbols
--      - Maybe LineAnnotation
-- 5. parse lexed blocks into trees


-- dropEmpty :: Cofree (MaybeT m) Text -> Cofree (MaybeT m) Text

-- Cofree (MaybeT m) Text -> Cofree (MaybeT (WriterT (Int, Maybe Text) m)) Text

-- Cofree (MaybeT m) Indented -> Cofree (MaybeT m) (WriterT (Maybe Annotation)

-- Block = Cofree (WriterT (Maybe Annotation) []) Text


-- Block =
--   Line  :: a -> Block a
--   Block :: a -> NonEmpty (Block a) -> Block a
-- Block = a | (a, NonEmpty (Block a))
-- Block = (a, Maybe (NonEmpty (Block a)))
-- Block = Cofree [] a


someFunc :: IO ()
someFunc = putStrLn "someFunc"
