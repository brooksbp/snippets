{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
  #-}

import Unbound.LocallyNameless

import Control.Applicative
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe

import Text.Parsec hiding ((<|>), Empty)
import qualified Text.Parsec.Tocken as P
import Text.Parsec.Language (haskellDef)


data Term = Var (Name Term)
          | App Term Term
          | Lam (Bind (Name Term) Term)
          deriving Show

