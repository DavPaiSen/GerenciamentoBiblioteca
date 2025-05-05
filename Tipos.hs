module Tipos (
    Usuario(..),
    Livro(..),
    Emprestimo(..)
) where

import Data.Time.Calendar (Day)

data Usuario = Usuario 
    { nome :: String
    , matricula :: String
    , email :: String
    , livrosEmprestados :: [(Int, Day)]
    } deriving (Show, Eq, Read)

data Livro = Livro 
    { nTotal :: Int
    , nDisponiveis :: Int
    , titulo :: String
    , idLivro :: Int
    , autor :: String
    , ano :: Int
    , listaDeEspera :: [Usuario]
    } deriving (Show, Eq, Read)

data Emprestimo = Emprestimo
    { ativo :: Bool
    , usuario :: Usuario
    , livro :: Livro
    } deriving (Show, Eq, Read)
