module Tipos (
    Usuario(..),
    Livro(..)
) where

data Usuario = Usuario 
    { nome :: String
    , matricula :: String
    , email :: String
    , livrosEmprestados :: [Int]
    } deriving (Show, Eq)

data Livro = Livro 
    { disponivel :: Bool
    , titulo :: String
    , idLivro :: Int
    , autor :: String
    , ano :: Int
    , listaDeEspera :: [String]
    } deriving (Show, Eq)