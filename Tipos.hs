module Tipos (
    Usuario(..),
    Livro(..)
) where

-- Como que vai representar o emprestimo como tipo algébrico???

data Usuario = Usuario 
    { nome :: String
    , matricula :: String
    , email :: String
    , livrosEmprestados :: [Int]
    } deriving (Show, Eq)

data Livro = Livro 
    { nTotal :: Int
    , nDisponiveis :: Int
    , titulo :: String
    , idLivro :: Int
    , autor :: String
    , ano :: Int
    , listaDeEspera :: [String]
    } deriving (Show, Eq)
