module Main (main) where
import Tipos
import Funcoes
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Exemplos

lerString :: String -> IO String
lerString s = do
    putStr s
    entrada <- getLine
    if null entrada
        then do
            putStrLn "Não pode ser vazio!"
            lerString s
        else
            return entrada

lerInt :: String -> IO Int
lerInt s = do
    entrada <- lerString s
    case stringPraInt entrada of
        Just num -> return num
        Nothing -> do
            putStrLn "Fudeu!"
            lerInt s

criaLivro :: [Livro] -> IO Livro
criaLivro listaLivros = do
    titulo <- lerString "Título: "

    autor <- lerString "Autor: "

    ano <- lerInt "Ano de lançamento do livro: "

    nTotal <- lerInt "Total de livros na biblioteca: "

    let id = novoId listaLivros

    return Livro {
        nTotal = nTotal,
        nDisponiveis = nTotal,
        titulo = titulo,
        idLivro = id,
        autor = autor,
        ano = ano,
        listaDeEspera = []
    }

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
