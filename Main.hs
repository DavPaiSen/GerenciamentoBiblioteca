module Main (main) where
import Tipos
import Funcoes
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

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

lerAno :: IO Int
lerAno = do
    entrada <- lerString "Ano de lançamento: "
    case stringPraInt entrada of
        Just ano -> return ano
        Nothing -> do
            putStrLn "Fudeu!"
            lerAno


criaLivro :: [Livro] -> IO Livro
criaLivro listaLivros = do
    titulo <- lerString "Título: "

    autor <- lerString "Autor: "

    ano <- lerAno


    let id = novoId listaLivros

    return Livro {
        disponivel = True,
        titulo = titulo,
        idLivro = id,
        autor = autor,
        ano = ano,
        listaDeEspera = []
    }

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
