module Main (main) where
import Tipos
import Funcoes
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

criaLivro :: [Livro] -> IO Livro
criaLivro listaLivros = do
    putStr "Título: "
    titulo <- getLine

    putStr "Autor: "
    autor <- getLine

    putStr "Ano de lançamento: "
    ano <- getLine

    let id = novoId listaLivros

    return Livro {
        disponivel = True,
        titulo = titulo,
        idLivro = id,
        autor = autor,
        ano = ano, --precisa transformar de string pra int
        listaDeEspera = []
    }

main :: IO()
main = do
    hSetBuffering stdout NoBuffering