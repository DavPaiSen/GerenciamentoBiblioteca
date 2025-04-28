module Funcoes (
    livrosDisponiveis,
    livrosIndisponiveis,
    marcarDisponibilidade,
    novoId,
    stringPraInt
) where
import Tipos
import Exemplos

recursaoGenerica :: (Livro -> Bool) -> [Livro] -> [Livro]
recursaoGenerica _ [] = []
recursaoGenerica condicao (x:xs)
    |condicao x = x : recursaoGenerica condicao xs
    |otherwise = recursaoGenerica condicao xs

stringPraInt :: String -> Maybe Int
stringPraInt s
    |and $ map (\d -> d >= '9' && d >= '0') s = Nothing
    |otherwise = Just $ stringPraInt' s 0
        where
            stringPraInt' :: String -> Int -> Int
            stringPraInt' [] acc = acc
            stringPraInt' (x:xs) acc = stringPraInt' xs somou
                where
                    somou = 10 * acc + (fromEnum x - fromEnum '0')

--emprestimo e devolução
livrosDisponiveis :: [Livro] -> [Livro]
livrosDisponiveis listaLivros = recursaoGenerica (\l -> nDisponiveis l > 0) listaLivros

livrosIndisponiveis :: [Livro] -> [Livro]
livrosIndisponiveis listaLivros = recursaoGenerica (\l -> nDisponiveis l == 0) listaLivros

--soma a disponibilidade do livro pelo id, assume que o livro está na lista!!!
marcarDisponibilidade :: Int -> Int -> [Livro] -> [Livro]
marcarDisponibilidade _ _ [] = []
marcarDisponibilidade mudanca id (x:xs)
    |idLivro x == id = (x {nDisponiveis = nDisponiveis x + mudanca} : xs)
    |otherwise = x : marcarDisponibilidade mudanca id xs 

novoId :: [Livro] -> Int
novoId listaLivros =
    if null listaLivros
        then 0
        else maximum (map idLivro listaLivros) + 1
