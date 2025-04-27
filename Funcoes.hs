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
livrosDisponiveis listaLivros = recursaoGenerica (\l -> disponivel l) listaLivros

livrosIndisponiveis :: [Livro] -> [Livro]
livrosIndisponiveis listaLivros = recursaoGenerica (\l -> not $ disponivel l) listaLivros

--transforma a disponibilidade do livro pelo id
marcarDisponibilidade :: Bool -> Int -> [Livro] -> Maybe [Livro]
marcarDisponibilidade _ _ [] = Nothing
marcarDisponibilidade mudanca id (x:xs)
    |idLivro x == id = Just (x {disponivel = mudanca} : xs)
    |otherwise =
            case marcarDisponibilidade mudanca id xs of
                Nothing -> Nothing
                Just lista -> Just (x:lista)

novoId :: [Livro] -> Int
novoId listaLivros =
    if null listaLivros
        then 0
        else maximum (map idLivro listaLivros) + 1
