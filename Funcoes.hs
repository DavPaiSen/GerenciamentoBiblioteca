module Funcoes (
    livrosDisponiveis,
    livrosIndisponiveis,
    marcarDisponibilidade,
    novoId
) where
import Tipos
import Exemplos

recursaoGenerica :: (Livro -> Bool) -> [Livro] -> [Livro]
recursaoGenerica _ [] = []
recursaoGenerica condicao (x:xs)
    |condicao x = x : recursaoGenerica condicao xs
    |otherwise = recursaoGenerica condicao xs

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