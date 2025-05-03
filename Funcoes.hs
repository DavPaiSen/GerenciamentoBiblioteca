module Funcoes (
    livrosDisponiveis,
    livrosIndisponiveis,
    marcarDisponibilidade,
    novoId,
    stringPraInt
) where
import Tipos
import Exemplos
import Data.List (splitAt)

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


-- função que remove livros filtrando por id
removerLivro :: Int -> [Livro] -> Either String [Livro]
removerLivro idLivro [] = Left "Livro não encontrado"
removerLivro idLivro (x:xs)
    | idLivro == idLivro x = Right xs
    | otherwise = case removerLivro idLivro xs of
                    Right rest -> Right (x:rest)
                    Left err -> Left err

-- função para formatação dos dados
listarLivros :: [Livro] -> [String]
listarLivros = map formatarLivro
    where
        formatarLivro l = "ID: " ++ show (idLivro l) 
                        ++ " | Título: " ++ titulo l
                        ++ " | Autor: " ++ autor l

-- edita um livro usando splitAt para localizar o elemento
editarLivro :: Int -> (Livro -> Livro) -> [Livro] -> Either String [Livro]
editarLivro idLivro f livros = 
    case break (\l -> idLivro l == idLivro) livros of
        (_, []) -> Left "Livro não encontrado"
        (prefix, (x:xs)) -> Right (prefix ++ [f x] ++ xs)

-- função de validação da matricula
validarMatricula :: String -> [Usuario] -> Either String String
validarMatricula mat usuarios
    | any (\u -> matricula u == mat) usuarios = Left "Matrícula já existe"
    | length mat /= 5 = Left "Matrícula deve ter 5 caracteres"
    | otherwise = Right mat
    
-- função de validação do email
validarEmail :: String -> Either String String
validarEmail email
    | '@' `elem` email && '.' `elem` (dropWhile (/='@') email) = Right email
    | otherwise = Left "Email inválido"

-- remove um usuário da lista por matrícula
removerUsuario :: String -> [Usuario] -> Either String [Usuario]
removerUsuario mat usuarios =
    if any (\u -> matricula u == mat) usuarios
        then Right (filter (\u -> matricula u /= mat) usuarios)
        else Left "Usuário não encontrado"

-- função para editar compos do usuario
editarUsuario :: String -> (Usuario -> Usuario) -> [Usuario] -> Either String [Usuario]
editarUsuario mat f usuarios = 
    case break (\u -> matricula u == mat) usuarios of
        (_, []) -> Left "Usuário não encontrado"
        (prefix, (x:xs)) -> Right (prefix ++ [f x] ++ xs)

-- função para evitar remoção de usuarios com livros pendentes
validarRemocaoUsuario :: Usuario -> Either String ()
validarRemocaoUsuario usuario
    | not (null (livrosEmprestados usuario)) = 
        Left "Usuário possui empréstimos ativos"
    | otherwise = Right ()

-- função para atualizar listas de espera apos edição de usuarios
atualizarListaEspera :: Int -> [Usuario] -> [Livro] -> [Livro]
atualizarListaEspera idLivro usuarios livros = 
    map (\livro -> if idLivro livro == idLivro 
                   then livro { listaDeEspera = filter (`elem` usuarios) (listaDeEspera livro) }
                   else livro) livros







