module Funcoes (
    livrosDisponiveis,
    livrosIndisponiveis,
    marcarDisponibilidade,
    novoId,
    stringPraInt,
    carregarDeArquivo,
    salvarEmArquivo,
    atualizarListaEspera,
    validarRemocaoUsuario,
    editarUsuario,
    removerUsuario,
    validarEmail,
    validarMatricula,
    editarLivro,
    listarLivros,
    removerLivro,
    registrarEmprestimo,
    registrarDevolucao,
    listarLivrosEmprestados,
    listarUsuariosComAtraso,
    listarListaEsperaPorLivro,
    salvarUsuarios,
    carregarUsuarios,
    listarUsuarios
) where
import Tipos
import Exemplos
import Data.List (splitAt,find, elem, delete)
import Data.Maybe (isJust, fromMaybe)
import System.Directory (doesFileExist)
import Data.Time (Day, utctDay, getCurrentTime, addDays, diffDays)

recursaoGenerica :: (Livro -> Bool) -> [Livro] -> [Livro]
recursaoGenerica _ [] = []
recursaoGenerica condicao (x:xs)
    |condicao x = x : recursaoGenerica condicao xs
    |otherwise = recursaoGenerica condicao xs

--conversao de string pra int, retorna Nothing se nao conseguir
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

--autoexplicativo
novoId :: [Livro] -> Int
novoId listaLivros =
    if null listaLivros
        then 0
        else maximum (map idLivro listaLivros) + 1


-- função que remove livros filtrando por id
removerLivro :: Int -> [Livro] -> Either String [Livro]
removerLivro id [] = Left "Livro não encontrado"
removerLivro id (x:xs)                                     
    | id == idLivro x = Right xs
    | otherwise = case removerLivro id xs of
                    Right rest -> Right (x:rest)
                    Left err -> Left err

listarUsuarios :: [Usuario] -> [String]
listarUsuarios = map formatarUsuario
    where
        formatarUsuario u = "Nome: " ++ nome u
            ++ " | Matricula: " ++ matricula u

-- função para formatação dos dados
listarLivros :: [Livro] -> [String]
listarLivros = map formatarLivro
    where
        formatarLivro l = "ID: " ++ show (idLivro l) 
                        ++ " | Título: " ++ titulo l
                        ++ " | Autor: " ++ autor l

-- edita um livro usando splitAt para localizar o elemento
editarLivro :: Int -> (Livro -> Livro) -> [Livro] -> Either String [Livro]
editarLivro id f livros = 
    case break (\l -> idLivro l == id) livros of                            
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
atualizarListaEspera id usuarios livros = 
    map (\livro -> if idLivro livro == id 
                   then livro { listaDeEspera = filter (`elem` usuarios) (listaDeEspera livro) } 
                   else livro) livros

--salva a lista de livros em um txt no caminho dado
salvarEmArquivo :: FilePath -> [Livro] -> IO ()
salvarEmArquivo caminho livros = writeFile caminho (show livros)

--carrega arquivo livro.txt e se n existir o arquivo ele cria um
carregarDeArquivo :: FilePath -> IO [Livro]
carregarDeArquivo caminho = do
    existe <- doesFileExist caminho
    if not existe
       then do
           writeFile caminho "[]"  -- arquivo vazio com lista vazia
           return []
       else do
           conteudo <- readFile caminho
           return (read conteudo :: [Livro])

-- Constante para prazo de empréstimo
prazoEmprestimoDias :: Integer
prazoEmprestimoDias = 14

{-
Registra o empréstimo de um livro para um usuário.
	Verifica se há exemplares disponíveis.
	Se houver:
		Registra o empréstimo.(atualiza livrosEmprestados.txt)
		Diminui o número de exemplares disponíveis.
	Se não houver:
		Pergunta ao usuário se deseja entrar na lista de espera.
			Se sim, adiciona à listaEspera.txt
-}
registrarEmprestimo :: Int -> String -> [Livro] -> [Usuario] -> IO ([Livro], [Usuario])
registrarEmprestimo idLivroEmpr matUsuario livros usuarios = do
    hoje <- utctDay <$> getCurrentTime
    let dataDevolucao = addDays prazoEmprestimoDias hoje -- Calcula data de devolução

    case findLivro idLivroEmpr livros of
        Nothing -> do
            putStrLn $ "Erro: Livro com ID " ++ show idLivroEmpr ++ " não encontrado."
            return (livros, usuarios)
        Just livro ->
            case findUsuario matUsuario usuarios of
                Nothing -> do
                    putStrLn $ "Erro: Usuário com matrícula " ++ matUsuario ++ " não encontrado."
                    return (livros, usuarios)
                Just usuario ->
                    -- Verifica se usuário JÁ TEM este livro
                    if any (\(id, _) -> id == idLivroEmpr) (livrosEmprestados usuario)
                    then do
                        putStrLn "Erro: Usuário já está com este livro emprestado."
                        return (livros, usuarios)
                    else -- Verifica disponibilidade
                        if nDisponiveis livro > 0
                        then do
                            let livroAtualizado = livro { nDisponiveis = nDisponiveis livro - 1 }
                                usuarioAtualizado = usuario { livrosEmprestados = (idLivroEmpr, dataDevolucao) : livrosEmprestados usuario } -- Adiciona tupla
                                novosLivros = updateLivro livroAtualizado livros
                                novosUsuarios = updateUsuario usuarioAtualizado usuarios
                            putStrLn $ "Empréstimo do livro '" ++ titulo livro ++ "' (ID: " ++ show idLivroEmpr ++ ") registrado para " ++ matUsuario
                            putStrLn $ "Devolver até: " ++ show dataDevolucao
                            return (novosLivros, novosUsuarios)
                        else do -- Livro indisponível -> Lista de espera
                            putStrLn $ "Livro '" ++ titulo livro ++ "' indisponível."
                            if any (\u -> matricula u == matUsuario) (listaDeEspera livro)
                            then do
                                putStrLn "Usuário já está na lista de espera."
                                return (livros, usuarios)
                            else do
                                putStr "Deseja entrar na lista de espera? (S/N): "
                                resposta <- getLine
                                if resposta `elem` ["S", "s"]
                                   then case findUsuario matUsuario usuarios of
                                        Just usuarioEncontrado -> do
                                            let livroComEspera = livro { listaDeEspera = listaDeEspera livro ++ [usuarioEncontrado] }
                                                novosLivros = updateLivro livroComEspera livros
                                            putStrLn "Usuário adicionado à lista de espera."
                                            return (novosLivros, usuarios)
                                   else do
                                       putStrLn "Usuário não adicionado à lista de espera."
                                       return (livros, usuarios)


{-
Processa a devolução de um livro por um usuário.
Remove o registro de empréstimo do usuário.
Verifica se há alguém na lista de espera para o livro:
	Se houver: entrega o livro ao próximo da fila automaticamente.
	Se não: aumenta o número de exemplares disponíveis do livro.
-}
registrarDevolucao :: Int -> String -> [Livro] -> [Usuario] -> IO ([Livro], [Usuario])
registrarDevolucao idLivroDev matUsuarioDevolvendo livros usuarios = do
    hoje <- utctDay <$> getCurrentTime
    case findUsuario matUsuarioDevolvendo usuarios of
        Nothing -> do
            putStrLn $ "Erro: Usuário " ++ matUsuarioDevolvendo ++ " não encontrado."
            return (livros, usuarios)
        Just usuarioDevolvendo ->
            if not (any (\(id, _) -> id == idLivroDev) (livrosEmprestados usuarioDevolvendo))
            then do
                putStrLn $ "Erro: Usuário " ++ matUsuarioDevolvendo ++ " não está com o livro ID " ++ show idLivroDev ++ "."
                return (livros, usuarios)
            else
                case findLivro idLivroDev livros of
                    Nothing -> do
                        putStrLn $ "Erro Crítico: Livro ID " ++ show idLivroDev ++ " (emprestado) não encontrado na lista de livros!"
                        return (livros, usuarios)
                    Just livroDevolvido -> do
                        putStrLn $ "Processando devolução do livro '" ++ titulo livroDevolvido ++ "' por " ++ matUsuarioDevolvendo ++ "."
                        -- 1. Remover livro do usuário
                        let emprestimosAtualizados = filter (\(id, _) -> id /= idLivroDev) (livrosEmprestados usuarioDevolvendo)
                            usuarioSemLivro = usuarioDevolvendo { livrosEmprestados = emprestimosAtualizados }
                            usuariosTemp = updateUsuario usuarioSemLivro usuarios
                        -- 2. Verificar lista de espera
                        case listaDeEspera livroDevolvido of
                            [] -> do --Sem fila
                                putStrLn "Ninguém na lista de espera. Livro agora disponível."
                                let livroDisponivel = livroDevolvido { nDisponiveis = nDisponiveis livroDevolvido + 1 }
                                    livrosFinais = updateLivro livroDisponivel livros
                                return (livrosFinais, usuariosTemp)

                            (matProximo : restoFila) -> do -- fila
                                putStrLn $ "Usuário " ++ matricula matProximo ++ " é o próximo da lista de espera."
                                case findUsuario (matricula matProximo) usuariosTemp of
                                    Nothing -> do
                                        -- usuário na fila não existe
                                        putStrLn $ "Erro: Usuário da fila " ++ matricula matProximo ++ " não encontrado. Removendo da fila."
                                        let livroFilaCorrigida = livroDevolvido { listaDeEspera = restoFila, nDisponiveis = nDisponiveis livroDevolvido + 1 }
                                        let livrosFinais = updateLivro livroFilaCorrigida livros
                                        return (livrosFinais, usuariosTemp)
                                    Just usuarioProximo -> do
                                        --remover próximo da fila
                                        let livroFilaAtualizada = livroDevolvido { listaDeEspera = restoFila }
                                            livrosTemp = updateLivro livroFilaAtualizada livros
                                        --nova data de devolução
                                        let novaDataDevolucao = addDays prazoEmprestimoDias hoje
                                        --adicionar livro emprestado
                                        let usuarioProximoComLivro = usuarioProximo { livrosEmprestados = (idLivroDev, novaDataDevolucao) : livrosEmprestados usuarioProximo }
                                            usuariosFinais = updateUsuario usuarioProximoComLivro usuariosTemp
                                        putStrLn $ "Livro emprestado automaticamente para " ++ matricula matProximo ++ "."
                                        putStrLn $ "Devolver até: " ++ show novaDataDevolucao
                                        return (livrosTemp, usuariosFinais)

{-
Gera um relatório dos livros que estão atualmente emprestados.
Lê o arquivo livrosEmprestados.txt
Mostra cada livro emprestado com o nome do usuário correspondente.
-}
listarLivrosEmprestados :: [Livro] -> [Usuario] -> IO ()
listarLivrosEmprestados livros usuarios = do
    putStrLn "\n--- Livros Emprestados Atualmente ---"
    let todosEmprestimos = concatMap (\u -> map (\(idL, dataD) -> (idL, dataD, u)) (livrosEmprestados u)) usuarios
    if null todosEmprestimos
    then putStrLn "Nenhum livro emprestado no momento."
    else mapM_ imprimirEmprestimo todosEmprestimos
    where
        imprimirEmprestimo (idLivroEmpr, dataDev, usuario) =
            case findLivro idLivroEmpr livros of
                Nothing -> putStrLn $ "! Erro: Livro ID " ++ show idLivroEmpr ++ " (emprestado por " ++ matricula usuario ++ ") não encontrado na base!"
                Just livro -> putStrLn $ "ID: " ++ show idLivroEmpr ++ " | Livro: '" ++ titulo livro ++
                                        "' | Usuário: " ++ nome usuario ++ " (Mat: " ++ matricula usuario ++ ")" ++
                                        " | Devolução: " ++ show dataDev

{-
Lista usuários com livros em atraso
-}
listarUsuariosComAtraso :: [Livro] -> [Usuario] -> IO ()
listarUsuariosComAtraso livros usuarios = do
    hoje <- utctDay <$> getCurrentTime
    putStrLn $ "\n--- Usuários com Livros em Atraso (Data Atual: " ++ show hoje ++ ") ---"
    let emprestimosAtrasados = filter (\(_, dataDev, _) -> dataDev < hoje) $
                               concatMap (\u -> map (\(idL, dataD) -> (idL, dataD, u)) (livrosEmprestados u)) usuarios

    if null emprestimosAtrasados
    then putStrLn "Nenhum usuário com livros em atraso."
    else mapM_ (imprimirAtraso hoje) emprestimosAtrasados
    where
        imprimirAtraso :: Day -> (Int, Day, Usuario) -> IO ()
        imprimirAtraso hoje (idLivroAtr, dataDev, usuario) =
             case findLivro idLivroAtr livros of
                Nothing -> putStrLn $ "! Erro: Livro ID " ++ show idLivroAtr ++ " (atrasado com " ++ matricula usuario ++ ") não encontrado!"
                Just livro -> do
                    let diasAtraso = diffDays hoje dataDev
                    putStrLn $ "Usuário: " ++ nome usuario ++ " (Mat: " ++ matricula usuario ++ ")" ++
                               " | Livro: '" ++ titulo livro ++ "' (ID: " ++ show idLivroAtr ++ ")" ++
                               " | Devolução Prevista: " ++ show dataDev ++
                               " | Dias em Atraso: " ++ show diasAtraso

{-
Mostra a lista de espera organizada por livro.
Agrupa os nomes dos usuários na listaEspera.txt por título
Exibe os livros e a ordem dos usuários que aguardam cada um.
-}
listarListaEsperaPorLivro :: [Livro] -> IO ()
listarListaEsperaPorLivro livros = do
    putStrLn "\n--- Listas de Espera por Livro ---"
    let livrosComEspera = filter (not . null . listaDeEspera) livros
    if null livrosComEspera
    then putStrLn "Nenhuma lista de espera ativa."
    else mapM_ imprimirLista livrosComEspera
    where
        imprimirLista livro = do
            putStrLn $ "Livro: '" ++ titulo livro ++ "' (ID: " ++ show (idLivro livro) ++ ")"
            putStrLn "  Usuários na fila (Matrícula):"
            mapM_ (\u -> putStrLn $ "  - " ++ matricula u) (listaDeEspera livro)

-- salva a lista de usuários em um arquivo no caminho dado
salvarUsuarios :: FilePath -> [Usuario] -> IO ()
salvarUsuarios caminho usuarios = writeFile caminho (show usuarios)

-- lê um arquivo e devolve uma lista de usuários
carregarUsuarios :: FilePath -> IO [Usuario]
carregarUsuarios caminho = do
    existe <- doesFileExist caminho
    if existe
          then do
             conteudo <- readFile caminho
             return (read conteudo :: [Usuario])
          else do
              putStrLn $ "Arquivo " ++ caminho ++ " não encontrado. Inicializando com lista vazia."
              return []

-- Procura um livro pelo ID
findLivro :: Int -> [Livro] -> Maybe Livro
findLivro id = find (\livro -> idLivro livro == id)

-- Procura um usuário pela matrícula
findUsuario :: String -> [Usuario] -> Maybe Usuario
findUsuario mat = find (\usuario -> matricula usuario == mat)

-- Atualiza um livro na lista (substitui pelo ID)
updateLivro :: Livro -> [Livro] -> [Livro]
updateLivro livroAtualizado = map (\livro -> if idLivro livro == idLivro livroAtualizado then livroAtualizado else livro)

-- Atualiza um usuário na lista (substitui pela matrícula)
updateUsuario :: Usuario -> [Usuario] -> [Usuario]
updateUsuario usuarioAtualizado = map (\usuario -> if matricula usuario == matricula usuarioAtualizado then usuarioAtualizado else usuario)
