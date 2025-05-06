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

--função para receber os dados do livro
adicionarLivro :: [Livro] -> IO [Livro]
adicionarLivro biblioteca = do
    novo <- criaLivro biblioteca
    return (novo : biblioteca)

-- função para adicionar novo usuario
adicionarUsuario :: [Usuario] -> IO (Either String [Usuario])
adicionarUsuario usuarios = do
    nome <- lerString "Nome: "
    mat <- lerString "Matrícula: "
    eml <- lerString "Email: "
    
    case validarMatricula mat usuarios of
        Left err -> return $ Left err
        Right matValida -> case validarEmail eml of
            Left err -> return $ Left err
            Right emlValido -> 
                let novoUsuario = Usuario nome matValida emlValido []
                in return $ Right (novoUsuario : usuarios)

-- sistema unificado para tratamento de erro para todas as operações
tratarErro :: Either String a -> (a -> IO ()) -> IO ()
tratarErro resultado ação = 
    case resultado of
        Left err -> putStrLn $ "Erro: " ++ err
        Right val -> ação val


-- Menu principal
menuPrincipal :: [Livro] -> [Usuario] -> IO ()
menuPrincipal livros usuarios = do
    putStrLn "========================================="
    putStrLn "Menu Principal"
    putStrLn "========================================="
    putStrLn "Opções:"
    putStrLn "1 - Cadastrar livros"
    putStrLn "2 - Cadastrar usuários"
    putStrLn "3 - Empréstimo e devolução"
    putStrLn "4 - Relatórios"
    putStrLn "5 - Editar livro"
    putStrLn "6 - Editar usuário"
    putStrLn "7 - Salvar e Sair"
    putStr "Digite uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            livrosAtualizados <- adicionarLivro livros
            menuPrincipal livrosAtualizados usuarios
        "2" -> do
            resultado <- adicionarUsuario usuarios
            tratarErro resultado (\usuariosAtualizados -> menuPrincipal livros usuariosAtualizados)
        "3" -> submenuEmprestimoDevolucao livros usuarios
        "4" -> submenuRelatorios livros usuarios
        "5" -> do
            putStrLn "Função para editar livro ainda não implementada."
            menuPrincipal livros usuarios
        "6" -> do
            putStrLn "Função para editar usuário ainda não implementada."
            menuPrincipal livros usuarios
        "7" -> do
            putStrLn "Salvando dados e saindo..."
            salvarEmArquivo "livros.txt" livros
            salvarUsuarios "usuarios.txt" usuarios
        _ -> do
            putStrLn "Opção inválida! Tente novamente."
            menuPrincipal livros usuarios

-- Submenu de Empréstimo e Devolução
submenuEmprestimoDevolucao :: [Livro] -> [Usuario] -> IO ()
submenuEmprestimoDevolucao livros usuarios = do
    putStrLn "========================================="
    putStrLn "Empréstimo e Devolução"
    putStrLn "========================================="
    putStrLn "Opções:"
    putStrLn "1 - Registrar empréstimo"
    putStrLn "2 - Registrar devolução"
    putStrLn "3 - Voltar ao Menu Principal"
    putStr "Digite uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            idLivro <- lerInt "Digite o ID do livro para empréstimo: "
            matriculaUsuario <- lerString "Digite a matrícula do usuário: "
            (livrosAtualizados, usuariosAtualizados) <- registrarEmprestimo idLivro matriculaUsuario livros usuarios
            submenuEmprestimoDevolucao livrosAtualizados usuariosAtualizados
        "2" -> do
            idLivro <- lerInt "Digite o ID do livro para devolução: "
            matriculaUsuario <- lerString "Digite a matrícula do usuário: "
            (livrosAtualizados, usuariosAtualizados) <- registrarDevolucao idLivro matriculaUsuario livros usuarios
            submenuEmprestimoDevolucao livrosAtualizados usuariosAtualizados
        "3" -> menuPrincipal livros usuarios
        _ -> do
            putStrLn "Opção inválida! Tente novamente."
            submenuEmprestimoDevolucao livros usuarios

-- Submenu de Relatórios
submenuRelatorios :: [Livro] -> [Usuario] -> IO ()
submenuRelatorios livros usuarios = do
    putStrLn "========================================="
    putStrLn "Relatórios"
    putStrLn "========================================="
    putStrLn "Opções:"
    putStrLn "1 - Listar livros emprestados"
    putStrLn "2 - Listar usuários com atraso"
    putStrLn "3 - Listar lista de espera por livro"
    putStrLn "4 - Voltar ao Menu Principal"
    putStr "Digite uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            listarLivrosEmprestados livros usuarios
            submenuRelatorios livros usuarios
        "2" -> do
            listarUsuariosComAtraso livros usuarios
            submenuRelatorios livros usuarios
        "3" -> do
            listarListaEsperaPorLivro livros
            submenuRelatorios livros usuarios
        "4" -> menuPrincipal livros usuarios
        _ -> do
            putStrLn "Opção inválida! Tente novamente."
            submenuRelatorios livros usuarios

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    livros <- carregarDeArquivo "livros.txt"
    usuarios <- carregarUsuarios "usuarios.txt"
    menuPrincipal livros usuarios
