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


-- função main (completar)
main :: IO()
main = do
    hSetBuffering stdout NoBuffering
