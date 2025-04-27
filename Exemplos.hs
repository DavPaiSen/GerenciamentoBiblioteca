module Exemplos (
    biblioteca,
    usuarios
) where
import Tipos
biblioteca :: [Livro]
biblioteca =
    [ Livro
        { disponivel = True
        , titulo = "Assim falava Zaratustra"
        , idLivro = 0
        , autor = "Nietzsche"
        , ano = 1883
        , listaDeEspera = []
        }
    , Livro
        { disponivel = True 
        , titulo = "Ensaio sobre a segueira"
        , idLivro = 1
        , autor = "José Saramago"
        , ano = 1995
        , listaDeEspera = []
        }
    , Livro
        { disponivel = True
        , titulo = "Senhor das moscas"
        , idLivro = 2
        , autor = "William Golding"
        , ano = 1954
        , listaDeEspera = []
        }
    , Livro
        { disponivel = False
        , titulo = "A peste"
        , idLivro = 3
        , autor = "Albert Camus"
        , ano = 1947
        , listaDeEspera = []
        }
    ]

usuarios :: [Usuario]
usuarios =
    [ Usuario
        { nome = "Usuariano da Silva"
        , matricula = "0a"
        , email = "usuarinodasilva@naoexiste.com"
        , livrosEmprestados = []
        }
    , Usuario
        { nome = "Usuarina Pereira"
        , matricula = "1b"
        , email = "usuarinapereira@tambemnaoexiste.com"
        , livrosEmprestados = []
        }
    , Usuario
        { nome = "Coisinho"
        , matricula = "2x"
        , email = "coisinho@alucinacoes.com.br"
        , livrosEmprestados = []
        }
    ]