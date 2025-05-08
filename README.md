# Sistema de Gerenciamento de Biblioteca em Haskell

Este projeto implementa um sistema de gerenciamento de biblioteca com funcionalidades para cadastro de livros e usuários, empréstimos, devoluções e geração de relatórios. Foi desenvolvido utilizando a linguagem de programação Haskell pelos alunos: Davi Paiva Sendin, João Paulo Costa Amui, Gabriel Henrique Carneiro Amorim e Gabriel Fachini.

## Funcionalidades Principais

* **Cadastro de Livros:** Permite adicionar novos livros ao sistema, registrando título, autor, ano de lançamento e o número total de exemplares disponíveis. Cada livro recebe um ID único gerado automaticamente.
* **Cadastro de Usuários:** Possibilita o registro de novos usuários na biblioteca, armazenando nome, matrícula (única), e-mail. A matrícula e o e-mail são validados no momento do cadastro.
* **Empréstimo de Livros:** Permite registrar o empréstimo de um livro para um usuário, verificando a disponibilidade e associando uma data de devolução (14 dias a partir da data do empréstimo). Caso o livro não esteja disponível, o usuário pode entrar na lista de espera.
* **Devolução de Livros:** Permite registrar a devolução de um livro, atualizando a disponibilidade e, caso haja usuários na lista de espera, automaticamente emprestando o livro ao próximo da fila.
* **Relatórios:** Oferece as seguintes opções de relatório:
    * Listagem de todos os livros atualmente emprestados, com o nome do usuário e a data de devolução.
    * Listagem de usuários com livros em atraso, indicando o livro, a data de devolução prevista e o número de dias de atraso.
    * Listagem da lista de espera para cada livro, mostrando os usuários na fila por matrícula.
* **Edição de Livros e Usuários:** (Funcionalidade parcialmente implementada, com placeholders no menu) Prevê a futura implementação da edição de informações de livros e usuários.
* **Persistência de Dados:** Os dados de livros e usuários são salvos em arquivos de texto (`livros.txt` e `usuarios.txt`) ao sair do sistema e carregados ao iniciar, garantindo a persistência das informações entre sessões.
## Como Executar
Para executar este sistema, você precisará ter o compilador GHC (Glasgow Haskell Compiler) instalado em seu sistema.

1.  **Salve os arquivos:** Salve os códigos fornecidos em três arquivos separados: `Main.hs`, `Funcoes.hs` e `Tipos.hs` no mesmo diretório.
2.  **Compile o código:** Abra um terminal ou prompt de comando, navegue até o diretório onde você salvou os arquivos e execute o seguinte comando para compilar o código:

    ```bash
    ghc Main.hs Funcoes.hs Tipos.hs -o biblioteca
    ```

    Este comando irá gerar um executável chamado `biblioteca` (ou `biblioteca.exe` no Windows).
3.  **Execute o programa:** No mesmo terminal, execute o programa com o seguinte comando:

    ```bash
    ./biblioteca
    ```

    (No Windows, use `biblioteca.exe`).

    O sistema de gerenciamento de biblioteca será iniciado, apresentando o menu principal com as opções disponíveis. Siga as instruções no console para interagir com o sistema.

## Estrutura do Projeto

O projeto está organizado em três módulos principais:

* **`Main.hs`:** Contém a função `main`, responsável por iniciar a aplicação, carregar os dados persistidos e apresentar o menu principal ao usuário. Também inclui as funções para interação com o usuário (ler strings e inteiros) e as funções dos menus e submenus da aplicação.
* **`Funcoes.hs`:** Define todas as funções de lógica de negócios do sistema, como adicionar, remover, editar livros e usuários, registrar empréstimos e devoluções, gerar relatórios e manipular a lista de espera. Também contém funções auxiliares para conversão de tipos e persistência de dados.
* **`Tipos.hs`:** Define os tipos de dados utilizados no sistema: `Usuario`, `Livro` e `Emprestimo`.

## Persistência de Dados

O sistema utiliza arquivos de texto simples para armazenar os dados:

* **`livros.txt`:** Armazena uma lista de todos os livros cadastrados no sistema, no formato de representação `Show` do tipo `Livro`.
* **`usuarios.txt`:** Armazena uma lista de todos os usuários cadastrados, no formato de representação `Show` do tipo `Usuario`.

Ao iniciar, o sistema tenta ler esses arquivos. Se os arquivos não existirem, eles são criados com listas vazias. Ao sair, o sistema salva o estado atual dos livros e usuários nesses arquivos.

## Observações

* A funcionalidade de edição de livros e usuários está presente no menu, mas a implementação das respectivas funções ainda não foi concluída.
* O tratamento de erros é feito através do tipo `Either String a`, permitindo indicar sucesso com um valor do tipo `a` ou falha com uma mensagem de erro do tipo `String`.
* A data de devolução dos empréstimos é calculada automaticamente como 14 dias a partir da data do empréstimo, utilizando a biblioteca `Data.Time`.
