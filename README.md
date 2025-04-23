# Projeto de Prolog

Este projeto é uma aplicação que usa Prolog para prever e catalogar o ciclo menstrual das usuárias. A aplicação oferece uma interface simples e interativa via **Streamlit**, permitindo que os usuários insiram dados sobre seus ciclos menstruais e obtenham previsões sobre os próximos ciclos.

## Funcionalidades

- **Cadastro de Ciclos Menstruais**: O usuário pode inserir informações sobre a duração do ciclo e a duração da menstruação.
- **Previsão de Ciclos Menstruais**: Com base nas informações cadastradas, a aplicação pode prever quando será o próximo ciclo menstrual.
- **Interface Simples com Streamlit**: Uma interface interativa e fácil de usar foi criada utilizando o Streamlit.

## Tecnologias Utilizadas

- **Prolog**: Usado para lógica de previsão de ciclos e armazenamento de dados.
- **Python**: Utilizado para integrar o Prolog com a interface Streamlit.
- **Streamlit**: Framework para criar interfaces web interativas em Python.
- **PySwip**: Biblioteca Python para integração com Prolog.

## Como Rodar o Projeto

Para rodar o projeto, siga os passos abaixo.

### Pré-requisitos

- Python 3.x
- pip (gerenciador de pacotes do Python)

### Passos para Configuração e Execução

1. **Clonar o repositório**:
    ```bash
    git clone https://github.com/seu-usuario/projeto-ciclo-menstrual.git
    cd projeto-ciclo-menstrual
    ```

2. **Criar e ativar um ambiente virtual**:
    ```bash
    python3 -m venv venv
    source venv/bin/activate  # Para Linux/macOS
    venv\Scripts\activate  # Para Windows
    ```

3. **Instalar as dependências**:
    Com o ambiente virtual ativado, instale as dependências necessárias:
    ```bash
    pip install -r requirements.txt
    ```

4. **Rodar a aplicação**:
    Para iniciar a aplicação Streamlit, execute o seguinte comando:
    ```bash
    streamlit run app.py
    ```

5. **Acessar no navegador**:
    A aplicação será iniciada no navegador padrão. O Streamlit exibirá um link com o endereço local onde a aplicação estará rodando, geralmente `http://localhost:8501`.



    /backend
  ├── app.py                # Código principal da API com Flask/FastAPI
  ├── prolog_rules.pl       # Arquivo com as    regras Prolog
  ├── cycle_data.py         # Funções para manipulação de dados dos ciclos
  ├── calculations.py       # Funções para cálculos de datas do ciclo
  ├── database.py           # Funções para interação com o banco de dados (SQLite, JSON, etc.)
  └── requirements.txt      # Dependências (Flask, PySWIP, etc.)



