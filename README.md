# 🌸 Aplicativo de Ciclo Menstrual com Prolog e Streamlit

Este projeto é um aplicativo para o acompanhamento do ciclo menstrual, desenvolvido com uma combinação da lógica de programação em **Prolog** e a interface interativa do **Streamlit**. O objetivo principal é fornecer às usuárias uma ferramenta personalizada para monitorar e prever seus ciclos menstruais, com base em dados de saúde e histórico menstrual.

Além de um sistema de monitoramento, o aplicativo oferece explicações educativas sobre as fases do ciclo menstrual e como fatores como saúde, idade e IMC influenciam esses ciclos.

🌼 Membros da equipe
Este projeto foi desenvolvido como parte da disciplina Lógica Aplicada à Computação. Os autores do projeto são:

- [Beatriz Pessôa](https://github.com/beapessoas)
- [Emyle Santos](https://github.com/Emysntts)
- [Clara Dantas](https://github.com/claratdantass)


## Funcionalidades

- **Perfil da usuária**:
  - Cadastro de informações pessoais, como nome, idade, peso, altura e histórico de saúde.
  - Registro de dados sobre a regularidade do ciclo e uso de métodos contraceptivos.
  - Recomendações personalizadas baseadas nas condições de saúde da usuária (ex. SOP, endometriose).

- **Acompanhamento do ciclo**:
  - **Visualização em calendário**: Exibição clara das fases do ciclo (Menstruação, Fase Folicular, Ovulação e Fase Lútea).
  - **Fases do ciclo**: Identificação das fases do ciclo e previsões sobre o próximo ciclo com base nas informações fornecidas.
  - **Período fértil**: Identificação do período fértil, com base na previsão da ovulação.
  - **Previsão do próximo ciclo**: A previsão da data do próximo ciclo menstrual é ajustada com base na idade, IMC, regularidade do ciclo e histórico menstrual.

- **Análise e histórico**:
  - Registro de ciclos anteriores e exibição de gráficos para análise da regularidade do ciclo menstrual.
  - Previsões e recomendações baseadas em sintomas registrados, como cólicas, alterações no humor e mudanças no apetite.
  - Estatísticas personalizadas e ajustes no ciclo com base em fatores como a idade e o IMC.

## 🏗️ Arquitetura do sistema

O sistema é composto por dois componentes principais: a lógica de **Prolog** e a interface de usuário com **Streamlit**.

### 🧠 Lógica de Prolog
A lógica em **Prolog** é responsável pela maior parte dos cálculos e previsões do aplicativo, incluindo:
- Cálculo das fases do ciclo menstrual com base na data do último ciclo e na duração média.
- Previsão do próximo ciclo menstrual com ajustes baseados na idade, IMC e regularidade do ciclo.
- Geração de recomendações personalizadas de acordo com as condições de saúde (como distúrbios hormonais).
- Registro de sintomas e análise de impactos no ciclo menstrual.
- Explicações sobre as fases do ciclo e suas implicações para a saúde.

### 🖥️ Interface de usuário em Streamlit
A interface foi desenvolvida utilizando **Streamlit**, que oferece uma maneira simples e interativa de apresentar as informações. As principais funcionalidades incluem:
- **Cadastro de Perfil**: Permite à usuária inserir dados como peso, altura, histórico de saúde, e regularidade do ciclo.
- **Visualização do Histórico**: Um calendário interativo exibe as fases do ciclo, incluindo as fases de menstruação, folicular, ovulação e lútea.
- **Previsões Personalizadas**: Com base nas informações fornecidas, o sistema calcula e exibe a previsão para o próximo ciclo menstrual.
- **Registro de Sintomas**: A usuária pode registrar sintomas recorrentes, como alterações no humor, cólicas ou mudanças no apetite, que são usados para ajustar as previsões futuras.

## 🔄 Cálculo das fases do ciclo

O ciclo menstrual é dividido nas seguintes fases principais:
1. **Menstruação**: A fase de menstruação é determinada pela duração especificada no perfil da usuária.
2. **Fase Folicular**: Inicia após a menstruação e vai até a metade do ciclo.
3. **Ovulação**: Acontece no meio do ciclo e marca o período fértil.
4. **Fase Lútea**: Vai da ovulação até o final do ciclo.

## 🔮 Previsão do próximo ciclo

A previsão do próximo ciclo é realizada utilizando o predicado **calcular_proxima_menstruacao** em Prolog, considerando:
- Idade da usuária (ajustes dependendo de ser menor de 18 anos ou maior de 45 anos).
- IMC da usuária (ajustes dependendo da faixa do IMC).
- Regularidade do ciclo (ajustes em caso de ciclo irregular ou não especificado).
- A data do último ciclo e a duração padrão informada.

## 🛠️ Tecnologias

- **Python** (Streamlit): Interface gráfica e gerenciamento de estado da aplicação.
- **Prolog** (SWI-Prolog): Lógica de programação para cálculos e previsões de ciclos.
- **PySwip**: Integração entre Python e Prolog.
- **Streamlit**: Framework para a criação da interface gráfica interativa.

## ⚙️ Instalação

1. Clone o repositório:
```bash
git clone https://github.com/usuario/repositorio.git
```

2. Instale as dependências necessárias:
```bash
pip install -r requirements.txt
```

3. Certifique-se de ter o **SWI-Prolog** instalado no seu sistema. Para instalar o SWI-Prolog, siga as instruções em: [https://www.swi-prolog.org/Download](https://www.swi-prolog.org/Download).

## Executando o Aplicativo

Para rodar o aplicativo, utilize o comando abaixo:

```bash
streamlit run src/streamlit_app/main.py
```

## 📂 Estrutura do Projeto

```
.
├── data
│   └── usuarios.json      # Arquivo com os dados dos usuários
└── src
    ├── prolog
    │   ├── ciclos.pl      # Cálculo das fases do ciclo
    │   ├── perfil.pl      # Gerenciamento do perfil do usuário
    │   └── previsao.pl    # Previsões de ciclos e ajustes
    └── streamlit_app
        ├── components
        │   ├── historico.py # Exibição do histórico de ciclos
        │   └── perfil.py    # Formulário de cadastro de perfil
        ├── main.py         # Arquivo principal para execução do app
        └── style.css       # Estilos personalizados do aplicativo
├── requirements.txt       # Dependências do projeto
├── README.md              # Documentação do projeto
```
