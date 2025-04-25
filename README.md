# Aplicativo de Ciclo Menstrual com Prolog e Streamlit

Este é um aplicativo para acompanhamento do ciclo menstrual que combina a interface amigável do Streamlit com a poderosa lógica de programação do Prolog.

## Funcionalidades

- **Perfil da Usuária**:
  - Cadastro de informações pessoais
  - Registro de dados de saúde
  - Histórico menstrual
  - Recomendações personalizadas

- **Acompanhamento do Ciclo**:
  - Visualização em calendário
  - Fases do ciclo
  - Período fértil
  - Previsão do próximo ciclo

- **Análise e Histórico**:
  - Registro de ciclos anteriores
  - Análise de regularidade
  - Previsão baseada em sintomas
  - Estatísticas personalizadas

## Tecnologias

- Python (Streamlit)
- Prolog (SWI-Prolog)
- PySwip (integração Python-Prolog)

## Instalação

1. Clone o repositório
2. Instale as dependências:
```bash
pip install -r requirements.txt
```
3. Certifique-se de ter o SWI-Prolog instalado no seu sistema

## Executando o Aplicativo

```bash
streamlit run app.py
```

## Estrutura do Projeto

```
.
├── app.py              # Aplicativo principal Streamlit
├── pages/             # Páginas do aplicativo
│   ├── perfil.py     # Página de perfil
│   └── historico.py  # Página de histórico
├── prolog/            # Arquivos Prolog
│   ├── ciclos.pl     # Lógica do ciclo menstrual
│   └── perfil.pl     # Gerenciamento de perfil
└── requirements.txt   # Dependências do projeto
```

## Contribuindo

Sinta-se à vontade para contribuir com o projeto! Algumas áreas que podem ser melhoradas:

- Adicionar mais análises em Prolog
- Melhorar as previsões baseadas em sintomas
- Adicionar mais visualizações de dados
- Expandir a base de conhecimento em Prolog
