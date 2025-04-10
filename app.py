import streamlit as st
from pyswip import Prolog

# Inicializa o motor Prolog
prolog = Prolog()

# Carrega as regras e fatos do Prolog a partir dos arquivos .pl
prolog.consult("prolog/ciclos.pl")  # Carrega os fatos
prolog.consult("prolog/previsao.pl")  # Carrega as regras de previsão

# Função para adicionar ciclo
def adicionar_ciclo(nome, data_inicio, duracao_ciclo, duracao_menstruacao):
    prolog.assertz(f"ciclo({nome}, date({data_inicio.year},{data_inicio.month},{data_inicio.day}), {duracao_ciclo}, {duracao_menstruacao}).")

# Função para prever próximo ciclo
def previsao_menstruacao(nome):
    query = f"previsao_menstruacao({nome}, ProximoCiclo)"
    result = list(prolog.query(query))
    if result:
        return result[0]["ProximoCiclo"]
    return None

# Interface com o usuário usando Streamlit
st.title("Previsão e Catalogação do Ciclo Menstrual")

# Entrar com dados do ciclo
nome = st.text_input("Nome", "")
data_inicio = st.date_input("Data de Início do Ciclo")
duracao_ciclo = st.number_input("Duração do Ciclo (dias)", min_value=1, max_value=60)
duracao_menstruacao = st.number_input("Duração da Menstruação (dias)", min_value=1, max_value=15)

if st.button("Adicionar Ciclo"):
    adicionar_ciclo(nome, data_inicio, duracao_ciclo, duracao_menstruacao)
    st.success(f"Ciclo de {nome} adicionado com sucesso!")

# Previsão do próximo ciclo
if st.button("Prever Próximo Ciclo"):
    previsao = previsao_menstruacao(nome)
    if previsao:
        st.write(f"O próximo ciclo de {nome} começa em {previsao}.")
    else:
        st.write(f"Não há dados suficientes para prever o ciclo de {nome}.")
