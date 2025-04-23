import streamlit as st
from pyswip import Prolog
import re
import datetime

# Inicializa o ambiente Prolog
prolog = Prolog()
prolog.consult("prolog/ciclos.pl")

# --- Funções auxiliares ---

def adicionar_ciclo(nome, data_inicio, duracao_ciclo, duracao_menstruacao):
    nome_atom = nome.strip().lower()
    prolog.assertz(
        f"ciclo({nome_atom}, date({data_inicio.year},{data_inicio.month},{data_inicio.day}), {duracao_ciclo}, {duracao_menstruacao})"
    )

def parse_date_term(date_term):
    """Parse a Prolog date term (Functor or string) into DD/MM/YYYY."""
    if hasattr(date_term, "args") and len(date_term.args) == 3:
        return f"{date_term.args[2]:02d}/{date_term.args[1]:02d}/{date_term.args[0]}"
    elif isinstance(date_term, str):
        match = re.match(r"date\((\d+),(\d+),(\d+)\)", date_term)
        if match:
            y, m, d = match.groups()
            return f"{int(d):02d}/{int(m):02d}/{int(y)}"
    return str(date_term)

def proximo_ciclo(nome):
    nome_atom = nome.strip().lower()
    query = f"proximo_ciclo({nome_atom}, ProximaData)"
    result = list(prolog.query(query))
    if result:
        return parse_date_term(result[0]["ProximaData"])
    return None

def ultimo_ciclo(nome):
    nome_atom = nome.strip().lower()
    query = f"ultimo_ciclo({nome_atom}, Data, Duracao, Menstruacao)"
    result = list(prolog.query(query))
    if result:
        d = result[0]["Data"]
        return parse_date_term(d), result[0]["Duracao"], result[0]["Menstruacao"]
    return None, None, None

def alerta_proximo_ciclo(nome):
    nome_atom = nome.strip().lower()
    query = f"alerta_proximo_ciclo({nome_atom}, Alerta)"
    result = list(prolog.query(query))
    if result:
        return result[0]["Alerta"]
    return None

def ciclo_regular(nome):
    nome_atom = nome.strip().lower()
    query = f"ciclo_regular({nome_atom})"
    result = list(prolog.query(query))
    return bool(result)

def periodo_fertil(nome):
    if not nome.strip():
        return None, None
    try:
        nome_atom = nome.strip().lower()
        query = f"periodo_fertil({nome_atom}, DataInicio, DataFim)"
        result = list(prolog.query(query))
        if result:
            return parse_date_term(result[0]["DataInicio"]), parse_date_term(result[0]["DataFim"])
    except Exception as e:
        print(f"Erro ao consultar periodo_fertil: {e}")
    return None, None

def alta_chance_gravidez(nome):
    if not nome.strip():
        return None, None
    try:
        nome_atom = nome.strip().lower()
        query = f"alta_chance_gravidez({nome_atom}, DataAlta1, DataAlta2)"
        result = list(prolog.query(query))
        if result:
            return parse_date_term(result[0]["DataAlta1"]), parse_date_term(result[0]["DataAlta2"])
    except Exception as e:
        print(f"Erro ao consultar alta_chance_gravidez: {e}")
    return None, None

# --- Interface Streamlit ---

st.title("Previsão e Catalogação do Ciclo Menstrual")

nome = st.text_input("Nome", "")
data_inicio = st.date_input("Data de Início do Ciclo", datetime.date.today())
data_fim = st.date_input("Data de Fim do Ciclo", datetime.date.today())
duracao_menstruacao = st.number_input("Duração da Menstruação (dias)", min_value=1, max_value=15, value=5)

# Calcula duração do ciclo
if data_fim >= data_inicio:
    duracao_ciclo = (data_fim - data_inicio).days
    st.write(f"Duração do ciclo: {duracao_ciclo} dias")
else:
    st.error("A data de fim deve ser posterior à data de início.")
    duracao_ciclo = None

# Adicionar ciclo
if st.button("Adicionar Ciclo"):
    if duracao_ciclo is not None and duracao_ciclo > 0:
        adicionar_ciclo(nome, data_inicio, duracao_ciclo, duracao_menstruacao)
        st.success(f"Ciclo de {nome} adicionado com sucesso!")
    else:
        st.error("A duração do ciclo deve ser de pelo menos 1 dia.")

# --- Calendário visual do ciclo ---
from streamlit_calendar import calendar

events = []

if nome.strip() and duracao_ciclo is not None and duracao_ciclo > 0:
    # 1 - Período menstruado
    for i in range(duracao_menstruacao):
        dia = data_inicio + datetime.timedelta(days=i)
        events.append({
            "title": "Menstruação",
            "start": dia.strftime("%Y-%m-%d"),
            "end": dia.strftime("%Y-%m-%d"),
            "color": "red"
        })

    # 2 - Dia de início e fim do ciclo
    events.append({
        "title": "Início do Ciclo",
        "start": data_inicio.strftime("%Y-%m-%d"),
        "end": data_inicio.strftime("%Y-%m-%d"),
        "color": "blue"
    })
    fim_ciclo = data_inicio + datetime.timedelta(days=duracao_ciclo)
    events.append({
        "title": "Fim do Ciclo",
        "start": fim_ciclo.strftime("%Y-%m-%d"),
        "end": fim_ciclo.strftime("%Y-%m-%d"),
        "color": "blue"
    })

    # 3 - Dias do período fértil
    inicio_fertil, fim_fertil = periodo_fertil(nome)
    if inicio_fertil and fim_fertil:
        try:
            inicio_fertil_dt = datetime.datetime.strptime(inicio_fertil, "%d/%m/%Y").date()
            fim_fertil_dt = datetime.datetime.strptime(fim_fertil, "%d/%m/%Y").date()
            delta = (fim_fertil_dt - inicio_fertil_dt).days
            for i in range(delta + 1):
                dia = inicio_fertil_dt + datetime.timedelta(days=i)
                events.append({
                    "title": "Período Fértil",
                    "start": dia.strftime("%Y-%m-%d"),
                    "end": dia.strftime("%Y-%m-%d"),
                    "color": "orange"
                })
        except Exception as e:
            print(f"Erro ao processar período fértil: {e}")

    calendar_options = {
        "initialView": "dayGridMonth",
        "locale": "pt-br",
        "selectable": False,
        "editable": False,
        "headerToolbar": {
            "left": "prev,next today",
            "center": "title",
            "right": "dayGridMonth,timeGridWeek,timeGridDay"
        }
    }
    st.subheader("Calendário do Ciclo Menstrual")
    calendar(events=events, options=calendar_options)