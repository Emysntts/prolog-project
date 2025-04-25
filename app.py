import streamlit as st
from pyswip import Prolog
import re
import datetime
from datetime import timedelta


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

def fase_menstrual(nome):
    data_inicio, duracao_ciclo, duracao_menstruacao = ultimo_ciclo(nome)
    if data_inicio:
        # Fase menstrual vai do primeiro dia ao último dia da menstruação
        data_inicio_menstruacao = data_inicio
        data_fim_menstruacao = data_inicio + timedelta(days=duracao_menstruacao)
        return data_inicio_menstruacao.strftime("%d/%m/%Y"), data_fim_menstruacao.strftime("%d/%m/%Y")
    return None, None

def fase_folicular(nome):
    data_inicio, duracao_ciclo, duracao_menstruacao = ultimo_ciclo(nome)
    if data_inicio:
        # Fase folicular começa após a menstruação
        data_inicio_folicular = data_inicio + timedelta(days=duracao_menstruacao)
        # Fase folicular termina 14 dias antes da ovulação (aproximadamente)
        data_fim_folicular = data_inicio_folicular + timedelta(days=14)
        return data_inicio_folicular.strftime("%d/%m/%Y"), data_fim_folicular.strftime("%d/%m/%Y")
    return None, None

def ovulacao(nome):
    data_inicio, duracao_ciclo, duracao_menstruacao = ultimo_ciclo(nome)
    if data_inicio:
        # Ovulação ocorre 14 dias antes do final do ciclo
        data_ovulacao = data_inicio + timedelta(days=duracao_ciclo - 14)
        return data_ovulacao.strftime("%d/%m/%Y")
    return None

def fase_lutea(nome):
    data_inicio, duracao_ciclo, duracao_menstruacao = ultimo_ciclo(nome)
    if data_inicio:
        # Fase lútea começa após a ovulação (14 dias antes do final do ciclo)
        data_inicio_lutea = data_inicio + timedelta(days=duracao_ciclo - 14)
        # Fase lútea termina 14 dias depois da ovulação
        data_fim_lutea = data_inicio_lutea + timedelta(days=14)
        return data_inicio_lutea.strftime("%d/%m/%Y"), data_fim_lutea.strftime("%d/%m/%Y")
    return None, None

def fases_do_ciclo(nome):
    data_inicio_menstruacao, data_fim_menstruacao = fase_menstrual(nome)
    data_inicio_folicular, data_fim_folicular = fase_folicular(nome)
    data_ovulacao = ovulacao(nome)
    data_inicio_lutea, data_fim_lutea = fase_lutea(nome)
    
    return {
        "fase_menstrual": (data_inicio_menstruacao, data_fim_menstruacao),
        "fase_folicular": (data_inicio_folicular, data_fim_folicular),
        "ovulacao": data_ovulacao,
        "fase_lutea": (data_inicio_lutea, data_fim_lutea)
    }

# -------------------------------------------------------------------------------------------------------------------------------- #
# --- Interface Streamlit ---

st.title("Previsão e Catalogação do Ciclo Menstrual")

nome = st.text_input("Nome", "")
data_inicio = st.date_input("Data de Início da Sua Última Menstruação", datetime.date.today())
duracao_menstruacao = st.number_input("Duração da Menstruação (dias)", min_value=1, max_value=15, value=5)
duracao_ciclo = st.number_input("Duração do Ciclo (dias)", min_value=1, max_value=60, value=28)

opcao_ciclo = st.radio(
    "Deseja ver as informações do ciclo do mês atual ou do próximo ciclo?",
    ("Ciclo Atual", "Próximo Ciclo")
)

# Lógica para calcular as datas conforme a opção escolhida
if opcao_ciclo == "Ciclo Atual":
    # Usando a data fornecida para calcular o ciclo atual
    st.write(f"Você selecionou o **Ciclo Atual**, começando em {data_inicio}.")
    # A lógica para calcular as datas do ciclo atual pode ser adicionada aqui

elif opcao_ciclo == "Próximo Ciclo":
    # Calculando a data de início do próximo ciclo
    data_inicio = data_inicio + datetime.timedelta(days=duracao_ciclo)
    st.write(f"Você selecionou o **Próximo Ciclo**, que começa em {data_inicio}.")
    # A lógica para calcular as datas do próximo ciclo pode ser adicionada aqui

# Adicionar ciclo
if st.button("Adicionar Ciclo"):
    if duracao_ciclo is not None and duracao_ciclo > 0:
        adicionar_ciclo(nome, data_inicio, duracao_ciclo, duracao_menstruacao)
        st.success(f"Ciclo de {nome} adicionado com sucesso!")
    else:
        st.error("A duração do ciclo deve ser de pelo menos 1 dia.")
# ------------------------------------------------------------------------------------------------------------------------------ #
# --- Mudancas no Calendário para Exibir o Ciclo Menstrual ---
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
            "color": "#F27794"  
        })

    # 2 - Dia de início e fim do ciclo
    events.append({
        "title": "Início do Ciclo",
        "start": data_inicio.strftime("%Y-%m-%d"),
        "end": data_inicio.strftime("%Y-%m-%d"),
        "color": "#FDDCC9"  
    })
    fim_ciclo = data_inicio + datetime.timedelta(days=duracao_ciclo)
    events.append({
        "title": "Fim do Ciclo",
        "start": fim_ciclo.strftime("%Y-%m-%d"),
        "end": fim_ciclo.strftime("%Y-%m-%d"),
        "color": "#D1A6AF"  
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
                    "color": "#B77E49"  
                })
        except Exception as e:
            print(f"Erro ao processar período fértil: {e}")

    # 4 - Fase Folicular (do início do ciclo até a ovulação)
    fase_folicular_inicio = data_inicio + datetime.timedelta(days=1)
    fase_folicular_fim = data_inicio + datetime.timedelta(days=duracao_ciclo // 2)
    for i in range((fase_folicular_fim - fase_folicular_inicio).days):
        dia = fase_folicular_inicio + datetime.timedelta(days=i)
        events.append({
            "title": "Fase Folicular",
            "start": dia.strftime("%Y-%m-%d"),
            "end": dia.strftime("%Y-%m-%d"),
            "color": "#E2CD8C"  
        })

    # 5 - Ovulação (por volta do meio do ciclo)
    ovulacao_dia = data_inicio + datetime.timedelta(days=duracao_ciclo // 2)
    events.append({
        "title": "Ovulação",
        "start": ovulacao_dia.strftime("%Y-%m-%d"),
        "end": ovulacao_dia.strftime("%Y-%m-%d"),
        "color": "#F7AC59"  
    })

    # 6 - Fase Lútea (do fim da ovulação até o fim do ciclo)
    fase_lutea_inicio = ovulacao_dia + datetime.timedelta(days=1)
    fase_lutea_fim = fim_ciclo
    for i in range((fase_lutea_fim - fase_lutea_inicio).days):
        dia = fase_lutea_inicio + datetime.timedelta(days=i)
        events.append({
            "title": "Fase Lútea",
            "start": dia.strftime("%Y-%m-%d"),
            "end": dia.strftime("%Y-%m-%d"),
            "color": "#A3A380"  
        })

    # Exibindo o calendário com as fases
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

# ----------------------------------------------------------------------------------------------------------------------- #
# --- Nova Página para Sintomas e Previsão de Fase do Ciclo ---

st.title("Em Qual Fase do Ciclo Você Pode Estar?")

# Lista de sintomas fornecidos pelo usuário
sintomas = st.multiselect(
    "Selecione os sintomas que você está sentindo:",
    ["Inchaço", "Dores de cabeça", "Náusea", "Cansaço", "Alterações de humor", "Tensão nos seios", 
     "Dor abdominal", "Aumento da libido", "Cólicas", "Dor no baixo ventre", "Mudanças no apetite", 
     "Mudanças no muco cervical", "Retenção de líquidos", "Sensibilidade nas mamas", "Vontade de comer"]
)

# Função de previsão com base nos sintomas
def prever_fase_do_ciclo(sintomas):
    # Dicionário de fases e seus sintomas
    fases = {
        "Menstruação": ["Inchaço", "Dores de cabeça", "Cansaço", "Alterações de humor", 
                         "Cólicas", "Retenção de líquidos", "Mudanças no apetite"],
        "Fase folicular": ["Cansaço", "Alterações de humor", "Aumento de energia", 
                           "Melhora no humor", "Sensibilidade nas mamas"],
        "Ovulação": ["Dor abdominal", "Tensão nos seios", "Aumento da libido", 
                     "Mudanças no muco cervical", "Dor no baixo ventre"],
        "Fase lútea": ["Inchaço", "Dores de cabeça", "Náusea", "Tensão nos seios", 
                       "Cansaço", "Alterações de humor", "Retenção de líquidos", "Vontade de comer"]
    }
    
    fase_prevista = []
    
    # Verificar quais fases têm pelo menos um sintoma correspondente
    for fase, sintomas_fase in fases.items():
        if any(sintoma in sintomas for sintoma in sintomas_fase):
            fase_prevista.append(fase)
    
    # Se houver fases previstas, retorna-as. Caso contrário, retorna uma mensagem padrão
    if fase_prevista:
        return fase_prevista
    return ["Fase desconhecida, consulte um especialista."]

# Se sintomas foram selecionados
if sintomas:
    fase = prever_fase_do_ciclo(sintomas)
    st.write(f"Com base nos sintomas selecionados, você pode estar na(s) seguinte(s) fase(s) do ciclo: {', '.join(fase)}")