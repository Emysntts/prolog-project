import streamlit as st
from pyswip import Prolog
from datetime import datetime, timedelta
import calendar
from streamlit_calendar import calendar

def init_prolog():
    prolog = Prolog()
    prolog.consult("prolog/ciclos.pl")
    prolog.consult("prolog/perfil.pl")
    return prolog

def parse_date(date_term):
    """Parse a Prolog date term into a datetime object."""
    if hasattr(date_term, 'args'):
        return datetime(year=int(date_term.args[0]), month=int(date_term.args[1]), day=int(date_term.args[2]))
    elif isinstance(date_term, str):
        import re
        match = re.match(r"date\((\d+),(\d+),(\d+)\)", date_term)
        if match:
            y, m, d = map(int, match.groups())
            return datetime(year=y, month=m, day=d)
    return datetime.now()  # fallback

def date_to_str(date):
    """Convert date to string in YYYY-MM-DD format for calendar."""
    return date.strftime("%Y-%m-%d")

def create_cycle_event(data_inicio, duracao, duracao_m):
    """Cria eventos para um ciclo específico."""
    events = []
    
    # Período menstrual (vermelho)
    events.append({
        'title': 'Menstruação',
        'start': date_to_str(data_inicio),
        'end': date_to_str(data_inicio + timedelta(days=duracao_m)),
        'backgroundColor': '#ff6b6b',
        'textColor': '#ffffff',
        'allDay': True
    })
    
    # Fase folicular (rosa claro)
    events.append({
        'title': 'Fase Folicular',
        'start': date_to_str(data_inicio + timedelta(days=duracao_m)),
        'end': date_to_str(data_inicio + timedelta(days=duracao//2)),
        'backgroundColor': '#ffd6e0',
        'textColor': '#000000',
        'allDay': True
    })
    
    # Período fértil (verde)
    events.append({
        'title': 'Período Fértil',
        'start': date_to_str(data_inicio + timedelta(days=duracao-16)),
        'end': date_to_str(data_inicio + timedelta(days=duracao-12)),
        'backgroundColor': '#51cf66',
        'textColor': '#ffffff',
        'allDay': True
    })
    
    # Fase lútea (lilás claro)
    events.append({
        'title': 'Fase Lútea',
        'start': date_to_str(data_inicio + timedelta(days=duracao//2)),
        'end': date_to_str(data_inicio + timedelta(days=duracao)),
        'backgroundColor': '#e5dbff',
        'textColor': '#000000',
        'allDay': True
    })
    
    return events

def show_historico_page():
    st.title("Histórico e Previsões")
    prolog = init_prolog()
    
    if 'nome' not in st.session_state:
        st.warning("Por favor, preencha seu perfil primeiro!")
        return
        
    nome = st.session_state.nome
    nome_atom = nome.strip().lower()
    
    # Inicializa a lista de eventos no state se não existir
    if 'calendar_events' not in st.session_state:
        st.session_state.calendar_events = []
    
    # Análise de regularidade
    query_regular = f"ciclo_muito_irregular('{nome_atom}')"
    irregular = bool(list(prolog.query(query_regular)))
    
    if irregular:
        st.warning("""
        Seu ciclo apresenta variações significativas. 
        Considere consultar um profissional de saúde para uma avaliação mais detalhada.
        """)
    
    # Média de duração do ciclo
    query_media = f"media_duracao_ciclo('{nome_atom}', Media)"
    result = list(prolog.query(query_media))
    if result:
        media = result[0]["Media"]
        st.info(f"A duração média do seu ciclo é de {media:.1f} dias.")
        
        if media < 21:
            st.warning("Seu ciclo está mais curto que o normal (21-35 dias). Consulte um ginecologista.")
        elif media > 35:
            st.warning("Seu ciclo está mais longo que o normal (21-35 dias). Consulte um ginecologista.")
        else:
            st.success("Seu ciclo está dentro da faixa considerada normal (21-35 dias).")
    
    # Histórico detalhado
    st.subheader("Registrar Ciclo")
    with st.form("registro_historico"):
        col1, col2 = st.columns(2)
        
        with col1:
            data_inicio = st.date_input("Data de início")
            duracao = st.number_input("Duração do ciclo (dias)", min_value=1, max_value=60, value=28)
            duracao_desconhecida = st.checkbox("Não sei a duração exata", help="Se marcado, usaremos uma duração média de 28 dias")
            if duracao_desconhecida:
                duracao = 28
        
        with col2:
            duracao_menstruacao = st.number_input("Duração da menstruação (dias)", min_value=1, max_value=10, value=5)
            
            # Sintomas experienciados
            sintomas = st.multiselect(
                "Sintomas experienciados",
                ["Cólicas", "TPM", "Dor nos seios", "Mudanças no muco cervical",
                 "Alterações de humor", "Dor abdominal", "Inchaço"]
            )
            
            # Intensidade dos sintomas
            intensidade = st.slider("Intensidade dos sintomas", 1, 5)
        
        submitted = st.form_submit_button("Registrar")
        if submitted:
            # Salvar no Prolog
            query = f"assertz(historico_menstrual('{nome_atom}', date({data_inicio.year},{data_inicio.month},{data_inicio.day}), {duracao}, {duracao_menstruacao}))"
            list(prolog.query(query))
            
            # Adiciona os novos eventos ao state
            novos_eventos = create_cycle_event(data_inicio, duracao, duracao_menstruacao)
            st.session_state.calendar_events.extend(novos_eventos)
            
            st.success("Ciclo registrado com sucesso!")
    
    # Visualização do histórico no calendário
    st.subheader("Calendário do Ciclo")
    
    # Buscar todos os ciclos se ainda não houver eventos no state
    if not st.session_state.calendar_events:
        query = f"historico_menstrual('{nome_atom}', Data, Duracao, DuracaoM)"
        ciclos = list(prolog.query(query))
        
        for ciclo in ciclos:
            data_inicio = parse_date(ciclo['Data'])
            eventos_ciclo = create_cycle_event(data_inicio, ciclo['Duracao'], ciclo['DuracaoM'])
            st.session_state.calendar_events.extend(eventos_ciclo)
    
    # Adicionar legenda
    st.markdown("""
    **Legenda:**
    - 🔴 Menstruação
    - 🌸 Fase Folicular
    - 🌿 Período Fértil
    - 🌷 Fase Lútea
    """)
    
    # Mostrar o calendário
    calendar_options = {
        "headerToolbar": {
            "left": "prev,next today",
            "center": "title",
            "right": "dayGridMonth,timeGridWeek"
        },
        "initialView": "dayGridMonth",
        "selectable": True,
        "editable": False,
    }
    
    calendar(
        events=st.session_state.calendar_events,
        options=calendar_options,
        key="calendar"
    )

if __name__ == "__main__":
    show_historico_page()
