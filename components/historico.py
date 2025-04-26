import streamlit as st
from datetime import datetime, timedelta
from streamlit_calendar import calendar
from pyswip import Prolog
import json

def init_prolog():
    prolog = Prolog()
    prolog.consult("prolog/ciclos.pl")
    prolog.consult("prolog/perfil.pl")
    return prolog

def decode_text(text):
    if isinstance(text, bytes):
        return text.decode('utf-8')
    return text

def create_cycle_event(title, start_date, end_date, color, opacity=1.0):

    end_date = end_date + timedelta(days=1)
    return {
        "title": title,
        "start": start_date.strftime("%Y-%m-%d"),
        "end": end_date.strftime("%Y-%m-%d"),
        "backgroundColor": f"rgba{tuple(int(color.lstrip('#')[i:i+2], 16) for i in (0, 2, 4)) + (opacity,)}",
        "borderColor": color
    }

def show_historico_page():
    st.title("🌸 Histórico")
    
    if 'nome' not in st.session_state:
        st.warning("Por favor, preencha seu perfil primeiro.")
        return
        
    nome = st.session_state.nome
    prolog = init_prolog()
    

    st.sidebar.subheader("Fases do Ciclo")
    fases = ["menstruacao", "folicular", "ovulacao", "lutea"]
    for fase in fases:
        query = f"explicacao_fase({fase}, Explicacao)"
        for resultado in prolog.query(query):
            st.sidebar.markdown(f"**{fase.title()}**")
            st.sidebar.write(decode_text(resultado["Explicacao"]))
            st.sidebar.markdown("---")
    

    cores = {
        "menstruacao": "#FFA4A4",  # Rosa claro
        "folicular": "#BBE9C0",    # Amarelo claro
        "ovulacao": "#FAF4B3",     # Laranja claro
        "lutea": "#C4D9FF",       # Verde claro
        "previsao": "#FFCCD3"      # Rosa mais escuro para previsões
    }
    
    events = []
    

    if st.session_state.ultimo_ciclo and st.session_state.duracao_ciclo:
        data_inicio = st.session_state.ultimo_ciclo
        duracao_ciclo = st.session_state.duracao_ciclo
        duracao_menstruacao = st.session_state.duracao_menstruacao
        

        end_date = data_inicio + timedelta(days=duracao_menstruacao-1)  # -1 porque o dia inicial já conta
        events.append(create_cycle_event("Menstruação", data_inicio, end_date, cores["menstruacao"]))
        

        folicular_start = data_inicio + timedelta(days=duracao_menstruacao)
        folicular_end = data_inicio + timedelta(days=(duracao_ciclo // 2)-1)
        events.append(create_cycle_event("Fase Folicular", folicular_start, folicular_end, cores["folicular"]))
        

        ovulacao_date = data_inicio + timedelta(days=duracao_ciclo // 2)
        events.append(create_cycle_event("Ovulação", ovulacao_date, ovulacao_date, cores["ovulacao"]))
        

        lutea_start = ovulacao_date + timedelta(days=1)
        lutea_end = data_inicio + timedelta(days=duracao_ciclo-1)
        events.append(create_cycle_event("Fase Lútea", lutea_start, lutea_end, cores["lutea"]))
        

        data_atual = data_inicio
        for i in range(2): 
            query = f"calcular_proxima_menstruacao('{nome.lower()}', date({data_atual.year},{data_atual.month},{data_atual.day}), {duracao_ciclo}, DataPrevista)"
            for resultado in prolog.query(query):
                data_prevista = resultado["DataPrevista"]
                ano = data_prevista.args[0] if hasattr(data_prevista, 'args') else int(str(data_prevista).split(',')[0].replace('date(', ''))
                mes = data_prevista.args[1] if hasattr(data_prevista, 'args') else int(str(data_prevista).split(',')[1])
                dia = data_prevista.args[2] if hasattr(data_prevista, 'args') else int(str(data_prevista).split(',')[2].replace(')', ''))
                
                data_prevista = datetime(ano, mes, dia).date()
                

                dias_ate = (data_prevista - datetime.now().date()).days
                if dias_ate > 0:
                    if i == 0:
                        st.warning(f" Sua próxima menstruação deve chegar em aproximadamente {dias_ate} dias")
                    else:
                        st.warning(f" A menstruação seguinte deve chegar em aproximadamente {dias_ate} dias")
                

                end_date = data_prevista + timedelta(days=duracao_menstruacao-1)
                events.append(create_cycle_event(
                    f"Menstruação Prevista ({i+1})", 
                    data_prevista, 
                    end_date, 
                    cores["previsao"],
                    0.8 - (i * 0.2) 
                    
                ))
                

                data_atual = data_prevista
    

    st.subheader("Sintomas Frequentes")
    if hasattr(st.session_state, 'sintomas_perfil') and st.session_state.sintomas_perfil:
        for sintoma in st.session_state.sintomas_perfil:
            st.write(f" • {sintoma} - Intensidade: {st.session_state.intensidade_perfil}/5")
    else:
        st.info("Nenhum sintoma registrado ainda")
    

    calendar_options = {
        "initialView": "dayGridMonth",
        "locale": "pt-br",
        "selectable": False,
        "editable": False,
        "headerToolbar": {
            "left": "prev,next today",
            "center": "title",
            "right": "dayGridMonth,timeGridWeek"
        }
    }
    

    st.markdown("""
    <div class="alerta-importante">
        <p>⚠️ As previsões podem ser afetadas por métodos contraceptivos hormonais e ciclos irregulares.</p>
        <p>🌼Para previsões mais precisas, é recomendado usar o aplicativo durante períodos sem uso de contraceptivos hormonais.</p>
    </div>
    """, unsafe_allow_html=True)


    st.subheader(" Calendário Menstrual")
    calendar(events=events, options=calendar_options)

if __name__ == "__main__":
    show_historico_page()
