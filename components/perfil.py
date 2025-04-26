import streamlit as st
from pyswip import Prolog
from datetime import datetime, timedelta

def init_prolog():
    prolog = Prolog()
    prolog.consult("prolog/ciclos.pl")
    prolog.consult("prolog/perfil.pl")
    return prolog

def decode_text(text):
    if isinstance(text, bytes):
        return text.decode('utf-8')
    return text

def show_perfil_page():
    st.title("Perfil")
    
    prolog = init_prolog()
    
    with st.form("perfil_form"):
        nome = st.text_input("Nome", value=st.session_state.get('nome', ''))
        col1, col2 = st.columns(2)
        
        with col1:
            ano_nascimento = st.number_input("Ano de Nascimento", 
                                           min_value=1950, 
                                           max_value=2010, 
                                           value=st.session_state.get('ano_nascimento', 1990))
            peso = st.number_input("Peso (kg)", 
                                 min_value=30.0, 
                                 max_value=200.0, 
                                 step=0.1,
                                 value=st.session_state.get('peso', 60.0))
            altura = st.number_input("Altura (cm)", 
                                   min_value=100, 
                                   max_value=250,
                                   value=st.session_state.get('altura', 160))
        
        with col2:
            regularidade = st.selectbox(
                "Seu ciclo é regular?",
                ["regular", "irregular"],
                index=["regular", "irregular"].index(st.session_state.get('regularidade', "regular"))
            )
            
            metodos = {
                "nenhum": "nenhum",
                "pilula_adesivo_anel": "pílula, adesivo ou anel",
                "pilula_progesterona": "pílula só de progesterona",
                "implante_injecao": "implante ou injeção",
                "sistema_intrauterino": "sistema intrauterino",
                "diu_sem_hormonio": "DIU sem hormônio",
                "preservativo": "preservativo"
            }
            
            metodo = st.selectbox(
                "Método Contraceptivo",
                list(metodos.keys()),
                format_func=lambda x: metodos[x],
                index=list(metodos.keys()).index(st.session_state.get('metodo', "nenhum"))
            )
            
            problemas_dict = {
                "sop": "síndrome dos ovários policísticos (SOP)",
                "endometriose": "endometriose",
                "miomas": "miomas",
                "nenhum": "nenhuma/não tenho certeza"
            }
            
            problemas = st.selectbox(
                "Condições de Saúde",
                list(problemas_dict.keys()),
                format_func=lambda x: problemas_dict[x],
                index=list(problemas_dict.keys()).index(st.session_state.get('problemas', "nenhum"))
            )
        
        st.subheader("Informações do Último Ciclo")
        col3, col4 = st.columns(2)
        
        with col3:
            data_ultimo_ciclo = st.date_input("Primeiro dia do último ciclo", 
                                            value=st.session_state.get('ultimo_ciclo', datetime.now().date()))
            
            duracao_ciclo = st.number_input("Duração do ciclo (dias)", 
                                          min_value=21, 
                                          max_value=45, 
                                          value=st.session_state.get('duracao_ciclo', 28),
                                          help="Tempo entre o primeiro dia de uma menstruação até o dia anterior da próxima")
            nao_sei_duracao = st.checkbox("Não sei a duração do ciclo", 
                                        value=st.session_state.get('nao_sei_duracao', False))
            if nao_sei_duracao:
                duracao_ciclo = 28  # Duração padrão
        
        with col4:
            duracao_menstruacao = st.number_input("Duração da menstruação (dias)", 
                                                min_value=1, 
                                                max_value=10, 
                                                value=st.session_state.get('duracao_menstruacao', 5))
            
            sintomas = st.multiselect(
                "Sintomas frequentes",
                ["Cólicas", "TPM", "Dor nos seios", "Mudanças no muco cervical",
                 "Alterações de humor", "Dor abdominal", "Inchaço"],
                default=st.session_state.get('sintomas_perfil', [])
            )
            
            intensidade = st.slider("Intensidade média dos sintomas", 
                                  1, 5, 
                                  value=st.session_state.get('intensidade_perfil', 3))
        
        submitted = st.form_submit_button("Salvar")
        
        if submitted and nome:
            # Salvar perfil
            query = f"cadastrar_usuario('{nome.lower()}', {ano_nascimento}, {peso}, {altura}, '{regularidade}', '{metodo}', '{problemas}')"
            list(prolog.query(query))
            
            # Salvar ciclo inicial
            ciclo_query = f"assertz(historico_menstrual('{nome.lower()}', date({data_ultimo_ciclo.year},{data_ultimo_ciclo.month},{data_ultimo_ciclo.day}), {duracao_ciclo}, {duracao_menstruacao}))"
            list(prolog.query(ciclo_query))
            
            # Armazenar no session state para uso em outras páginas
            st.session_state.nome = nome
            st.session_state.ultimo_ciclo = data_ultimo_ciclo
            st.session_state.duracao_ciclo = duracao_ciclo
            st.session_state.duracao_menstruacao = duracao_menstruacao
            st.session_state.sintomas_perfil = sintomas
            st.session_state.intensidade_perfil = intensidade
            st.session_state.nao_sei_duracao = nao_sei_duracao  # Armazenar a informação de não saber duração
            
            # Armazenar dados do formulário
            st.session_state.ano_nascimento = ano_nascimento
            st.session_state.peso = peso
            st.session_state.altura = altura
            st.session_state.regularidade = regularidade
            st.session_state.metodo = metodo
            st.session_state.problemas = problemas
            
            st.success("Perfil salvo com sucesso!")
            
            # Mostrar alertas e recomendações relevantes
            st.subheader("Informações Importantes")
            
            # Alerta do método contraceptivo
            query_metodo = f"alerta_metodo('{metodo}', Alerta)"
            for resultado in prolog.query(query_metodo):
                st.info(decode_text(resultado["Alerta"]))
            
            # Alerta de condições de saúde
            if problemas != "nenhum":
                query_saude = f"alerta_saude('{problemas}', Alerta)"
                for resultado in prolog.query(query_saude):
                    st.warning(decode_text(resultado["Alerta"]))
            
            # Recomendações personalizadas
            st.subheader("Recomendações Personalizadas")
            query_rec = f"gerar_recomendacoes('{nome.lower()}', Recomendacoes)"
            for resultado in prolog.query(query_rec):
                for rec in resultado["Recomendacoes"]:
                    st.write("• " + decode_text(rec))

if __name__ == "__main__":
    show_perfil_page()
