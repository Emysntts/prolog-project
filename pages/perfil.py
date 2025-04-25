import streamlit as st
from pyswip import Prolog

def init_prolog():
    prolog = Prolog()
    prolog.consult("prolog/ciclos.pl")
    prolog.consult("prolog/perfil.pl")
    return prolog

def salvar_usuario(prolog, nome, ano_nasc, peso, altura, regular, metodo, problemas):
    if not nome.strip():
        st.error("Por favor, preencha seu nome.")
        return False
    
    nome_atom = nome.strip().lower()
    query = f"cadastrar_usuario('{nome_atom}', {ano_nasc}, {peso}, {altura}, '{regular}', '{metodo}', '{problemas}')"
    list(prolog.query(query))
    return True

def decode_recomendacao(rec):
    if isinstance(rec, bytes):
        return rec.decode('utf-8')
    return rec

def obter_recomendacoes(prolog, nome):
    if not nome.strip():
        return []
        
    nome_atom = nome.strip().lower()
    query = f"gerar_recomendacoes('{nome_atom}', Recomendacoes)"
    result = list(prolog.query(query))
    if result:
        return [decode_recomendacao(rec) for rec in result[0]["Recomendacoes"]]
    return []

def show_perfil_page():
    st.title("Perfil da Usuária")
    prolog = init_prolog()
    
    # Formulário de perfil
    with st.form("perfil_form"):
        nome = st.text_input("Nome")
        col1, col2 = st.columns(2)
        
        with col1:
            ano_nasc = st.number_input("Ano de Nascimento", min_value=1950, max_value=2010, value=2000)
            peso = st.number_input("Peso (kg)", min_value=30.0, max_value=200.0, value=60.0)
            regular = st.selectbox("Sua menstruação é regular?", ["regular", "irregular"])
        
        with col2:
            altura = st.number_input("Altura (cm)", min_value=100.0, max_value=220.0, value=165.0)
            metodo = st.selectbox("Método Contraceptivo", 
                                ["nenhum", "pilula", "diu", "implante", "adesivo", "anel", "injecao"])
            problemas = st.multiselect("Problemas de Saúde", 
                                     ["nenhum", "endometriose", "sop", "mioma", "outros"])
        
        st.markdown("---")
        st.subheader("Informações do Ciclo")
        
        ultima_menstruacao = st.date_input("Data da última menstruação")
        lembra_data = st.checkbox("Não lembro a data exata")
        
        if lembra_data:
            st.write("Sem problemas! Vamos tentar identificar sua fase atual pelos sintomas.")
            sintomas = st.multiselect(
                "Quais sintomas você está sentindo?",
                ["Cólicas", "TPM", "Dor nos seios", "Mudanças no muco cervical",
                 "Alterações de humor", "Dor abdominal", "Inchaço"]
            )
        
        submitted = st.form_submit_button("Salvar Perfil")
        if submitted:
            problemas_str = ','.join(problemas) if problemas else 'nenhum'
            if salvar_usuario(prolog, nome, ano_nasc, peso, altura, regular, metodo, problemas_str):
                st.success("Perfil salvo com sucesso!")
                
                # Mostrar recomendações personalizadas
                st.subheader("Recomendações Personalizadas")
                recomendacoes = obter_recomendacoes(prolog, nome)
                if recomendacoes:
                    for rec in recomendacoes:
                        st.info(rec)
                else:
                    st.warning("Não foi possível gerar recomendações. Por favor, verifique os dados informados.")
                
                # Salvar na sessão
                st.session_state.nome = nome
                st.session_state.ultima_menstruacao = ultima_menstruacao
                if lembra_data:
                    st.session_state.sintomas = sintomas

if __name__ == "__main__":
    show_perfil_page()
