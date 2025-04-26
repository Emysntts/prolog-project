import streamlit as st
from pages.perfil import show_perfil_page
from pages.historico import show_historico_page

# Configuração da página
st.set_page_config(
    page_title="Ciclo Menstrual",
    page_icon="🌸",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Navegação simplificada
page = st.sidebar.radio("Navegação", ['Perfil', 'Histórico'])

if page == 'Perfil':
    show_perfil_page()
else:  # Histórico
    show_historico_page()