import streamlit as st
from components.perfil import show_perfil_page
from components.historico import show_historico_page

st.set_page_config(
    page_title="Ciclo Menstrual",
    page_icon="🌸",
    layout="wide",
    initial_sidebar_state="expanded"
)


with open('src/streamlit_app/style.css') as f:
    st.markdown(f'<style>{f.read()}</style>', unsafe_allow_html=True)


hide_streamlit_style = """
        <style>
        #MainMenu {visibility: hidden;}
        footer {visibility: hidden;}
        [data-testid="collapsedControl"] {display: none;}
        section[data-testid="stSidebar"] > div:first-child {
            padding-top: 2rem;
        }
        div[data-testid="stToolbar"] {display: none;}
        .stDeployButton {display: none;}
        header[data-testid="stHeader"] {display: none;}
        </style>
        """
st.markdown(hide_streamlit_style, unsafe_allow_html=True)


page = st.sidebar.radio("Navegação", ['Perfil', 'Histórico'])

if page == 'Perfil':
    show_perfil_page()
else:  
    show_historico_page()