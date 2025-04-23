from datetime import datetime, timedelta
from pyswip import Prolog

# Inicializa o ambiente Prolog
prolog = Prolog()

# Carregar as regras do Prolog
prolog.consult("prolog_rules.pl")

def calcular_ovulacao(data_inicio, duracao_ciclo):
    """
    Calcula a data de ovulação com base na data de início do ciclo
    e na duração do ciclo menstrual.
    """
    data_inicio_obj = datetime.strptime(data_inicio, '%Y-%m-%d')
    ovulacao_data = data_inicio_obj + timedelta(days=duracao_ciclo - 14)  # Ovulação 14 dias antes da próxima menstruação
    return ovulacao_data

def calcular_periodo_fertil(data_ovulacao):
    """
    Calcula o período fértil com base na data de ovulação.
    O período fértil é 5 dias antes da ovulação e até 1 dia depois.
    """
    inicio_fertil = data_ovulacao - timedelta(days=5)
    fim_fertil = data_ovulacao + timedelta(days=1)
    return inicio_fertil, fim_fertil

def verificar_ciclo_irregular(duracao_ciclo):
    """
    Verifica se o ciclo menstrual está dentro da faixa de ciclos típicos (21 a 35 dias).
    """
    if duracao_ciclo < 21 or duracao_ciclo > 35:
        return True
    return False

def calcular_dias_entre_ciclos(data_inicio_ultimo_ciclo, data_inicio_atual_ciclo):
    """
    Calcula o número de dias entre o início de dois ciclos menstruais consecutivos.
    """
    data_inicio_ultimo = datetime.strptime(data_inicio_ultimo_ciclo, '%Y-%m-%d')
    data_inicio_atual = datetime.strptime(data_inicio_atual_ciclo, '%Y-%m-%d')
    return (data_inicio_atual - data_inicio_ultimo).days

def identificar_fase_do_ciclo(sintomas):
    """
    Usa a lógica do Prolog para identificar a fase do ciclo com base nos sintomas fornecidos.
    """
    sintomas_prolog = ', '.join(sintomas)  # Converte a lista de sintomas em string
    query = f"identificar_fase([{sintomas_prolog}], Fase)"
    resultados = list(prolog.query(query))
    
    if resultados:
        return resultados[0]["Fase"]
    else:
        return "Fase indefinida"

def calcular_ovulacao_e_fase(data_inicio, duracao_ciclo, sintomas):
    """
    Função principal para calcular a ovulação, o período fértil e determinar a fase do ciclo com base nos sintomas.
    """
    # Calcular a ovulação
    data_ovulacao = calcular_ovulacao(data_inicio, duracao_ciclo)
    
    # Calcular o período fértil
    inicio_fertil, fim_fertil = calcular_periodo_fertil(data_ovulacao)
    
    # Identificar a fase do ciclo com base nos sintomas
    fase = identificar_fase_do_ciclo(sintomas)
    
    # Retornar os resultados
    resultado = {
        "data_ovulacao": data_ovulacao.strftime('%Y-%m-%d'),
        "inicio_fertil": inicio_fertil.strftime('%Y-%m-%d'),
        "fim_fertil": fim_fertil.strftime('%Y-%m-%d'),
        "fase_ciclo": fase
    }
    
    return resultado
