
previsao_menstruacao(Usuario, ProximoCiclo) :-
    ciclo(Usuario, date(Ano, Mes, Dia), DuracaoCiclo, _),
    ProximoDia is Dia + DuracaoCiclo,
    ProximoMes is Mes + (ProximoDia // 30),
    ProximoAno is Ano + (ProximoMes // 12),
    ProximoDiaFinal is ProximoDia mod 30,
    ProximoCiclo = date(ProximoAno, ProximoMes, ProximoDiaFinal).
