% Fatos sobre o ciclo menstrual
ciclo_menstrual(28).  % Ciclo de 28 dias (valor típico)
ciclo_menstrual(30).  % Ciclo de 30 dias
ciclo_menstrual(32).  % Ciclo de 32 dias

% Regra para calcular a ovulação (14 dias antes da próxima menstruação)
ovulacao(DataInicio, DataOvulacao) :-
    ciclo_menstrual(Duracao),
    DataOvulacao is DataInicio + Duracao - 14.

% Regra para calcular o período fértil (5 dias antes da ovulação até 1 dia depois)
periodo_fertil(DataOvulacao, InicioPeriodoFertil, FimPeriodoFertil) :-
    InicioPeriodoFertil is DataOvulacao - 5,  % 5 dias antes da ovulação
    FimPeriodoFertil is DataOvulacao + 1.     % 1 dia após a ovulação

% Regra para ciclos irregulares (fora do intervalo típico de 21 a 35 dias)
ciclo_irregular(DataInicio, Duracao, Ovulacao) :-
    ovulacao(DataInicio, Ovulacao),  % Calcula a ovulação com base na duração fornecida
    (Duracao < 21; Duracao > 35),  % Ciclo fora da faixa normal
    format('Ciclo irregular detectado. Ciclo de ~w dias.', [Duracao]).

% Sintomas associados às diferentes fases do ciclo menstrual
sintomas_folicular(fase_folicular, [dor_abdominal, alteracao_humor]).
sintomas_ovulacao(ovulacao, [aumento_secrecao, dor_ovulatoria]).
sintomas_luteal(fase_luteal, [fadiga, irritabilidade]).

% Regra para prever a ovulação e o período fértil baseado no início da menstruação
prever_ovulacao(DataInicio, Previsao) :-
    ovulacao(DataInicio, DataOvulacao),
    periodo_fertil(DataOvulacao, Inicio, Fim),
    Previsao = [DataOvulacao, Inicio, Fim].

% Função para calcular os sintomas baseados na fase do ciclo
prever_sintomas(Fase, Sintomas) :-
    (Fase == fase_folicular -> sintomas_folicular(Fase, Sintomas);
     Fase == ovulacao -> sintomas_ovulacao(Fase, Sintomas);
     Fase == fase_luteal -> sintomas_luteal(Fase, Sintomas)).

% Função principal para calcular a ovulação e o período fértil
calcular_ovulacao_e_fase(DataInicio, Resultado) :-
    prever_ovulacao(DataInicio, Previsao),
    format('Previsão de Ovulação: ~w~n', [Previsao]),
    (Previsao = [DataOvulacao, Inicio, Fim] ->
        prever_sintomas(fase_folicular, SintomasFaseFolicular),
        format('Sintomas na fase folicular: ~w~n', [SintomasFaseFolicular]),
        prever_sintomas(ovulacao, SintomasOvulacao),
        format('Sintomas na ovulação: ~w~n', [SintomasOvulacao]),
        prever_sintomas(fase_luteal, SintomasFaseLuteal),
        format('Sintomas na fase luteal: ~w~n', [SintomasFaseLuteal]),
        Resultado = [DataOvulacao, Inicio, Fim]
        Resultado = 'Erro ao calcular a ovulação ou a fase do ciclo').

% Sintomas comuns em cada fase do ciclo menstrual
sintomas_folicular([dor_abdominal, alteracao_humor]).
sintomas_ovulacao([aumento_secrecao, dor_ovulatoria]).
sintomas_luteal([fadiga, irritabilidade]).

% Regra para identificar a fase do ciclo com base nos sintomas fornecidos
identificar_fase(Sintomas, Fase) :-
    sintomas_folicular(SintomasFaseFolicular),
    subset(SintomasFaseFolicular, Sintomas),  % Verifica se todos os sintomas da fase folicular estão presentes
    Fase = fase_folicular.

identificar_fase(Sintomas, Fase) :-
    sintomas_ovulacao(SintomasFaseOvulacao),
    subset(SintomasFaseOvulacao, Sintomas),  % Verifica se todos os sintomas da ovulação estão presentes
    Fase = ovulacao.

identificar_fase(Sintomas, Fase) :-
    sintomas_luteal(SintomasFaseLuteal),
    subset(SintomasFaseLuteal, Sintomas),  % Verifica se todos os sintomas da fase luteal estão presentes
    Fase = fase_luteal.

% Caso o conjunto de sintomas não se encaixe claramente em uma fase
identificar_fase(_, fase_indefinida) :- 
    write('Sintomas não correspondem claramente a uma fase do ciclo.').

