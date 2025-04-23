% Fatos que armazenam dados sobre os ciclos menstruais passados
% ciclo_menstrual(DataInicio, Duracao)
ciclo_menstrual(data(2025, 4, 22), 28).
ciclo_menstrual(data(2025, 5, 20), 30).
ciclo_menstrual(data(2025, 6, 18), 32).

% Sintomas associados a cada ciclo (opcional, dependendo do sistema)
sintomas_ciclo(data(2025, 4, 22), [dor_abdominal, alteracao_humor]).
sintomas_ciclo(data(2025, 5, 20), [aumento_secrecao, dor_ovulatoria]).
sintomas_ciclo(data(2025, 6, 18), [fadiga, irritabilidade]).

% Regra para calcular a data de início do próximo ciclo com base na média das durações anteriores
calcular_proximo_ciclo(DataProximoCiclo) :-
    findall(Duracao, ciclo_menstrual(_, Duracao), Duracoes),
    sum_list(Duracoes, SomaDuracao),
    length(Duracoes, QtdCiclos),
    MediaDuracao is SomaDuracao / QtdCiclos,
    ciclo_menstrual(data(UltimaData, _, _), _), % Pega a última data
    DataProximoCiclo is UltimaData + MediaDuracao.

% Regra para verificar se o ciclo é regular
ciclo_regular :- 
    findall(Duracao, ciclo_menstrual(_, Duracao), Duracoes),
    min_list(Duracoes, MinDuracao),
    max_list(Duracoes, MaxDuracao),
    MaxDuracao - MinDuracao =< 5. % Diferença máxima de 5 dias para ser considerado regular

% Regra para calcular o período fértil com base na data de início
periodo_fertil(DataInicio, PeriodoInicio, PeriodoFim) :-
    ciclo_menstrual(DataInicio, Duracao),
    Ovulacao is DataInicio + Duracao - 14,
    PeriodoInicio is Ovulacao - 5,  % 5 dias antes da ovulação
    PeriodoFim is Ovulacao + 1.     % 1 dia após a ovulação
