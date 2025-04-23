% Arquivo: ciclos.pl

% Fato que armazena os ciclos menstruais
% ciclo(Usuario, DataInicio, DuracaoCiclo, DuracaoMenstruacao)
% Exemplo:
% ciclo(maria, date(2025,4,1), 28, 5).

:- dynamic ciclo/4.

% Adicionar novo ciclo
adicionar_ciclo(Usuario, DataInicio, DuracaoCiclo, DuracaoMenstruacao) :-
    assertz(ciclo(Usuario, DataInicio, DuracaoCiclo, DuracaoMenstruacao)).

% Obter o último ciclo registrado de um usuário
ultimo_ciclo(Usuario, DataInicio, DuracaoCiclo, DuracaoMenstruacao) :-
    findall((Data, DurC, DurM), ciclo(Usuario, Data, DurC, DurM), Lista),
    last(Lista, (DataInicio, DuracaoCiclo, DuracaoMenstruacao)).

% Calcular a próxima data de ciclo para um usuário
proximo_ciclo(Usuario, ProximaData) :-
    ultimo_ciclo(Usuario, date(Y,M,D), DuracaoCiclo, _),
    date_time_stamp(date(Y,M,D), Stamp),
    ProxStamp is Stamp + DuracaoCiclo * 86400,
    stamp_date_time(ProxStamp, DateTime, 'UTC'),
    date_time_value(date, DateTime, ProximaData).

% Consultar a duração do ciclo anterior
duracao_ciclo_anterior(Usuario, Duracao) :-
    ultimo_ciclo(Usuario, _, Duracao, _).

% Checar regularidade do ciclo (variação máxima de 2 dias)
ciclo_regular(Usuario) :-
    findall(Dur, ciclo(Usuario, _, Dur, _), Duracoes),
    max_list(Duracoes, Max), min_list(Duracoes, Min),
    Diff is Max - Min, Diff =< 2.

% Gerar alerta se o próximo ciclo estiver próximo (em até 3 dias)
alerta_proximo_ciclo(Usuario, Alerta) :-
    proximo_ciclo(Usuario, date(Y,M,D)),
    get_time(NowStamp),
    date_time_stamp(date(Y,M,D), ProxStamp),
    DiffDays is round((ProxStamp - NowStamp) / 86400),
    (DiffDays =< 3, DiffDays >= 0 ->
        format(atom(Alerta), 'Atenção: próximo ciclo de ~w em ~d dias.', [Usuario, DiffDays])
    ;
        Alerta = 'Sem alerta para o próximo ciclo.'
    ).

% Calcula o período fértil de uma usuária baseado no último ciclo
periodo_fertil(Usuario, DataInicioFertil, DataFimFertil) :-
    ultimo_ciclo(Usuario, date(Y,M,D), DuracaoCiclo, _),
    OvulacaoDia is DuracaoCiclo - 14,
    DataOvulacao is D + OvulacaoDia,
    DataInicioF is DataOvulacao - 4, % 4 dias antes da ovulação
    DataFimF is DataOvulacao + 1,    % 1 dia após a ovulação
    % Calcula datas reais (ajustar para meses/anos, se necessário)
    date_time_stamp(date(Y,M,D), Stamp),
    StampInicio is Stamp + (DataInicioF - 1) * 86400,
    StampFim is Stamp + (DataFimF - 1) * 86400,
    stamp_date_time(StampInicio, DTInicio, 'UTC'),
    stamp_date_time(StampFim, DTFim, 'UTC'),
    date_time_value(date, DTInicio, DataInicioFertil),
    date_time_value(date, DTFim, DataFimFertil).

% Calcula os dias de maior chance de gravidez (dia da ovulação e o anterior)
alta_chance_gravidez(Usuario, DataAlta1, DataAlta2) :-
    ultimo_ciclo(Usuario, date(Y,M,D), DuracaoCiclo, _),
    OvulacaoDia is DuracaoCiclo - 14,
    DataOvulacao is D + OvulacaoDia,
    DataAlta1Dia is DataOvulacao - 1, % Dia anterior à ovulação
    DataAlta2Dia is DataOvulacao,     % Dia da ovulação
    date_time_stamp(date(Y,M,D), Stamp),
    StampAlta1 is Stamp + (DataAlta1Dia - 1) * 86400,
    StampAlta2 is Stamp + (DataAlta2Dia - 1) * 86400,
    stamp_date_time(StampAlta1, DTAlta1, 'UTC'),
    stamp_date_time(StampAlta2, DTAlta2, 'UTC'),
    date_time_value(date, DTAlta1, DataAlta1),
    date_time_value(date, DTAlta2, DataAlta2).