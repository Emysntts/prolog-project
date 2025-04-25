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

% Checar regularidade do ciclo (variação maxima de 2 dias)
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

% Calcula o período fértil de uma usuaria baseado no último ciclo
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

% Fase Menstrual (início e término da menstruação)
fase_menstrual(Usuario, DataInicioMenstruacao, DataFimMenstruacao) :-
    ultimo_ciclo(Usuario, date(Ano, Mes, Dia), _, DuracaoMenstruacao),
    DataInicioMenstruacao = date(Ano, Mes, Dia),
    % Calcula o final da menstruação
    DataFimMenstruacao is Dia + DuracaoMenstruacao,
    % Ajuste para o fim do mes, se necessário
    (DataFimMenstruacao > 30 -> 
        ProximoMes is Mes + 1,
        ProximoDia is DataFimMenstruacao - 30,
        DataFimMenstruacao = date(Ano, ProximoMes, ProximoDia)
    ; DataFimMenstruacao = date(Ano, Mes, DataFimMenstruacao)).

% Fase Folicular (duração da fase após a menstruação até a ovulação)
fase_folicular(Usuario, DataInicioFolicular, DataFimFolicular) :-
    ultimo_ciclo(Usuario, date(Ano, Mes, Dia), DuracaoCiclo, DuracaoMenstruacao),
    % A fase folicular começa após a menstruação
    DataInicioFolicular is Dia + DuracaoMenstruacao,
    % A fase folicular termina 14 dias antes da ovulação
    DataFimFolicular is DataInicioFolicular + 14.

% Ovulação (calcula a data da ovulação, 14 dias antes do final do ciclo)
ovulacao(Usuario, DataOvulacao) :-
    ultimo_ciclo(Usuario, date(Ano, Mes, Dia), DuracaoCiclo, _),
    OvulacaoDia is DuracaoCiclo - 14,  % Ovulação acontece 14 dias antes do final
    DataOvulacao is Dia + OvulacaoDia.

% Fase Lútea (após a ovulação até o início do próximo ciclo, geralmente 14 dias)
fase_lutea(Usuario, DataInicioLutea, DataFimLutea) :-
    ultimo_ciclo(Usuario, date(Ano, Mes, Dia), DuracaoCiclo, _),
    OvulacaoDia is DuracaoCiclo - 14,
    DataInicioLutea is Dia + OvulacaoDia,
    DataFimLutea is DataInicioLutea + 14.

% Fases do Ciclo (menstrual, folicular, ovulação e lútea)
fases_do_ciclo(Usuario, FaseMenstrual, FaseFolicular, Ovulacao, FaseLutea) :-
    fase_menstrual(Usuario, DataInicioMenstruacao, DataFimMenstruacao),
    fase_folicular(Usuario, DataInicioFolicular, DataFimFolicular),
    ovulacao(Usuario, DataOvulacao),
    fase_lutea(Usuario, DataInicioLutea, DataFimLutea),
    FaseMenstrual = (DataInicioMenstruacao, DataFimMenstruacao),
    FaseFolicular = (DataInicioFolicular, DataFimFolicular),
    Ovulacao = DataOvulacao,
    FaseLutea = (DataInicioLutea, DataFimLutea).


% Fatos que associam sintomas a cada fase do ciclo menstrual
sintomas_fase(menstruacao, [
    "Inchaço", "Dores de cabeça", "Cansaço", "Alterações de humor", 
    "Cólicas", "Retenção de líquidos", "Mudanças no apetite"
]).
sintomas_fase(fase_folicular, [
    "Cansaço", "Alterações de humor", "Aumento de energia", 
    "Melhora no humor", "Sensibilidade nas mamas"
]).
sintomas_fase(ovulacao, [
    "Dor abdominal", "Tensão nos seios", "Aumento da libido", 
    "Mudanças no muco cervical", "Dor no baixo ventre"
]).
sintomas_fase(fase_lutea, [
    "Inchaço", "Dores de cabeça", "Náusea", "Tensão nos seios", 
    "Cansaço", "Alterações de humor", "Retenção de líquidos", "Vontade de comer"
]).

% Regra para prever a fase do ciclo com base nos sintomas fornecidos
prever_fase_do_ciclo(Sintomas, FasesPrevistas) :-
    findall(Fase, (
        sintomas_fase(Fase, SintomasFase), 
        subset(SintomasFase, Sintomas)
    ), FasesPrevistas),
    (FasesPrevistas = [] -> 
        FasesPrevistas = ["Fase desconhecida, consulte um especialista."]
    ; true).


salvar_dados :-
    tell('ciclos.pl'),
    listing(ciclo),
    told.

carregar_dados :-
    [ciclos].  % Carrega o arquivo ciclos.pl