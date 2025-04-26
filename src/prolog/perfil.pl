% Fatos para armazenar informações do perfil
:- dynamic usuario/7.
% usuario(Nome, AnoNascimento, Peso, Altura, RegularidadeMenstrual, MetodoContraceptivo, ProblemasSaude).

% Fatos para armazenar histórico menstrual
:- dynamic historico_menstrual/4.
% historico_menstrual(Nome, DataInicio, DuracaoCiclo, DuracaoMenstruacao).

% Predicados para gerenciar perfil
cadastrar_usuario(Nome, AnoNasc, Peso, Altura, Regular, Metodo, Problemas) :-
    retractall(usuario(Nome, _, _, _, _, _, _)),
    assertz(usuario(Nome, AnoNasc, Peso, Altura, Regular, Metodo, Problemas)).

atualizar_usuario(Nome, Campo, NovoValor) :-
    usuario(Nome, AnoNasc, Peso, Altura, Regular, Metodo, Problemas),
    retract(usuario(Nome, AnoNasc, Peso, Altura, Regular, Metodo, Problemas)),
    atualizar_campo(Campo, Nome, AnoNasc, Peso, Altura, Regular, Metodo, Problemas, NovoValor).

% Predicados auxiliares para atualização
atualizar_campo(ano_nascimento, Nome, _, Peso, Altura, Regular, Metodo, Problemas, Valor) :-
    assertz(usuario(Nome, Valor, Peso, Altura, Regular, Metodo, Problemas)).
atualizar_campo(peso, Nome, AnoNasc, _, Altura, Regular, Metodo, Problemas, Valor) :-
    assertz(usuario(Nome, AnoNasc, Valor, Altura, Regular, Metodo, Problemas)).
atualizar_campo(altura, Nome, AnoNasc, Peso, _, Regular, Metodo, Problemas, Valor) :-
    assertz(usuario(Nome, AnoNasc, Peso, Valor, Regular, Metodo, Problemas)).
atualizar_campo(regularidade, Nome, AnoNasc, Peso, Altura, _, Metodo, Problemas, Valor) :-
    assertz(usuario(Nome, AnoNasc, Peso, Altura, Valor, Metodo, Problemas)).
atualizar_campo(metodo, Nome, AnoNasc, Peso, Altura, Regular, _, Problemas, Valor) :-
    assertz(usuario(Nome, AnoNasc, Peso, Altura, Regular, Valor, Problemas)).
atualizar_campo(problemas, Nome, AnoNasc, Peso, Altura, Regular, Metodo, _, Valor) :-
    assertz(usuario(Nome, AnoNasc, Peso, Altura, Regular, Metodo, Valor)).

% Predicados para análise de ciclo
ciclo_muito_irregular(Nome) :-
    findall(Duracao, historico_menstrual(Nome, _, Duracao, _), Duracoes),
    length(Duracoes, Len),
    Len >= 3,
    desvio_padrao(Duracoes, Desvio),
    Desvio > 5.

% Predicado para calcular média de duração do ciclo
media_duracao_ciclo(Nome, Media) :-
    findall(Duracao, historico_menstrual(Nome, _, Duracao, _), Duracoes),
    length(Duracoes, Len),
    Len > 0,
    sum_list(Duracoes, Soma),
    Media is Soma / Len.

% Predicados para análise de ciclo baseado no perfil
pode_prever_ciclo(Nome) :-
    usuario(Nome, _, _, _, Regular, Metodo, Problemas),
    Regular \= irregular,
    Metodo \= hormonios,
    Metodo \= diu_hormonal,
    \+ sub_atom(Problemas, _, _, _, sop),
    \+ sub_atom(Problemas, _, _, _, endometriose).

% Predicado para adicionar dias a uma data
date_add(date(Y,M,D), Days, date(Y2,M2,D2)) :-
    date_time_stamp(date(Y,M,D,0,0,0,0,-,-), Stamp),
    NewStamp is Stamp + Days * 86400,
    stamp_date_time(NewStamp, DateTime, 'UTC'),
    date_time_value(year, DateTime, Y2),
    date_time_value(month, DateTime, M2),
    date_time_value(day, DateTime, D2).

% Previsão de ciclos futuros
prever_proximos_ciclos(Nome, DataUltimo, DuracaoPadrao, NumCiclos, Previsoes) :-
    pode_prever_ciclo(Nome),
    findall(Data, 
        (between(1, NumCiclos, N),
         Dias is DuracaoPadrao * N,
         date_add(DataUltimo, Dias, Data)),
        Previsoes).

% Ajustar previsão baseado na idade
ajustar_duracao_ciclo(Nome, DuracaoPadrao, DuracaoAjustada) :-
    usuario(Nome, AnoNasc, _, _, _, _, _),
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, 'UTC'),
    date_time_value(year, DateTime, AnoAtual),
    Idade is AnoAtual - AnoNasc,
    (Idade < 18 -> 
        DuracaoAjustada is DuracaoPadrao + 2;
     Idade > 45 ->
        DuracaoAjustada is DuracaoPadrao - 2;
     DuracaoAjustada = DuracaoPadrao).

% Explicações das fases do ciclo com emojis
explicacao_fase(menstruacao, '🌺 Durante a menstruação, o revestimento do útero é eliminado. Esta fase dura tipicamente de 3 a 7 dias. Sintomas comuns incluem cólicas, fadiga e mudanças de humor.').
explicacao_fase(folicular, '🌱 A fase folicular começa com a menstruação e termina na ovulação. Neste período, os folículos se desenvolvem e os níveis de estrogênio aumentam, preparando o corpo para uma possível gravidez.').
explicacao_fase(ovulacao, '🥚 A ovulação ocorre quando um óvulo é liberado do ovário, geralmente no meio do ciclo. Este é o período mais fértil, com maior chance de gravidez. Pode haver mudanças no muco cervical e leve dor abdominal.').
explicacao_fase(lutea, '🌙 A fase lútea vai da ovulação até o início da próxima menstruação. O corpo se prepara para uma possível gravidez. Se não houver fertilização, os níveis hormonais caem, levando à menstruação.').

% Predicado para calcular próxima menstruação com base no perfil
calcular_proxima_menstruacao(Nome, DataUltima, DuracaoPadrao, DataPrevista) :-
    usuario(Nome, AnoNasc, Peso, Altura, Regular, Metodo, Problemas),
    % Ajuste baseado na idade
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, 'UTC'),
    date_time_value(year, DateTime, AnoAtual),
    Idade is AnoAtual - AnoNasc,
    (Idade < 18 -> AjusteIdade is 2;
     Idade > 45 -> AjusteIdade is -2;
     AjusteIdade is 0),
    
    % Ajuste baseado no IMC
    AlturaMetros is Altura / 100,
    IMC is Peso / (AlturaMetros * AlturaMetros),
    (IMC < 18.5 -> AjusteIMC is 2;
     IMC > 30 -> AjusteIMC is 1;
     AjusteIMC is 0),
    
    % Ajuste baseado na regularidade
    (Regular = regular -> AjusteRegular is 0;
     Regular = irregular -> AjusteRegular is 3;
     AjusteRegular is 1),
    
    % Cálculo final
    DuracaoAjustada is DuracaoPadrao + AjusteIdade + AjusteIMC + AjusteRegular,
    date_add(DataUltima, DuracaoAjustada, DataPrevista).

% Predicado para registrar sintomas
:- dynamic sintoma_registrado/5.
% sintoma_registrado(Nome, Data, Sintoma, Intensidade, Fase).

registrar_sintoma(Nome, Data, Sintoma, Intensidade, Fase) :-
    assertz(sintoma_registrado(Nome, Data, Sintoma, Intensidade, Fase)).

% Predicado para obter histórico de sintomas
obter_historico_sintomas(Nome, Sintomas) :-
    findall([Data, Sintoma, Intensidade, Fase],
            sintoma_registrado(Nome, Data, Sintoma, Intensidade, Fase),
            Sintomas).

% Alertas e recomendações baseados no método contraceptivo
alerta_metodo(hormonios, 'O uso de anticoncepcionais hormonais altera o ciclo natural. As previsões do calendário podem não ser precisas.').
alerta_metodo(diu_hormonal, 'O DIU hormonal pode alterar ou até interromper o ciclo menstrual. As previsões do calendário podem não ser precisas.').
alerta_metodo(diu_cobre, 'O DIU de cobre pode causar sangramentos mais intensos e longos nos primeiros meses.').
alerta_metodo(nenhum, 'Sem uso de métodos hormonais, o calendário pode ser mais preciso para prever os ciclos.').

% Alertas e recomendações baseados em condições de saúde
alerta_saude(sop, 'A Síndrome dos Ovários Policísticos pode causar ciclos irregulares. As previsões podem não ser precisas.').
alerta_saude(endometriose, 'A endometriose pode afetar o ciclo menstrual e causar sangramentos irregulares.').
alerta_saude(nenhum, 'Sem condições de saúde que afetem o ciclo, as previsões tendem a ser mais precisas.').

% Predicados para gerar recomendações personalizadas
gerar_recomendacoes(Nome, Recomendacoes) :-
    usuario(Nome, _, _, _, Regular, Metodo, Problemas),
    findall(Rec, (
        recomendacao_base(Nome, Rec);
        recomendacao_regularidade(Regular, Rec);
        recomendacao_contraceptivo(Metodo, Rec);
        recomendacao_saude(Problemas, Rec);
        recomendacao_ciclo_desconhecido(Nome, Rec)
    ), Recomendacoes).

% Base de recomendações
recomendacao_base(Nome, "Mantenha um registro regular do seu ciclo para previsões mais precisas.") :-
    usuario(Nome, _, _, _, _, _, _).
recomendacao_base(Nome, "Consulte um profissional de saúde regularmente para acompanhamento.") :-
    usuario(Nome, _, _, _, _, _, _).

% Recomendações para ciclos com duração desconhecida
recomendacao_ciclo_desconhecido(Nome, "Tente registrar as datas do seu ciclo por pelo menos 3 meses para identificar padrões.") :-
    \+ media_duracao_ciclo(Nome, _).
recomendacao_ciclo_desconhecido(Nome, "Use um aplicativo ou calendário para marcar o início de cada menstruação.") :-
    \+ media_duracao_ciclo(Nome, _).
recomendacao_ciclo_desconhecido(Nome, "Observe e anote seus sintomas para ajudar a identificar as fases do ciclo.") :-
    \+ media_duracao_ciclo(Nome, _).

% Base de recomendações específicas
recomendacao_regularidade(irregular, "Mantenha um diário detalhado dos seus sintomas para identificar padrões.").
recomendacao_regularidade(irregular, "Considere consultar um ginecologista para avaliar a irregularidade.").
recomendacao_regularidade(regular, "Continue monitorando seu ciclo para manter o acompanhamento.").

recomendacao_contraceptivo(pilula, "Tome a pílula no mesmo horário todos os dias.").
recomendacao_contraceptivo(diu, "Faça checkups regulares para verificar o posicionamento do DIU.").
recomendacao_contraceptivo(nenhum, "Use o calendário fértil com cautela se estiver evitando gravidez.").

recomendacao_saude(Problemas, "Monitore a intensidade das cólicas e outros sintomas.") :-
    sub_atom(Problemas, _, _, _, endometriose).
recomendacao_saude(Problemas, "Mantenha um registro de alterações no ciclo e sintomas relacionados.") :-
    sub_atom(Problemas, _, _, _, sop).
recomendacao_saude(Problemas, "Mantenha um estilo de vida saudável com exercícios e alimentação balanceada.") :-
    Problemas = nenhum.

% Predicados utilitários
desvio_padrao(Lista, Desvio) :-
    length(Lista, N),
    N > 0,
    sum_list(Lista, Soma),
    Media is Soma / N,
    findall(Diff, (
        member(X, Lista),
        Diff is (X - Media) * (X - Media)
    ), Diffs),
    sum_list(Diffs, SomaQuadrados),
    Variancia is SomaQuadrados / N,
    Desvio is sqrt(Variancia).
