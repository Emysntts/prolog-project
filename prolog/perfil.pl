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

% Predicados para recomendações personalizadas
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
