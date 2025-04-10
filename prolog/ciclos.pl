% Arquivo: ciclos.pl

% Fato que armazena os ciclos menstruais
% ciclo(Usuario, DataInicio, DuracaoCiclo, DuracaoMenstruacao)
ciclo(maria, date(2025,4,1), 28, 5).
ciclo(joana, date(2025,4,5), 30, 4).

% Regra para adicionar novos ciclos
adicionar_ciclo(Usuario, DataInicio, DuracaoCiclo, DuracaoMenstruacao) :-
    assert(ciclo(Usuario, DataInicio, DuracaoCiclo, DuracaoMenstruacao)).
