% Definindo fatos
gosta_de(joao, pizza).
gosta_de(maria, salada).
gosta_de(ana, pizza).

% Definindo uma regra
gosta_de_comida(X, Y) :- gosta_de(X, Y).
