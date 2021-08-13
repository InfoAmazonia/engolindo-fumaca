---
output:
  pdf_document: default
  word_document: default
  html_document: default
---
# Resultados da análise

## Contexto

O modelo estatístico utilizado busca explicar o número de internações por 
município a partir de variáveis como a área desmatada por mês, a permanência de
altos índices de poluição e o tamanho do município (população). A partir disso, 
obtemos os efeitos de cada variável no número total de internações por Covid e 
por SRAG.

## Variável de interesse: poluição

A principal variável em que há interesse é a variável relacionada com a 
poluição. Neste caso, foi considerada tanto a concentração média mensal de 
material particulado 2.5 (pm25) quanto a quantidade de dias em cada mês 
(dias_acima_25) em que a média diária ficou acima de 25 microgramas por m³, ou 
seja, a _continuidade_ da exposição a um nível alto de poluição.

No caso da concentração média (pm25), a variável não é tem um peso tão grande no
modelo, ou seja, o efeito da variação da concentração média no total de 
internações é menos importante que, por exemplo, o total de dias acima de 25.

No caso do total de dias acima de 25 (dias_acima_25), há um efeito _positivo_ no
total de internações, ou seja, _quanto mais dias, mais internações_. Fixando 
todas as outras variáveis (ou seja, considerando que não há mudança 
no nível de desmatamento, precipitação, casos de Covid, etc), o aumento de 
**1 dia** por mês com PM acima de 25 microgramas/m³ implica em um aumento de 
cerca de **+2,0%** no total de internações (em relação a uma situação hipotética
considerando o mesmo município e condições iguais de precipitação, desmatamento,
etc). Para o modelo considerando os casos de SRAG de forma geral (e não apenas 
Covid), esse incremento (nas internações por SRAG) é de **+2,6%**. 

No [dashboard](https://infoamazonia.shinyapps.io/engolindo-fumaca/),
o percentual indicado é sempre em relação a um mês hipotético no mesmo município
em que houve 0 dias acima de 25.


## Agrupamento dos municípios

Para identificar que municípios possuem características similares entre si 
considerando queimadas, desmatamento, precipitação, poluição e população, foi
feito um agrupamento utilizando um algoritmo de classificação iterativo para 5 
grupos. Nesse processo, os municípios são agrupados levando em consideração quão
similares ou dissimilares são entre si; municípios com perfis parecidos são 
classificados em um mesmo grupo, enquanto municípios menos parecidos são 
classificados em grupos distintos.

Os dados para o agrupamento consideraram apenas o período de queimadas, de julho
a outubro, já que o intuito era identificar perfis semelhantes de municípios 
justamente nesse período e, assim, buscar quais são os municípios mais 
vulneráveis.

Os 10 municípios indicados no dashboard como "municípios vulneráveis" integram o
grupo com maior média de pm25 e dias_acima_25, precipitação média a baixa, maior
média de área desmatada e quantidade de focos de calor e população média (sempre
em comparação com os outros grupos).

