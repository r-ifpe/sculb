library(dplyr)
library(xml2)

# produção tecnica
t <- sculb:::read_lattes_xml("inst/lattes_examples/7532050172035129.zip")
t <- sculb:::read_lattes_xml("inst/lattes_examples/8224658682910588.zip")

score_detalhado <- rbind(
  lattes_titulacao(t),
  lattes_bibliografia(t),
  lattes_orientacoes(t),
  lattes_producao_tecnica(t)
)

score <- data.frame(
  nome = lattes_nome(t),
  pontuacao_total = sum(score_detalhado$total)
)





