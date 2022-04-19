library(dplyr)
library(xml2)

# produção tecnica
t <- sculb:::read_lattes_xml("inst/lattes_examples/7532050172035129.zip")
t <- sculb:::read_lattes_xml("inst/lattes_examples/8224658682910588.zip")

lattes_banca(t)

e <- "inst/lattes_examples/8710150668674818.zip"

score_detalhado <- rbind(
  lattes_titulacao(read_lattes_xml(e)),
  lattes_bibliografia(read_lattes_xml(e)),
  lattes_orientacoes(read_lattes_xml(e)),
  lattes_producao_tecnica(read_lattes_xml(e)),
  lattes_banca(read_lattes_xml(e))
)

score <- data.frame(
  nome = lattes_nome(read_lattes_xml(e)),
  pontuacao_total = sum(score_detalhado$total, na.rm = TRUE)
)

list(
  nome = score$nome,
  score = score,
  score_detalhado = score_detalhado
)


