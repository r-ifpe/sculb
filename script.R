library(dplyr)
library(xml2)

# produção tecnica
t <- sculb:::read_lattes_xml("inst/lattes_examples/7532050172035129.zip")
t <- sculb:::read_lattes_xml("inst/lattes_examples/8224658682910588.zip")
t <- sculb:::read_lattes_xml("inst/lattes_examples/4403057502641157.zip")

lattes_atuacao(t)

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

sculb:::read_lattes_xml("inst/lattes_examples/4403057502641157.zip") %>%
  lattes_bibliografia()

t <- openxlsx::read.xlsx("inst/lattes/qualis.xlsx")

t %>%
  arrange(ISSN, Estrato) %>%
  group_by(ISSN, Título) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(ISSN =  stringr::str_remove_all(ISSN, "-")) %>%
  rename(TIULO = Título, ESTRATO = Estrato) %>%
  select(ISSN, TIULO, ESTRATO) %>%
  write.table("inst/lattes/qualis_maior.csv", row.names = FALSE, sep = ",")



