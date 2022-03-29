#' @export
lattes_bibliografia <- function(curriculo,
                                pontuacao = NULL,
                                maximo = NULL,
                                ultimos_anos = 5) {

  ano_inicial <- ano_inicial(ultimos_anos)
  pontuacao_bibliografia <- criar_pontuacao_bibliografia(pontuacao)

  if (is.null(maximo)) {
    maximo <- list(
      congresso_trabalho_completo = 10,
      congresso_resumo = 10
    )
  }

  periodicos <- ler_periodicos(curriculo) %>%
    filtrar_periodicos(ano_inicial)

  congressos <- ler_congressos(curriculo) %>%
    filtrar_congressos(ano_inicial)

  livros <- ler_livro(curriculo) %>%
    filtrar_livros(ano_inicial)

  score_bibliografia(periodicos, congressos, livros, pontuacao_bibliografia)
}

criar_pontuacao_bibliografia <- function(pontuacao) {
  if(is.null(pontuacao)) {
    pontuacao_bibliografia <- data.frame(
      item = c(
        "A1", "A2", "B1", "B2", "B3", "B4", "B5", "C", "SEM_QUALIS",
        "LIVRO_PUBLICADO", "LIVRO_ORGANIZADO", "LIVRO_CAPITULO",
        "CONGRESSO_TRABALHO_COMPLETO", "CONGRESSO_RESUMO"
      ),
      pontuacao = c(
        5, 5, 4, 4, 4, 2, 2, 0.5, 0.5,
        5, 4, 2.5,
        1.5, 0.5
      )
  )
  } else {
    pontuacao_bibliografia <- pontuacao
  }

  pontuacao_bibliografia
}

ler_congressos <- function(x) {
  xpath <- "//TRABALHOS-EM-EVENTOS/TRABALHO-EM-EVENTOS/DADOS-BASICOS-DO-TRABALHO"
  eventos <- xml2::xml_find_all(x, xpath)

  data.frame(
    titulo = xml2::xml_attr(eventos, "TITULO-DO-TRABALHO"),
    natureza = xml2::xml_attr(eventos, "NATUREZA"),
    ano = xml2::xml_attr(eventos, "ANO-DO-TRABALHO")
  ) %>%
    dplyr::mutate(natureza = dplyr::if_else(
      natureza == "COMPLETO", "CONGRESSO_TRABALHO_COMPLETO", "CONGRESSO_RESUMO"
    ))
}

filtrar_congressos <- function(x, ano_inicial) {
  congresso_deferido <- dplyr::filter(x, ano >= ano_inicial)
  congresso_indeferido <- dplyr::filter(x, ano < ano_inicial) %>%
    dplyr::mutate(motivo = "Ano: fora do periodo")

  list(
    deferidos = congresso_deferido,
    indeferidos = congresso_indeferido
  )
}

ler_periodicos <- function(x) {
  xpath <- "//ARTIGO-PUBLICADO/DADOS-BASICOS-DO-ARTIGO"
  periodicos <- xml2::xml_find_all(x, xpath)
  xpath <- "//ARTIGO-PUBLICADO/DETALHAMENTO-DO-ARTIGO"
  periodicos_detalhes <- xml2::xml_find_all(x, xpath)

  periodicos_tabela <- data.frame(
    titulo = xml2::xml_attr(periodicos, "TITULO-DO-ARTIGO"),
    natureza = xml2::xml_attr(periodicos, "NATUREZA"),
    ano = xml2::xml_attr(periodicos, "ANO-DO-ARTIGO"),
    issn = xml2::xml_attr(periodicos_detalhes, "ISSN")
  )

 classificar_qualis(periodicos_tabela)
}

classificar_qualis <- function(periodicos) {
  qualis <- read_qualis()
  sjr <- read_sjr()
  dplyr::left_join(periodicos, qualis, by = c("issn" = "ISSN")) %>%
    dplyr::left_join(sjr, by = c("issn" = "ISSN")) %>%
    dplyr::mutate(sjr = dplyr::if_else(SJR >= 2.5, "A1", "B1")) %>%
    dplyr::mutate(
      estrato = dplyr::if_else(is.na(ESTRATO), sjr, ESTRATO)
    ) %>%
    dplyr::mutate(
      estrato = dplyr::if_else(is.na(estrato), "SEM_QUALIS", estrato)
    ) %>%
    dplyr::select(titulo, natureza, ano, issn, estrato)
}

filtrar_periodicos <- function(x, ano_inicial) {
  periodico_deferido <- dplyr::filter(x, ano >= ano_inicial)
  periodico_indeferido <- dplyr::filter(x, ano < ano_inicial) %>%
    dplyr::mutate(motivo = "Ano: fora do periodo")

  list(
    deferidos = periodico_deferido,
    indeferidos = periodico_indeferido
  )
}

ler_livro <- function(x) {
  xpath <- "//LIVRO-PUBLICADO-OU-ORGANIZADO/DADOS-BASICOS-DO-LIVRO"
  livro <- xml2::xml_find_all(x, xpath)

  livro <- data.frame(
    titulo = xml2::xml_attr(livro, "TITULO-DO-LIVRO"),
    natureza = xml2::xml_attr(livro, "TIPO"),
    ano = xml2::xml_attr(livro, "ANO")
  ) %>%
  dplyr::mutate(natureza = dplyr::if_else(
      natureza == "LIVRO_ORGANIZADO_OU_EDICAO", "LIVRO_ORGANIZADO", natureza))

  xpath <- "//CAPITULO-DE-LIVRO-PUBLICADO/DADOS-BASICOS-DO-CAPITULO"
  capitulo <- xml2::xml_find_all(x, xpath)
  xpath <- "//CAPITULO-DE-LIVRO-PUBLICADO/DETALHAMENTO-DO-CAPITULO"
  capitulo_detalhes <- xml2::xml_find_all(x, xpath)

  capitulo <- data.frame(
    titulo = xml2::xml_attr(capitulo_detalhes, "TITULO-DO-LIVRO"),
    natureza = xml2::xml_attr(capitulo, "TIPO"),
    ano = xml2::xml_attr(capitulo, "ANO")
  ) %>%
  dplyr::mutate(natureza = "LIVRO_CAPITULO")

  rbind(livro, capitulo)
}

filtrar_livros <- function(x, ano_inicial) {

  livro_deferido <- dplyr::filter(x, ano >= ano_inicial)
  livro_indeferido <- dplyr::filter(x, ano < ano_inicial)
  capitulo_deferido <- dplyr::filter(x, ano >= ano_inicial)
  capitulo_indeferido <- dplyr::filter(x, ano < ano_inicial)

  list(
    deferidos = rbind(livro_deferido, capitulo_deferido),
    indeferidos = rbind(livro_indeferido, capitulo_indeferido)
  )
}

score_bibliografia <- function(periodicos, congressos,
                               livros, pontuacao_bibliografia) {

  periodicos_deferidos <- periodicos$deferidos %>%
    dplyr::group_by(estrato) %>%
    dplyr::tally(name = "quantidade") %>%
    dplyr::mutate(quantidade_max = "-") %>%
    dplyr::left_join(pontuacao_bibliografia, by = c("estrato" = "item")) %>%
    dplyr::rename(item = estrato) %>%
    dplyr::mutate(item = paste0("PERIODICO_", item),
                  total = quantidade * pontuacao)

  congressos_deferidos <- congressos$deferidos %>%
    dplyr::group_by(natureza) %>%
    dplyr::tally(name = "quantidade") %>%
    dplyr::mutate(quantidade_max = 10) %>%
    dplyr::left_join(pontuacao_bibliografia, by = c("natureza" = "item")) %>%
    dplyr::rename(item = natureza) %>%
    dplyr::mutate(total = dplyr::if_else(
      quantidade > quantidade_max,
      quantidade_max * pontuacao, quantidade * pontuacao
    ))

  livros_deferidos <- livros$deferidos %>%
    dplyr::group_by(natureza) %>%
    dplyr::tally(name = "quantidade") %>%
    dplyr::mutate(quantidade_max = "-") %>%
    dplyr::left_join(pontuacao_bibliografia, by = c("natureza" = "item")) %>%
    dplyr::rename(item = natureza) %>%
    dplyr::mutate(total = quantidade * pontuacao)

  rbind(
    periodicos_deferidos,
    congressos_deferidos,
    livros_deferidos
  )
}

