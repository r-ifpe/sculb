ano_inicial <- function(ultimos_anos) {
  as.numeric(substr(Sys.Date(), 1, 4)) - ultimos_anos
}

#' @export
read_qualis <- function() {
  utils::read.csv(system.file(
    "lattes/qualis_2010_2016.csv", package = "sculb"
  ))
}

#' @export
read_sjr <- function() {
  utils::read.csv(system.file(
    "lattes/scimagojr_1999_2019.csv", package = "sculb"
  )) %>%
    dplyr::mutate(
      SJR = as.numeric(sub(",", ".", SJR)),
      ISSN = stringr::str_trim(ISSN)
  )
}

#' @export
read_lattes_xml <- function(x) {
  xml2::read_xml(x)
}

node_existe <- function(x, node) {
  !is.na(xml2::xml_find_first(x, node))
}

extrair_xml_dados_basicos_e_detalhes <- function(x, dados_basicos, detalhes) {
  list(
    dados_basicos = xml2::xml_find_all(x, dados_basicos),
    detalhes = xml2::xml_find_all(x, detalhes)
  )
}

ler_orientacao <- function(x) {
  titulo <- verificar_atributo_titulo(x$dados_basicos)

  data.frame(
    titulo = xml2::xml_attr(x$dados_basicos, titulo),
    ano = xml2::xml_attr(x$dados_basicos, "ANO"),
    orientacao = xml2::xml_attr(x$detalhes, "TIPO-DE-ORIENTACAO")
  ) %>%
    dplyr::mutate(orientacao = dplyr::if_else(
      orientacao == "ORIENTADOR_PRINCIPAL", "ORIENTADOR", "COORIENTADOR"
  ))
}

atributo_existe <- function(x, attr) {
  any(xml2::xml_has_attr(x, attr))
}

verificar_atributo_titulo <- function(x) {
  if (atributo_existe(x, "TITULO")) {
    "TITULO"
  } else {
    "TITULO-DO-TRABALHO"
  }
}

filtrar_deferidos_indeferidos <- function(x, ano_inicial) {
  list(
    deferidos = dplyr::filter(x, ano >= ano_inicial),
    indeferidos = dplyr::filter(x, ano < ano_inicial)
  )
}

#' @importFrom dplyr %>%
calcular_score <- function(deferidos, pontuacao) {
  deferidos %>%
    dplyr::group_by(item) %>%
    dplyr::tally(name = "quantidade") %>%
    dplyr::left_join(pontuacao, by = "item") %>%
    dplyr::mutate(total = dplyr::if_else(
      quantidade > quantidade_max, quantidade_max * pontuacao, quantidade * pontuacao
    )) %>%
    dplyr::select(
      item, quantidade, quantidade_max, pontuacao, total
    )
}

#' @importFrom dplyr %>%
extrair_xml_attr_curriculo <- function(x, area, atributo) {
  xml2::xml_find_all(x, area) %>%
    xml2::xml_attr(atributo)
}
