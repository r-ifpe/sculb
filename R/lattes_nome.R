#' @export
lattes_nome <- function(x) {
  xml2::xml_find_all(x, "//DADOS-GERAIS") %>%
    xml2::xml_attr("NOME-COMPLETO")
}
