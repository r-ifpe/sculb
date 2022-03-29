#' @export
lattes_nome <- function(x) {
  xml_find_all(x, "//DADOS-GERAIS") %>%
    xml_attr("NOME-COMPLETO")
}
