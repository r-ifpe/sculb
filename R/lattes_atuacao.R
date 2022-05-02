#' @export
lattes_atuacao <- function(curriculo,
                           pontuacao = NULL,
                           maximo = NULL,
                           ultimos_anos = 5) {
  ano_inicial <- ano_inicial(ultimos_anos)
  atuacao <- criar_pontuacao_atuacao(pontuacao)

  browser()
  producao_atuacao <- rbind(
    ler_membro_comite(curriculo)
  ) %>%
    filtrar_deferidos_indeferidos(ano_inicial)

  calcular_score(
    producao_atuacao$deferidos,
    pontuacao_producao_tecnica
  )
}

criar_pontuacao_atuacao<- function(pontuacao) {
  if (is.null(pontuacao)) {
    pontuacao_atuacao <- data.frame(
      item = c(
        "MEMBRO_COMITE_ASSESSOR", "REVISOR_PERIODICO",
        "MEMBRO_CORPO_EDITORIAL", "REVISOR_PROJETO"
      ),
      pontuacao = c(0.5, 1, 0.5, 1),
      quantidade_max = c(3, 3, 3, 3)
    )
  } else {
    pontuacao_atuacao <- pontuacao
  }

  pontuacao_atuacao
}

#' @importFrom dplyr %>%
ler_membro_comite <- function(x){
  browser()
  if (length(x) == 0) {
    data.frame()
  } else {
    data.frame(
      item = "MEMBRO_COMITE_ASSESSOR",
      tipo = extrair_xml_attr_curriculo(x,
        "//ATUACAO-PROFISSIONAL/VINCULOS",
        "OUTRO-VINCULO-INFORMADO"
      ),
      nome = extrair_xml_attr_curriculo(x,
        "//ATUACAO-PROFISSIONAL",
        "NOME-INSTITUICAO"
      ),
      ano = extrair_xml_attr_curriculo(x,
        "//ATUACAO-PROFISSIONAL/VINCULOS",
        "ANO-FIM"
      )) %>%
        dplyr::filter(tipo == "Membro de comitê assessor") %>%
        dplyr::select(item, nome, ano)
  }

}



criar_lattes_data_frame <- function(x, item, name, year) {
  if (length(x) == 0) {
    data.frame()
  } else {
    data.frame(
      item = item,
      nome = xml2::xml_attr(x, name),
      ano = xml2::xml_attr(x, year)
    )
  }
}





