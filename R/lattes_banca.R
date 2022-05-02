#' @export
lattes_banca <- function(curriculo,
                         pontuacao = NULL,
                         maximo = NULL,
                         ultimos_anos = 5) {
  ano_inicial <- ano_inicial(ultimos_anos)
  pontuacao_banca <- criar_pontuacao_banca(pontuacao)

  banca <- rbind(
    ler_banca_stricto_sensu(curriculo),
    ler_banca_lato_sensu(curriculo),
    ler_banca_graduacao(curriculo),
    ler_banca_evento(curriculo)
  ) %>%
    filtrar_bancas(ano_inicial)

  score_banca(
    banca$deferidos,
    pontuacao_banca
  )
}

criar_pontuacao_banca <- function(pontuacao) {
  if (is.null(pontuacao)) {
    pontuacao_banca <- data.frame(
      item = c(
        "BANCA_POS_STRICTO_SENSU", "BANCA_POS_LATO_SENSU",
        "BANCA_GRADUACAO", "BANCA_EVENTO"
      ),
      pontuacao = c(1.5 , 1, 0.5, 0.5),
      quantidade_max = c(10, 10, 10, 10)
    )
  } else {
    pontuacao_banca <- pontuacao
  }

  pontuacao_banca
}

ler_banca_stricto_sensu <- function(x) {
 rbind(
   ler_banca_stricto_sensu_mestrado(x),
   ler_banca_doutorado(x)
 )
}

ler_banca_lato_sensu <- function(x) {
  ler_banca_lato_sensu_mestrado(x)
}

#' @importFrom dplyr %>%
ler_banca_stricto_sensu_mestrado <- function(x) {
  x <- xml2::xml_find_all(x,
    "//PARTICIPACAO-EM-BANCA-DE-MESTRADO/DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-MESTRADO"
  )

  if (length(x) == 0) {
    data.frame()
  } else {
    data.frame(
      item = "BANCA_POS_STRICTO_SENSU",
      natureza = xml2::xml_attr(x, "NATUREZA"),
      tipo = xml2::xml_attr(x, "TIPO"),
      nome = xml2::xml_attr(x, "TITULO"),
      ano = xml2::xml_attr(x, "ANO")
    ) %>%
      dplyr::filter(tipo == "ACADEMICO") %>%
      dplyr::select(item, nome, ano)
  }
}

ler_banca_doutorado <- function(x) {
  banca_doutorado <- xml2::xml_find_all(x,
    "//PARTICIPACAO-EM-BANCA-DE-DOUTORADO/DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-DOUTORADO"
  )

  if (length(banca_doutorado) == 0) {
    doutorados <- data.frame()
  } else {
    doutorados <- data.frame(
      item = "BANCA_POS_STRICTO_SENSU",
      nome = xml2::xml_attr(banca_doutorado, "TITULO"),
      ano = xml2::xml_attr(banca_doutorado, "ANO")
    )
  }

  banca_qualificacao <- xml2::xml_find_all(x,
    "//PARTICIPACAO-EM-BANCA-DE-EXAME-QUALIFICACAO/DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-EXAME-QUALIFICACAO"
  )

  if (length(banca_qualificacao) == 0) {
    qualificacoes <- data.frame()
  } else {
    qualificacoes <-   data.frame(
      item = "BANCA_POS_STRICTO_SENSU",
      nome = xml2::xml_attr(banca_qualificacao, "TITULO"),
      ano = xml2::xml_attr(banca_qualificacao, "ANO")
    )
  }

  rbind(doutorados, qualificacoes)

}

#' @importFrom dplyr %>%
ler_banca_lato_sensu_mestrado <- function(x) {
  x <- xml2::xml_find_all(x,
    "//PARTICIPACAO-EM-BANCA-DE-MESTRADO/DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-MESTRADO"
  )

  if (length(x) == 0) {
    data.frame()
  } else {
    data.frame(
      item = "BANCA_POS_LATO_SENSU",
      natureza = xml2::xml_attr(x, "NATUREZA"),
      tipo = xml2::xml_attr(x, "TIPO"),
      nome = xml2::xml_attr(x, "TITULO"),
      ano = xml2::xml_attr(x, "ANO")
    ) %>%
      dplyr::filter(tipo != "ACADEMICO") %>%
      dplyr::select(item, nome, ano)
  }
}

ler_banca_graduacao <- function(x) {
  x <- xml2::xml_find_all(x,
    "//PARTICIPACAO-EM-BANCA-DE-GRADUACAO/DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-GRADUACAO"
  )

  if (length(x) == 0) {
    data.frame()
  } else {
    data.frame(
      item = "BANCA_GRADUACAO",
      nome = xml2::xml_attr(x, "TITULO"),
      ano = xml2::xml_attr(x, "ANO")
    )
  }
}

ler_banca_evento <- function(x) {
  x <- xml2::xml_find_all(x,
    "//OUTRAS-BANCAS-JULGADORAS/DADOS-BASICOS-DE-OUTRAS-BANCAS-JULGADORAS"
  )

  if (length(x) == 0) {
    data.frame()
  } else {
    data.frame(
      item = "BANCA_EVENTO",
      nome = xml2::xml_attr(x, "TITULO"),
      ano = xml2::xml_attr(x, "ANO")
    )
  }
}



filtrar_bancas <- function(x, ano_inicial) {
  list(
    deferidos = dplyr::filter(x, ano >= ano_inicial),
    indeferidos = dplyr::filter(x, ano < ano_inicial)
  )
}

#' @importFrom dplyr %>%
score_banca <- function(deferidos, pontuacao) {
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
