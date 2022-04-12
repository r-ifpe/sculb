#' @export
lattes_orientacoes <- function(curriculo,
                               pontuacao = NULL,
                               maximo = NULL,
                               ultimos_anos = 5) {

  ano_inicial <- ano_inicial(ultimos_anos)
  pontuacao_orientacoes <- criar_pontuacao_orientacoes(pontuacao)

  orientacoes <- rbind(
    ler_orientacao_doutorado(curriculo),
    ler_orientacao_mestrado(curriculo),
    ler_outras_orientacoes(curriculo)
  ) %>%
    filtrar_orientacoes(ano_inicial)

    score_orientacoes(orientacoes$deferidos, pontuacao_orientacoes)
}

ler_orientacao_doutorado <- function(x) {

  doutorado_completo <- extrair_xml_dados_basicos_e_detalhes(x,
    "//ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO/DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO",
    "//ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO/DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO"
  ) %>%
    ler_orientacao() %>%
    dplyr::mutate(natureza = paste0("DOUTORADO_COMPLETO_", orientacao))

  doutorado_em_andamento <- extrair_xml_dados_basicos_e_detalhes(x,
    "//ORIENTACAO-EM-ANDAMENTO-DE-DOUTORADO/DADOS-BASICOS-DA-ORIENTACAO-EM-ANDAMENTO-DE-DOUTORADO",
    "//ORIENTACAO-EM-ANDAMENTO-DE-DOUTORADO/DETALHAMENTO-DA-ORIENTACAO-EM-ANDAMENTO-DE-DOUTORADO"
  ) %>%
    ler_orientacao() %>%
    dplyr::mutate(natureza = paste0("DOUTORADO_EM_ANDAMENTO_", orientacao))

  rbind(doutorado_completo, doutorado_em_andamento)
}

ler_orientacao_mestrado <- function(x) {

  mestrado_completo <- extrair_xml_dados_basicos_e_detalhes(x,
    "//ORIENTACOES-CONCLUIDAS-PARA-MESTRADO/DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO",
    "//ORIENTACOES-CONCLUIDAS-PARA-MESTRADO/DETALHAMENTO-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO"
  ) %>%
    ler_orientacao() %>%
    dplyr::mutate(natureza = paste0("MESTRADO_COMPLETO_", orientacao))

  mestrado_em_andamento <- extrair_xml_dados_basicos_e_detalhes(x,
    "//ORIENTACAO-EM-ANDAMENTO-DE-MESTRADO/DADOS-BASICOS-DA-ORIENTACAO-EM-ANDAMENTO-DE-MESTRADO",
    "//ORIENTACAO-EM-ANDAMENTO-DE-MESTRADO/DETALHAMENTO-DA-ORIENTACAO-EM-ANDAMENTO-DE-MESTRADO"
  ) %>%
    ler_orientacao() %>%
    dplyr::mutate(natureza = paste0("MESTRADO_EM_ANDAMENTO_", orientacao))

  rbind(mestrado_completo, mestrado_em_andamento)
}


ler_outras_orientacoes <- function(x) {
  orientacoes_xml <-xml2::xml_find_all(x,
    "//OUTRAS-ORIENTACOES-CONCLUIDAS/DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS"
  )

  data.frame(
    titulo = xml2::xml_attr(orientacoes_xml, "TITULO"),
    ano = xml2::xml_attr(orientacoes_xml, "ANO"),
    natureza = xml2::xml_attr(orientacoes_xml, "NATUREZA")
  ) %>%
    dplyr::mutate(
      natureza = dplyr::if_else(natureza == "INICIACAO_CIENTIFICA",
        "INICIACAO_CIENTIFICA_COMPLETA", natureza),
      orientacao = "ORIENTADOR" # existe um bug no lattes todo mundo fica como co-orientador
      ) %>%
    dplyr::mutate(natureza = paste0(
      natureza, "_", orientacao
    ))
}

criar_pontuacao_orientacoes <- function(pontuacao) {
  if(is.null(pontuacao)) {
    pontuacao_orientacoes <- data.frame(
      item = c(
        "DOUTORADO_COMPLETO_ORIENTADOR", "DOUTORADO_EM_ANDAMENTO_ORIENTADOR",
        "DOUTORADO_COMPLETO_COORIENTADOR", "DOUTORADO_EM_ANDAMENTO_COORIENTADOR",
        "MESTRADO_COMPLETO_ORIENTADOR", "MESTRADO_EM_ANDAMENTO_ORIENTADOR",
        "MESTRADO_COMPLETO_COORIENTADOR", "MESTRADO_EM_ANDAMENTO_COORIENTADOR",
        "MONOGRAFIA_DE_CONCLUSAO_DE_CURSO_APERFEICOAMENTO_E_ESPECIALIZACAO_ORIENTADOR",
        "TRABALHO_DE_CONCLUSAO_DE_CURSO_GRADUACAO_ORIENTADOR",
        "INICIACAO_CIENTIFICA_COMPLETA_ORIENTADOR", "INICIACAO_CIENTIFICA_EM_ANDAMENTO_ORIENTADOR",
        "OUTRAS_ORIENTACOES"
      ),
      pontuacao = c(
        5, 3, 2.5, 1.5,
        3, 2, 1, 0.5,
        2, 1,
        2.5, 1, 0.5
      )
    )
  } else {
    pontuacao_orientacoes <- pontuacao
  }

  pontuacao_orientacoes
}

filtrar_orientacoes <- function(x, ano_inicial) {
  list(
    deferidos = dplyr::filter(x, ano >= ano_inicial),
    indeferidos = dplyr::filter(x, ano < ano_inicial)
  )
}

score_orientacoes <- function(x, pontuacao_orientacoes) {
  x %>%
    dplyr::mutate(natureza = dplyr::if_else(
      natureza %in% pontuacao_orientacoes$item,
      natureza, "OUTRAS_ORIENTACOES"
    )) %>%
    dplyr::group_by(natureza) %>%
    dplyr::tally(name = "quantidade") %>%
    dplyr::mutate(quantidade_max = 10) %>%
    dplyr::left_join(pontuacao_orientacoes, by = c("natureza" = "item")) %>%
    dplyr::rename(item = natureza) %>%
    dplyr::mutate(total = dplyr::if_else(
      quantidade > quantidade_max,
      quantidade_max * pontuacao, quantidade * pontuacao
    ))
}
