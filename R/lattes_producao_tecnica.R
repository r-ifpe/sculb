#' @export
lattes_producao_tecnica <- function(curriculo,
                                    pontuacao = NULL,
                                    maximo = NULL,
                                    ultimos_anos = 5) {
  ano_inicial <- ano_inicial(ultimos_anos)
  pontuacao_producao_tecnica <- criar_pontuacao_producao_tecnica(pontuacao)

  producao_tecnica <- rbind(
    ler_acessoria_e_consultoria(curriculo),
    ler_produto_tecnologico(curriculo),
    ler_trabalhos_tecnicos(curriculo),
    ler_apresentacao_e_palestras(curriculo)
  ) %>%
    filtrar_orientacoes(ano_inicial)

  score_producao_tecnica(
    producao_tecnica$deferidos,
    pontuacao_producao_tecnica
  )
}

criar_pontuacao_producao_tecnica <- function(pontuacao) {
  if (is.null(pontuacao)) {
    pontuacao_orientacoes <- data.frame(
      item = c(
        "ACESSORIA_E_CONSULTORIA", "PRODUTOS_TECNOLOGICOS",
        "PROCESSOS_E_TECNICAS", "TRABALHOS_TECNICOS",
        "CURSO_DE_CURTA_DURACAO", "APRESENTACAO_DE_TRABALHOS_E_PALESTRAS"
      ),
      pontuacao = c(1, 1, 1, 0.5, 0.5, 0.5),
      quantidade_max = c(5, 5, 5, 5, 5, 10)
    )
  } else {
    pontuacao_orientacoes <- pontuacao
  }

  pontuacao_orientacoes
}

#' @importFrom dplyr %>%
ler_acessoria_e_consultoria <- function(x) {
  x <- xml2::xml_find_all(
    x,
    "//PRODUCAO-TECNICA/TRABALHO-TECNICO/DADOS-BASICOS-DO-TRABALHO-TECNICO"
  )

  if (length(x) == 0) {
    data.frame()
  } else {
    data.frame(
      item = "ACESSORIA_E_CONSULTORIA",
      natureza = xml2::xml_attr(x, "NATUREZA"),
      nome = xml2::xml_attr(x, "TITULO-DO-TRABALHO-TECNICO"),
      ano = xml2::xml_attr(x, "ANO")
    ) %>%
      dplyr::filter(natureza == "CONSULTORIA") %>%
      dplyr::select(item, nome, ano)
  }
}

#' @importFrom dplyr %>%
ler_trabalhos_tecnicos <- function(x) {
  x <- xml2::xml_find_all(
    x,
    "//PRODUCAO-TECNICA/TRABALHO-TECNICO/DADOS-BASICOS-DO-TRABALHO-TECNICO"
  )

  if (length(x) == 0) {
    data.frame()
  } else {
    data.frame(
      item = "TRABALHOS_TECNICOS",
      natureza = xml2::xml_attr(x, "NATUREZA"),
      nome = xml2::xml_attr(x, "TITULO-DO-TRABALHO-TECNICO"),
      ano = xml2::xml_attr(x, "ANO")
    ) %>%
      dplyr::filter(natureza == "PARECER") %>%
      dplyr::select(item, nome, ano)
  }
}

ler_apresentacao_e_palestras <- function(x) {
  x <- xml2::xml_find_all(
    x,
    "//DEMAIS-TIPOS-DE-PRODUCAO-TECNICA/APRESENTACAO-DE-TRABALHO/DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"
  )

  validate_producao_tecnica_data_frame(
    x, "APRESENTACAO_DE_TRABALHOS_E_PALESTRAS", "TITULO", "ANO"
  )
}

ler_produto_tecnologico <- function(x) {
  x <- xml2::xml_find_all(x, "//PRODUTO-TECNOLOGICO/DADOS-BASICOS-DO-PRODUTO-TECNOLOGICO")
  validate_producao_tecnica_data_frame(
    x, "PRODUTOS_TECNOLOGICOS", "TITULO-DO-PRODUTO", "ANO"
  )
}

filtrar_producao_tecnica <- function(x, ano_inicial) {
  list(
    deferidos = dplyr::filter(x, ano >= ano_inicial),
    indeferidos = dplyr::filter(x, ano < ano_inicial)
  )
}

validate_producao_tecnica_data_frame <- function(x, item, name, year) {
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

#' @importFrom dplyr %>%
score_producao_tecnica <- function(deferidos, pontuacao) {
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
