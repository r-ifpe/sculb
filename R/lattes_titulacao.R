#' @importFrom dplyr %>%
#' @export
lattes_titulacao <- function(x, pontuacao = NULL) {
  if (is.null(pontuacao)) {
    titulacao <- data.frame(
      item = c("GRADUACAO", "ESPECIALIZACAO", "MESTRADO", "DOUTORADO"),
      pontuacao = c(2, 4, 7, 10)
    )
  } else {
    titulacao <- pontuacao
  }

  titulacao$quantidade <- 1
  titulacao$quantidade_max <- 1

  titulacao_existe <- c(
    node_existe(x, "//GRADUACAO"),
    node_existe(x, "//ESPECIALIZACAO"),
    node_existe(x, "//MESTRADO"),
    node_existe(x, "//DOUTORADO")
  )

  titulacao[titulacao_existe, ] %>%
    dplyr::mutate(total = pontuacao * quantidade) %>%
    dplyr::select(item, quantidade, quantidade_max, pontuacao, total)
}
