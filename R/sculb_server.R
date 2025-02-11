#' @export
sculb_server <- function() {

function(input, output, session) {
    output$lattes_output <- renderText({
      if (is.null(input$lattes)) {
        "Por favor insira os curriculos lattes"
      } else {
        output_folder <- utils::choose.dir()
        t <- lapply(
          input$lattes$datapath,
          function(e) {
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
          }
        )

        # escrever pontuacao dos professores
        do.call(
          "rbind",
          lapply(1:length(t), function(e) {
            t[[e]]$score
          })
        ) %>%
          dplyr::arrange(desc(pontuacao_total)) %>%
          openxlsx::write.xlsx(paste0(
            output_folder, "/pontuacao_total.xlsx"
          ))

        lapply(1:length(t), function(e) {
          openxlsx::write.xlsx(
            t[[e]]$score_detalhado,
            paste0(
              output_folder, "/",
              t[[e]]$nome, ".xlsx"
            )
          )
        })

        "Currículos avaliados com sucesso!!!"
      }
    })
  }
}
