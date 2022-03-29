#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ifpe.apps)
library(xml2)

ui <- fluidPage(
    shiny::navbarPage(
        "App Lattes",
        tabPanel("Pontuação",
                 sidebarLayout(
                     sidebarPanel(
                         shiny::fileInput(
                             "lattes", "Escolha os curriculos lattes",
                             multiple = TRUE,
                             buttonLabel = "Arquivos",
                             placeholder = "Nada Selecionado",
                             accept = c("zip")
                         )
                     ),
                     mainPanel(textOutput("lattes_output"))

                 )
                 )

    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

   output$lattes_output <- renderText({
       if (is.null(input$lattes)) {
           "Por favor insira os curriculos lattes"
       } else {
           output_folder <- choose.dir()
           t <- lapply(
               input$lattes$datapath,
               function(e) {

                   score_detalhado <- rbind(
                       lattes_titulacao(read_lattes_xml(e)),
                       lattes_bibliografia(read_lattes_xml(e)),
                       lattes_orientacoes(read_lattes_xml(e))
                   )

                   score <- data.frame(
                       nome = lattes_nome(read_lattes_xml(e)),
                       pontuacao_total = sum(score_detalhado$total)
                   )

                   list(
                       nome = score$nome,
                       score = score,
                       score_detalhado = score_detalhado
                   )
               }
           )

           # escrever pontuacao dos professores
           do.call("rbind",
                   lapply(1:length(t), function(e) {
                       t[[e]]$score
                   })
           ) %>%
               dplyr::arrange(desc(pontuacao_total)) %>%
               openxlsx::write.xlsx(paste0(
                 output_folder, "/pontuacao_total.xlsx"
               )
                   )

           lapply(1:length(t), function(e) {
               openxlsx::write.xlsx(
                   t[[e]]$score_detalhado,
                   paste0(output_folder,  "/",
                          t[[e]]$nome, ".xlsx")
               )
           })
       }


     # "download realizado com sucesso!!!"
   })


}

# Run the application
shinyApp(ui = ui, server = server)
