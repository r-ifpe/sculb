#' @export
sculb_ui <- function() {
  shiny::fluidPage(
    shiny::navbarPage("Sculb", sculb_panel())
  )
}

sculb_panel <- function() {
  shiny::tabPanel(
    "Pontuação",
    shiny::sidebarLayout(
      shiny::sidebarPanel(sculb_lattes_input()),
      shiny::mainPanel(shiny::textOutput("lattes_output"))
    )
  )
}

sculb_lattes_input <- function() {
  shiny::fileInput(
    "lattes", "Escolha os curriculos lattes",
    multiple = TRUE,
    buttonLabel = "Arquivos",
    placeholder = "Nada Selecionado",
    accept = c("zip")
  )
}



