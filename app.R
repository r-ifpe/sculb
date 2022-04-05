library(sculb)

shiny::shinyApp(
  ui = sculb_ui(),
  server = sculb_server()
)
