library(shiny)

utils::data("example_data", package = "assign4nspack", envir = environment())

ui <- fluidPage(
  titlePanel("Nitrate Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Select site:", choices = unique(example_data$site))
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  data_filtered <- reactive(subset(example_data, site == input$site))

  output$plot <- renderPlot({
    d <- data_filtered()
    plot(d$time, d$nitrate, type = "l",
         xlab = "Date", ylab = "Nitrate (mg/L)")
  })

  output$summary <- renderTable({
    d <- data_filtered()
    data.frame(
      n = nrow(d),
      mean = mean(d$nitrate, na.rm = TRUE),
      sd = sd(d$nitrate, na.rm = TRUE)
    )
  })
}

shinyApp(ui, server)
