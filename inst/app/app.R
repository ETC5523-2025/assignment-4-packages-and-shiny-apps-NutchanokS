library(shiny)
library(ggplot2)
library(dplyr)

# Load data from package
data("wq_hourly", package = "assign4nspack")
data("nitrate_hourly", package = "assign4nspack")
data("wq_nitrate", package = "assign4nspack")

ui <- fluidPage(
  titlePanel("NEON Data: Freshwater Quality Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose Dataset:",
                  choices = c("Water Quality" = "wq_hourly",
                              "Nitrate" = "nitrate_hourly",
                              "Combined" = "wq_nitrate")),
      selectInput("site", "Choose Site:", choices = unique(wq_hourly$site)),
      selectInput("sensor", "Sensor Location:", choices = unique(wq_hourly$sensor)),
      selectInput("variable", "Select Variable:",
                  choices = c("Conductivity (µS/cm)" = "cond_uScm",
                              "Dissolved Oxygen (mg/L)" = "do_mgL",
                              "Turbidity (FNU)" = "turb_FNU",
                              "Nitrate (µmol/L)" = "nitrate_umolL")),
      hr(),
      helpText("Use this app to explore freshwater sensor data from NEON sites.",
               "Note: Nitrate sensors record less frequently, so missing periods are normal.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Time Trend", plotOutput("timePlot")),
        tabPanel("Boxplot by Site", plotOutput("boxPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  reactive_data <- reactive({
    df <- switch(input$dataset,
                 "wq_hourly" = wq_hourly,
                 "nitrate_hourly" = nitrate_hourly,
                 "wq_nitrate" = wq_nitrate)
    df %>%
      filter(site == input$site, sensor == input$sensor)
  })

  output$timePlot <- renderPlot({
    df <- reactive_data()
    ggplot(df, aes(date_time, .data[[input$variable]], color = site)) +
      geom_line(na.rm = TRUE) +
      labs(x = "Date", y = input$variable, title = paste("Time Trend for", input$variable)) +
      theme_minimal()
  })

  output$boxPlot <- renderPlot({
    df <- reactive_data()
    ggplot(df, aes(x = site, y = .data[[input$variable]], fill = site)) +
      geom_boxplot(na.rm = TRUE) +
      labs(y = input$variable, title = paste("Distribution of", input$variable, "by Site")) +
      theme_minimal()
  })
}

shinyApp(ui, server)
