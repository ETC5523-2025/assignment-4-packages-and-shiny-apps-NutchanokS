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
                  choices = c("Conductivity (uS/cm)" = "cond_uScm",
                              "Dissolved Oxygen (mg/L)" = "do_mgL",
                              "Turbidity (FNU)" = "turb_FNU",
                              "Nitrate (umol/L)" = "nitrate_umolL")),
      hr(),
      helpText(
        "Use this app to explore freshwater sensor data from NEON sites.",
        "Note: Nitrate sensors record less frequently, so missing periods are normal."
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About the Data",
          h4("About the Dataset"),
          p("This app explores freshwater quality data collected by the National Ecological Observatory Network (NEON) at multiple stream sites."),
          p("Each site includes two water-quality sensors: S1_upstream and S2_downstream."),
          p("Nitrate data come from a separate instrument that records less frequently, resulting in occasional missing values."),
          h4("How to Use the App"),
          tags$ul(
            tags$li("Choose the dataset (Water Quality, Nitrate, or Combined)."),
            tags$li("Select a site and sensor to view time trends or compare distributions."),
            tags$li("The 'Time Trend' tab shows changes over time; the 'Boxplot' tab compares overall distributions.")
          ),
          h4("How to Interpret"),
          p("Higher dissolved oxygen generally indicates better water quality. Higher turbidity or nitrate may indicate runoff or pollution."),
          h4("Data Source"),
          p("NEON products DP1.20288.001 (Water Quality) and DP1.20033.001 (Nitrate), processed with scripts in data-raw/.")
        ),
        tabPanel("Time Trend", plotOutput("timePlot")),
        tabPanel("Boxplot by Site", plotOutput("boxPlot"))
      )
    )
  ) # closes sidebarLayout
)   # closes fluidPage

server <- function(input, output, session) {
  reactive_data <- reactive({
    df <- switch(input$dataset,
                 "wq_hourly" = wq_hourly,
                 "nitrate_hourly" = nitrate_hourly,
                 "wq_nitrate" = wq_nitrate)
    df %>% filter(site == input$site, sensor == input$sensor)
  })

  output$timePlot <- renderPlot({
    df <- reactive_data()
    ggplot(df, aes(date_time, .data[[input$variable]], color = site)) +
      geom_line(na.rm = TRUE) +
      labs(x = "Date", y = input$variable,
           title = paste("Time Trend for", input$variable)) +
      theme_minimal()
  })

  output$boxPlot <- renderPlot({
    df <- reactive_data()
    ggplot(df, aes(x = site, y = .data[[input$variable]], fill = site)) +
      geom_boxplot(na.rm = TRUE) +
      labs(y = input$variable,
           title = paste("Distribution of", input$variable, "by Site")) +
      theme_minimal()
  })
}

shinyApp(ui, server)
