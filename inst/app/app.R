# inst/app/app.R
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)

# Load packaged data
data("wq_hourly",      package = "assign4nspack")
data("nitrate_hourly", package = "assign4nspack")
data("wq_nitrate",     package = "assign4nspack")

# Pretty labels (use Unicode escapes for µ)
var_labels <- c(
  cond_uScm     = "Conductivity (\u00B5S/cm)",
  do_mgL        = "Dissolved oxygen (mg/L)",
  turb_FNU      = "Turbidity (FNU)",
  nitrate_umolL = "Nitrate (\u00B5mol/L)"
)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("NEON Freshwater Quality Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose Dataset:",
                  choices = c("Water Quality" = "wq_hourly",
                              "Nitrate"       = "nitrate_hourly",
                              "Combined"      = "wq_nitrate"),
                  selected = "wq_hourly"),
      selectInput("site", "Choose Site:",
                  choices = c("All sites", sort(unique(wq_hourly$site))),
                  selected = "All sites"),
      selectInput("sensor", "Sensor Location:",
                  choices = sort(unique(wq_hourly$sensor))),
      selectInput("variable", "Select Variable:",
                  choices = c("Conductivity (\u00B5S/cm)" = "cond_uScm",
                              "Dissolved Oxygen (mg/L)"   = "do_mgL",
                              "Turbidity (FNU)"           = "turb_FNU",
                              "Nitrate (\u00B5mol/L)"     = "nitrate_umolL"),
                  selected = "cond_uScm"),
      hr(),
      helpText(
        "Use this app to explore freshwater sensor data from NEON sites.",
        "Note: nitrate is often absent at upstream sensors; this is expected."
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "About the Data",
          h4("About the Dataset"),
          p("High-frequency water-quality (DP1.20288.001) and nitrate (DP1.20033.001) data from NEON streams, processed and packaged in this R package."),
          p("Each site typically includes two water-quality sensors: S1_upstream and S2_downstream. Nitrate data come from a separate instrument and can be sparser."),
          h4("How to Use the App"),
          tags$ul(
            tags$li("Choose dataset, site (or 'All sites'), sensor, and variable."),
            tags$li("See changes over time (Time Trend), compare sites (Boxplot), or inspect nitrate relationships (Nitrate Relationship).")
          ),
          h4("How to Interpret"),
          p("Spikes in turbidity or nitrate can indicate runoff events; higher dissolved oxygen generally indicates better water quality.")
        ),
        tabPanel("Time Trend", plotlyOutput("timePlot")),
        tabPanel("Boxplot by Site", plotlyOutput("boxPlot")),
        tabPanel(
          "Nitrate Relationship",
          fluidRow(
            column(
              width = 4,
              selectInput("xvar_rel", "Water-quality variable (x):",
                          choices = c("Conductivity (\u00B5S/cm)" = "cond_uScm",
                                      "Dissolved Oxygen (mg/L)"   = "do_mgL",
                                      "Turbidity (FNU)"           = "turb_FNU"),
                          selected = "cond_uScm"),
              checkboxInput("facet_sites", "Facet by site (when All sites)", TRUE),
              checkboxInput("add_lm",    "Add linear fit (LM)", TRUE),
              checkboxInput("add_loess", "Add smooth (LOESS)", FALSE),
              sliderInput("trim_pct", "Hide extremes (percent trimmed each tail)",
                          min = 0, max = 5, value = 1, step = 1, post = "%"),
              sliderInput("loess_span", "LOESS span",
                          min = 0.2, max = 1, value = 0.6, step = 0.1),
              helpText("This tab uses the Combined dataset for best nitrate overlap. For nitrate, try S2_downstream.")
            ),
            column(
              width = 8,
              tags$div(
                style = "display:flex; gap:18px; align-items:baseline; margin:4px 0 6px 0;",
                uiOutput("rel_badge_cov"),
                uiOutput("rel_badge_n")
              ),
              plotlyOutput("relPlot"),
              tags$div(style="margin-top:8px;",
                       uiOutput("corr_text"),
                       uiOutput("lm_text"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session){

  # -------- Base table by dataset (for Time Trend / Boxplot) --------
  get_base <- reactive({
    switch(input$dataset,
           "wq_hourly"      = wq_hourly,
           "nitrate_hourly" = nitrate_hourly,
           "wq_nitrate"     = wq_nitrate)
  })

  # -------- Time Trend (native plotly lines) --------
  reactive_data <- reactive({
    df <- get_base() %>% filter(sensor == input$sensor)
    if (input$site != "All sites") df <- df %>% filter(site == input$site)
    arrange(df, date_time)
  })

  output$timePlot <- renderPlotly({
    df <- reactive_data()
    v  <- input$variable

    validate(
      need(nrow(df) > 0, "No data for this selection."),
      need(v %in% names(df), paste0("Variable '", v, "' not in this dataset.")),
      need(any(!is.na(df[[v]])), paste0("All values of '", var_labels[[v]], "' are missing."))
    )

    plot_ly(
      df,
      x = ~date_time,
      y = ~.data[[v]],
      color = ~site,
      type = "scatter",
      mode = "lines",
      hovertext = ~paste(
        "Site:", site,
        "<br>Sensor:", sensor,
        "<br>Date:", date_time,
        "<br>", var_labels[[v]], ":", signif(.data[[v]], 4)
      ),
      hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = var_labels[[v]]),
        legend = list(title = list(text = "Site"))
      )
  })

  # -------- Boxplot (compare sites at the same sensor) --------
  output$boxPlot <- renderPlotly({
    df <- get_base() %>% filter(sensor == input$sensor)
    v  <- input$variable
    validate(
      need(nrow(df) > 0, "No data for this selection."),
      need(v %in% names(df), paste0("Variable '", v, "' not in this dataset."))
    )
    p <- ggplot(df, aes(x = site, y = .data[[v]], fill = site,
                        text = paste0(
                          "Site: ", site,
                          "<br>", var_labels[[v]], ": ", signif(.data[[v]], 4)
                        ))) +
      geom_boxplot(na.rm = TRUE, outlier.alpha = 0.25) +
      labs(x = "Site", y = var_labels[[v]],
           title = paste0("Distribution across sites (", input$sensor, ")")) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  # -------- Nitrate Relationship (always use Combined) --------
  rel_base <- reactive({ wq_nitrate })

  # auto-switch visible dataset selector to "Combined" when tab opens
  observeEvent(input$tabs, {
    if (identical(input$tabs, "Nitrate Relationship") && input$dataset != "wq_nitrate") {
      updateSelectInput(session, "dataset", selected = "wq_nitrate")
    }
  })

  dat_rel <- reactive({
    req(input$xvar_rel, input$sensor)
    df <- rel_base() %>% filter(sensor == input$sensor)
    if (input$site != "All sites") df <- df %>% filter(site == input$site)
    xvar <- input$xvar_rel
    df <- df %>% filter(!is.na(.data[[xvar]]), !is.na(nitrate_umolL))
    # trim symmetric tails
    if (is.numeric(input$trim_pct) && input$trim_pct > 0) {
      lo <- input$trim_pct / 100
      hi <- 1 - lo
      qx <- quantile(df[[xvar]], probs = c(lo, hi), na.rm = TRUE, names = FALSE)
      qy <- quantile(df$nitrate_umolL, probs = c(lo, hi), na.rm = TRUE, names = FALSE)
      df <- df %>%
        filter(.data[[xvar]] >= qx[1], .data[[xvar]] <= qx[2],
               nitrate_umolL >= qy[1],  nitrate_umolL <= qy[2])
    }
    df
  })

  # badges
  output$rel_badge_cov <- renderUI({
    xvar <- input$xvar_rel
    df0  <- rel_base()
    has  <- mean(!is.na(df0[[xvar]]) & !is.na(df0$nitrate_umolL))
    tags$span(paste0("Coverage: ", round(100*has), "% pairs"),
              class = "badge bg-success", style = "padding:6px 10px;")
  })
  output$rel_badge_n <- renderUI({
    n <- nrow(dat_rel())
    tags$span(paste0("n = ", scales::comma(n)),
              class = "badge bg-secondary", style = "padding:6px 10px;")
  })

  output$relPlot <- renderPlotly({
    df <- dat_rel()
    validate(need(nrow(df) > 0, "No overlapping data for this selection (try S2_downstream)."))
    xvar <- input$xvar_rel

    p <- ggplot(df, aes(x = .data[[xvar]], y = nitrate_umolL, colour = site,
                        text = paste0(
                          "Site: ", site,
                          "<br>Sensor: ", sensor,
                          "<br>", var_labels[[xvar]], ": ", signif(.data[[xvar]], 4),
                          "<br>Nitrate (\u00B5mol/L): ", signif(nitrate_umolL, 4)
                        ))) +
      geom_point(alpha = 0.6, size = 1.3)

    if (isTRUE(input$add_lm)) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    if (isTRUE(input$add_loess)) {
      p <- p + geom_smooth(method = "loess", se = FALSE, linetype = 2, span = input$loess_span)
    }

    p <- p +
      labs(
        x = var_labels[[xvar]],
        y = "Nitrate (\u00B5mol/L)",
        title = if (input$site == "All sites")
          paste("Nitrate vs", var_labels[[xvar]], "across sites (", input$sensor, ")")
        else
          paste("Nitrate vs", var_labels[[xvar]], "at", input$site, "(", input$sensor, ")")
      ) +
      theme_minimal(base_size = 13)

    if (input$site == "All sites" && isTRUE(input$facet_sites)) {
      p <- p + facet_wrap(~ site, scales = "free")
    }

    ggplotly(p, tooltip = "text") |>
      layout(legend = list(title = list(text = "Site")))
  })

  output$corr_text <- renderUI({
    df <- dat_rel(); xvar <- input$xvar_rel
    if (!nrow(df)) return(HTML(""))
    ct <- try(suppressWarnings(cor.test(df[[xvar]], df$nitrate_umolL)), silent = TRUE)
    if (inherits(ct, "try-error")) return(HTML(""))
    HTML(
      paste0("<b>Correlation (Pearson):</b> r = ",
             sprintf("%.2f", unname(ct$estimate)),
             ", p = ", formatC(ct$p.value, format = "e", digits = 2))
    )
  })

  output$lm_text <- renderUI({
    df <- dat_rel(); xvar <- input$xvar_rel
    if (!nrow(df) || !isTRUE(input$add_lm)) return(HTML(""))
    fit <- try(lm(nitrate_umolL ~ df[[xvar]]), silent = TRUE)
    if (inherits(fit, "try-error")) return(HTML(""))
    b1 <- coef(fit)[2]
    units <- switch(xvar,
                    cond_uScm = "\u00B5mol/L per \u00B5S/cm",
                    do_mgL    = "\u00B5mol/L per mg/L",
                    turb_FNU  = "\u00B5mol/L per FNU")
    HTML(paste0("<b>LM slope:</b> ", sprintf("%.2f", b1), " ", units))
  })
}

shinyApp(ui, server)
