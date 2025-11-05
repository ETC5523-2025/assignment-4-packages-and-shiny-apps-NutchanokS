# NEON Freshwater Quality Explorer (Final Version)

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(rlang)

# ---- Load packaged data ---------------------------------------------
data("wq_hourly",      package = "assign4nspack")
data("nitrate_hourly", package = "assign4nspack")
data("wq_nitrate",     package = "assign4nspack")

# Pretty variable labels
var_labels <- c(
  cond_uScm     = "Conductivity (\u00B5S/cm)",
  do_mgL        = "Dissolved oxygen (mg/L)",
  turb_FNU      = "Turbidity (FNU)",
  nitrate_umolL = "Nitrate (\u00B5mol/L)"
)

# ---- UI --------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    base_font    = font_google("Lato"),
    heading_font = font_google("Lato"),
    primary = "#1F77B4",
    bg = "#f9fafb", fg = "#111827"
  ),
  titlePanel("NEON Freshwater Quality Explorer"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput(
        "dataset", "Choose Dataset:",
        choices = c("Water Quality" = "wq_hourly",
                    "Nitrate"       = "nitrate_hourly",
                    "Combined"      = "wq_nitrate"),
        selected = "wq_hourly"
      ),

      conditionalPanel(
        condition = "input.tabs !== 'Boxplot by Site'",
        selectInput(
          "site", "Choose Site:",
          choices = c("All sites", sort(unique(wq_hourly$site))),
          selected = "All sites"
        )
      ),

      selectInput(
        "sensor", "Sensor Location:",
        choices = sort(unique(wq_hourly$sensor)),
        selected = "S1_upstream"
      ),

      conditionalPanel(
        condition = "input.tabs !== 'Nitrate Relationship'",
        selectInput(
          "variable", "Select Variable:",
          choices = c("Conductivity (\u00B5S/cm)" = "cond_uScm",
                      "Dissolved Oxygen (mg/L)"   = "do_mgL",
                      "Turbidity (FNU)"           = "turb_FNU",
                      "Nitrate (\u00B5mol/L)"     = "nitrate_umolL"),
          selected = "cond_uScm"
        )
      ),
      hr(),
      helpText(
        "Explore high-frequency water-quality and nitrate data from NEON.",
        "Nitrate may be missing at upstream sensors — this is expected."
      )
    ),

    mainPanel(
      width = 8,
      tabsetPanel(
        id = "tabs",

        # --------------------------------------------------------------
        tabPanel(
          "About the Data",
          h4("About the Dataset"),
          p("This app visualises data from NEON’s in-situ freshwater sensors
             (DP1.20288.001 Water Quality and DP1.20033.001 Nitrate).
             Each site includes sensors such as S1_upstream and S2_downstream."),

          h4("How to Use"),
          tags$ul(
            tags$li("Select dataset, site, sensor, and variable."),
            tags$li("Time Trend: shows changes over time."),
            tags$li("Boxplot: compares distributions across sites."),
            tags$li("Nitrate Relationship: explores nitrate vs other variables.")
          ),

          h4("Data Interpretation"),
          p("These datasets were analysed in Kermorvant et al. (2023, PLOS ONE),
             which modelled nonlinear relationships between nitrate and other
             water-quality variables across NEON sites."),
          tags$ul(
            tags$li("Conductivity reflects dissolved ions; elevated values may indicate runoff or pollution."),
            tags$li("Dissolved oxygen shows diurnal fluctuations due to photosynthesis and respiration."),
            tags$li("Turbidity measures suspended sediments affecting light and nutrient uptake."),
            tags$li("Nitrate concentration responds to flow and land use, peaking after rainfall or agricultural input.")
          ),
          p("LEWI - Lewis Run (urban/agricultural) showed highest nitrate levels,
             CARI - Caribou Creek (subarctic) the lowest, and ARIK - Arikaree (semi-arid) intermediate variability.")
        ),

        # --------------------------------------------------------------
        tabPanel("Time Trend",  plotlyOutput("timePlot")),
        tabPanel("Boxplot by Site", plotlyOutput("boxPlot")),

        # --------------------------------------------------------------
        tabPanel(
          "Nitrate Relationship",
          fluidRow(
            column(
              width = 5,
              selectInput(
                "xvar_rel", "Water-quality variable (x):",
                choices = c("Conductivity (\u00B5S/cm)" = "cond_uScm",
                            "Dissolved Oxygen (mg/L)"   = "do_mgL",
                            "Turbidity (FNU)"           = "turb_FNU"),
                selected = "cond_uScm"
              ),
              checkboxInput("facet_sites", "Facet by site (when All sites)", TRUE),
              checkboxInput("add_lm",    "Add linear fit (LM)", TRUE),
              checkboxInput("add_loess", "Add smooth (LOESS)", TRUE),
              sliderInput("trim_pct", "Hide extremes (percent trimmed each tail)",
                          min = 0, max = 5, value = 1, step = 1, post = "%"),
              sliderInput("loess_span", "LOESS span",
                          min = 0.2, max = 1, value = 0.6, step = 0.1),
              helpText("This uses the Combined dataset for best nitrate overlap.
                        For nitrate, try S2_downstream.")
            ),
            column(
              width = 7,
              tags$div(
                style = "display:flex; gap:12px; align-items:baseline; margin:0 0 6px 0;",
                uiOutput("rel_badge_cov"),
                uiOutput("rel_badge_n")
              ),
              plotlyOutput("relPlot"),
              tags$div(style="margin-top:6px;",
                       uiOutput("corr_text"),
                       uiOutput("lm_text"),
                       uiOutput("fit_legend"))
            )
          )
        )
      ),
      hr(),
      uiOutput("summary_text")
    )
  )
)

# ---- SERVER ----------------------------------------------------------
server <- function(input, output, session){

  # Auto adjust site/dataset when switching tabs
  observeEvent(input$tabs, {
    if (identical(input$tabs, "Boxplot by Site"))
      updateSelectInput(session, "site", selected = "All sites")
    if (identical(input$tabs, "Nitrate Relationship") && input$dataset != "wq_nitrate")
      updateSelectInput(session, "dataset", selected = "wq_nitrate")
  })

  get_base <- reactive({
    switch(input$dataset,
           "wq_hourly"      = wq_hourly,
           "nitrate_hourly" = nitrate_hourly,
           "wq_nitrate"     = wq_nitrate)
  })

  # Fit legend (below correlation)
  output$fit_legend <- renderUI({
    HTML(
      "<div style='margin-top:6px; font-size:12px;'>
         <span style='display:inline-block;width:34px;border-top:3px solid #E69F00;
               vertical-align:middle;margin-right:6px'></span>
         <span style='margin-right:16px'>Linear (LM) fit</span>
         <span style='display:inline-block;width:34px;border-top:3px dashed #000;
               vertical-align:middle;margin-right:6px'></span>
         <span>LOESS fit</span>
       </div>"
    )
  })

  # ---- Time Trend ----------------------------------------------------
  reactive_data <- reactive({
    df <- get_base() %>% filter(sensor == input$sensor)
    if (!identical(input$site, "All sites")) df <- df %>% filter(site == input$site)
    arrange(df, date_time)
  })

  output$timePlot <- renderPlotly({
    df <- reactive_data()
    v  <- input$variable

    validate(
      need(nrow(df) > 0, "No data for this selection."),
      need(v %in% names(df), paste0("Variable '", v, "' not in this dataset."))
    )

    plot_ly(
      df,
      x = ~date_time,
      y = ~.data[[v]],
      color = ~site,
      type = "scatter", mode = "lines",
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
        legend = list(title = list(text = "Site")),
        font   = list(family = "Lato", size = 11)
      )
  })

  # ---- Boxplot -------------------------------------------------------
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
      theme_minimal(base_size = 12, base_family = "Lato") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold"))

    ggplotly(p, tooltip = "text") |>
      layout(font = list(family = "Lato", size = 11))
  })

  # ---- Nitrate Relationship ------------------------------------------
  rel_base <- reactive(wq_nitrate)

  dat_rel <- reactive({
    req(input$xvar_rel, input$sensor)
    df <- rel_base() %>% filter(sensor == input$sensor)
    if (!identical(input$site, "All sites")) df <- df %>% filter(site == input$site)
    xvar <- input$xvar_rel
    df <- df %>% filter(!is.na(.data[[xvar]]), !is.na(nitrate_umolL))

    # symmetric trim
    if (is.numeric(input$trim_pct) && input$trim_pct > 0) {
      lo <- input$trim_pct / 100; hi <- 1 - lo
      qx <- quantile(df[[xvar]], probs = c(lo, hi), na.rm = TRUE, names = FALSE)
      qy <- quantile(df$nitrate_umolL, probs = c(lo, hi), na.rm = TRUE, names = FALSE)
      df <- df %>%
        filter(.data[[xvar]] >= qx[1], .data[[xvar]] <= qx[2],
               nitrate_umolL >= qy[1],  nitrate_umolL <= qy[2])
    }
    df
  })

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
              class = "badge bg-danger", style = "padding:6px 10px;")
  })

  # --- LOESS precompute for Plotly reliability ---
  output$relPlot <- renderPlotly({
    df <- dat_rel()
    validate(need(nrow(df) > 0, "No overlapping data for this selection (try S2_downstream)."))
    xvar <- input$xvar_rel
    xsym <- sym(xvar)

    loess_lines <- NULL
    if (isTRUE(input$add_loess) && nrow(df) >= 10) {
      by_vars <- if (identical(input$site, "All sites")) vars(site) else vars()
      loess_lines <- df |>
        group_by(!!!by_vars) |>
        group_modify(function(.x, .k) {
          xgrid <- seq(min(.x[[xvar]], na.rm = TRUE),
                       max(.x[[xvar]], na.rm = TRUE),
                       length.out = 200)
          fit <- try(loess(formula(paste0("nitrate_umolL ~ `", xvar, "`")),
                           data = .x, span = input$loess_span), silent = TRUE)
          if (inherits(fit, "try-error")) return(tibble(!!xsym := numeric(0), loess_y = numeric(0)))
          newdat <- setNames(data.frame(xgrid), xvar)
          pred   <- suppressWarnings(predict(fit, newdata = newdat))
          tibble(!!xsym := xgrid, loess_y = pred)
        }) |>
        ungroup()
    }

    p <- ggplot(df, aes(x = .data[[xvar]], y = nitrate_umolL,
                        colour = site,
                        text = paste0(
                          "Site: ", site,
                          "<br>Sensor: ", sensor,
                          "<br>", var_labels[[xvar]], ": ", signif(.data[[xvar]], 4),
                          "<br>Nitrate (\u00B5mol/L): ", signif(nitrate_umolL, 4)
                        ))) +
      geom_point(alpha = 0.6, size = 1.5)

    # --- LM line: bright orange solid ---
    if (isTRUE(input$add_lm)) {
      p <- p + geom_smooth(aes(group = 1),
                           method = "lm", se = FALSE,
                           colour = "#E69F00", linewidth = 1.3,
                           formula = y ~ x)
    }

    # --- LOESS line: thick black dashed ---
    if (!is.null(loess_lines) && nrow(loess_lines)) {
      p <- p + geom_line(
        data = loess_lines,
        aes(x = .data[[xvar]], y = loess_y),
        colour = "#000000",
        linewidth = 1.4,
        linetype = "longdash",
        alpha = 0.9,
        inherit.aes = FALSE
      )
    }

    if (identical(input$site, "All sites") && isTRUE(input$facet_sites))
      p <- p + facet_wrap(~ site, scales = "free")

    p <- p +
      labs(x = var_labels[[xvar]], y = "Nitrate (\u00B5mol/L)",
           title = if (identical(input$site, "All sites"))
             paste("Nitrate vs", var_labels[[xvar]], "across sites (", input$sensor, ")")
           else
             paste("Nitrate vs", var_labels[[xvar]], "at", input$site, "(", input$sensor, ")")) +
      theme_minimal(base_size = 11, base_family = "Lato") +
      theme(plot.title = element_text(face = "bold"),
            legend.title = element_text(size = 9),
            legend.text  = element_text(size = 9),
            strip.text   = element_text(size = 9))

    ggplotly(p, tooltip = "text") |>
      layout(font = list(family = "Lato", size = 11))
  })

  output$corr_text <- renderUI({
    df <- dat_rel(); xvar <- input$xvar_rel
    if (!nrow(df)) return(HTML(""))
    ct <- try(suppressWarnings(cor.test(df[[xvar]], df$nitrate_umolL)), silent = TRUE)
    if (inherits(ct, "try-error")) return(HTML(""))
    HTML(paste0("<b>Correlation (Pearson):</b> r = ",
                sprintf("%.2f", unname(ct$estimate)),
                ", p = ", formatC(ct$p.value, format = "e", digits = 2)))
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

  # ---- Summary text below tabs --------------------------------------
  output$summary_text <- renderUI({
    switch(input$tabs,
           "Time Trend" = HTML("<b>Interpretation:</b> Observe temporal trends in water-quality variables. Diurnal and seasonal patterns reveal changes in flow, temperature, and oxygen."),
           "Boxplot by Site" = HTML("<b>Interpretation:</b> Compare central tendency and variability across sites. Wider boxes or outliers indicate greater short-term fluctuations."),
           "Nitrate Relationship" = HTML("<b>Interpretation:</b> Explore nonlinear nitrate–water-quality relationships. LOESS (black dashed) shows local trends; LM (orange) shows overall direction."),
           HTML("")
    )
  })
}

shinyApp(ui, server)
