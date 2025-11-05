# Sports Stat Analyzer (Shiny app)
# Filename: app.R
# Purpose: Interactive Shiny app to analyze player-level statistics across multiple sports
# (basketball, football, baseball, soccer), build simple predictive models, and export results.
# This single-file app is GitHub-ready. Add a README.md and this file to a repo and users can
# run the app with: runApp('path/to/repo') or open this file in RStudio and click "Run App".

# ---------------------------
# Required packages
# ---------------------------
required_pkgs <- c("shiny","shinydashboard","ggplot2","DT","dplyr","tidyr","ranger","caret","shinythemes")
inst <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(inst)) install.packages(inst, repos = "https://cloud.r-project.org")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(ranger)
library(caret)
library(shinythemes)

# ---------------------------
# Helper: generate sample datasets for each sport
# ---------------------------
generate_sample_data <- function(sport, n = 200) {
  set.seed(42)
  if (sport == "Basketball") {
    tibble::tibble(
      player_id = paste0("B", sprintf("%03d", 1:n)),
      player = paste0("Player_", 1:n),
      games = sample(10:82, n, replace = TRUE),
      minutes = round(rnorm(n, 28, 6),1),
      points = round(pmax(0, rnorm(n, 12 + 0.4*minutes, 8)),1),
      rebounds = round(pmax(0, rnorm(n, 4 + 0.1*minutes, 3)),1),
      assists = round(pmax(0, rnorm(n, 3 + 0.05*minutes, 2)),1)
    )
  } else if (sport == "Football") {
    tibble::tibble(
      player_id = paste0("F", sprintf("%03d", 1:n)),
      player = paste0("Player_", 1:n),
      games = sample(1:17, n, replace = TRUE),
      attempts = sample(5:40, n, replace = TRUE),
      yards = round(rnorm(n, 50 + 6*attempts, 80),0),
      touchdowns = rbinom(n, size = 5, prob = 0.1)
    )
  } else if (sport == "Baseball") {
    tibble::tibble(
      player_id = paste0("BB", sprintf("%03d", 1:n)),
      player = paste0("Player_", 1:n),
      games = sample(20:162, n, replace = TRUE),
      at_bats = sample(50:600, n, replace = TRUE),
      hits = round(pmax(0, rbinom(n, size = at_bats, prob = 0.25))),
      home_runs = rbinom(n, size = 40, prob = 0.03)
    ) %>%
      mutate(average = ifelse(at_bats>0, round(hits/at_bats,3), NA))
  } else if (sport == "Soccer") {
    tibble::tibble(
      player_id = paste0("S", sprintf("%03d", 1:n)),
      player = paste0("Player_", 1:n),
      matches = sample(1:38, n, replace = TRUE),
      minutes = round(rnorm(n, 75, 20)),
      goals = rpois(n, lambda = pmax(0.1, 0.05*minutes/90*matches)),
      assists = rpois(n, lambda = pmax(0.05, 0.03*minutes/90*matches))
    )
  } else stop("Unknown sport")
}

# ---------------------------
# App UI
# ---------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Sports Stat Analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload & Sample", tabName = "upload", icon = icon("upload")),
      menuItem("Exploration", tabName = "explore", icon = icon("chart-line")),
      menuItem("Modeling", tabName = "model", icon = icon("cogs")),
      menuItem("Player Lookup", tabName = "lookup", icon = icon("search")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  dashboardBody(
    shiny::fluidPage(
      theme = shinytheme("flatly"),
      tabItems(
        tabItem(tabName = "upload",
                fluidRow(
                  box(title = "Data source", width = 6, status = "primary", solidHeader = TRUE,
                      selectInput("sport", "Select sport:", choices = c("Basketball","Football","Baseball","Soccer"), selected = "Basketball"),
                      fileInput("file", "Upload CSV (player-level rows)", accept = c('.csv')),
                      actionButton("use_sample", "Use sample dataset"),
                      helpText("If you upload, the app will attempt to detect columns automatically for common stats.")
                  ),
                  box(title = "Preview", width = 6, status = "info", solidHeader = TRUE,
                      DTOutput("preview_table")
                  )
                )
        ),
        tabItem(tabName = "explore",
                fluidRow(
                  box(width = 4, title = "Choose plot", status = "primary", solidHeader = TRUE,
                      uiOutput("xvar_ui"), uiOutput("yvar_ui"), selectInput("plot_type","Type",choices=c("scatter","histogram","boxplot"),selected="scatter"),
                      numericInput("point_size", "Point size (scatter)", value = 2, min = 0.5, step = 0.5)
                  ),
                  box(width = 8, title = "Plot", status = "info", solidHeader = TRUE,
                      plotOutput("main_plot", height = "500px")
                  )
                ),
                fluidRow(
                  box(width = 12, title = "Summary Table", status = "success", solidHeader = TRUE,
                      DTOutput("summary_table")
                  )
                )
        ),
        tabItem(tabName = "model",
                fluidRow(
                  box(width = 4, title = "Model settings", status = "primary", solidHeader = TRUE,
                      uiOutput("target_ui"),
                      selectInput("model_type","Model type", choices = c("Linear Regression","Random Forest"), selected = "Linear Regression"),
                      sliderInput("train_frac","Train fraction", min = 0.5, max = 0.9, value = 0.7),
                      actionButton("train_model","Train model")
                  ),
                  box(width = 8, title = "Model output", status = "info", solidHeader = TRUE,
                      verbatimTextOutput("model_summary"),
                      plotOutput("model_plot")
                  )
                )
        ),
        tabItem(tabName = "lookup",
                fluidRow(
                  box(width = 4, title = "Find player", status = "primary", solidHeader = TRUE,
                      textInput("player_search","Player name or ID"), actionButton("search_btn","Search")
                  ),
                  box(width = 8, title = "Player results", status = "info", solidHeader = TRUE,
                      DTOutput("player_table")
                  )
                )
        ),
        tabItem(tabName = "export",
                fluidRow(
                  box(width = 6, title = "Export data", status = "primary", solidHeader = TRUE,
                      downloadButton("download_data","Download current dataset (CSV)"),
                      downloadButton("download_model","Download model (RDS)")
                  ),
                  box(width = 6, title = "Notes for GitHub", status = "warning", solidHeader = TRUE,
                      tags$ul(
                        tags$li("Include this app.R in the project root."),
                        tags$li("Add README.md describing expected CSV format. Example columns are in the sample data."),
                        tags$li("Add a data/ folder with sample CSV files and a LICENSE if desired."),
                        tags$li("To deploy: use shinyapps.io or a Shiny server. Set options(shiny.port) if needed.")
                      )
                  )
                )
        )
      )
    )
  )
)

# ---------------------------
# Server logic
# ---------------------------
server <- function(input, output, session) {

  # Reactive: loaded dataset
  data_r <- reactiveVal(NULL)

  observeEvent(input$use_sample, {
    df <- generate_sample_data(input$sport, n = 300)
    data_r(df)
  })

  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(read.csv(input$file$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df)) {
      showNotification("Could not read uploaded CSV", type = "error")
    } else {
      data_r(df)
    }
  })

  output$preview_table <- renderDT({
    df <- data_r()
    if (is.null(df)) return(NULL)
    datatable(head(df, 50), options = list(pageLength = 8, scrollX = TRUE))
  })

  # Dynamic UI for EDA variable selection
  output$xvar_ui <- renderUI({
    df <- data_r()
    req(df)
    nums <- names(df)[sapply(df, is.numeric)]
    selectInput("xvar","X variable", choices = nums, selected = nums[1])
  })
  output$yvar_ui <- renderUI({
    df <- data_r()
    req(df)
    nums <- names(df)[sapply(df, is.numeric)]
    selectInput("yvar","Y variable", choices = c("(none)" = "", nums), selected = nums[2])
  })

  output$main_plot <- renderPlot({
    df <- data_r()
    req(df)
    x <- input$xvar
    y <- input$yvar
    type <- input$plot_type
    if (type == "scatter") {
      req(x, y)
      ggplot(df, aes_string(x = x, y = y)) + geom_point(size = input$point_size) + theme_minimal()
    } else if (type == "histogram") {
      req(x)
      ggplot(df, aes_string(x = x)) + geom_histogram(bins = 30) + theme_minimal()
    } else if (type == "boxplot") {
      req(x, y)
      ggplot(df, aes_string(x = x, y = y)) + geom_boxplot() + theme_minimal()
    }
  })

  output$summary_table <- renderDT({
    df <- data_r()
    req(df)
    nums <- df %>% select(where(is.numeric))
    summary_df <- nums %>% summarise_all(list(~mean(., na.rm = TRUE), ~sd(., na.rm = TRUE), ~min(., na.rm = TRUE), ~max(., na.rm = TRUE)))
    # tidy names
    names(summary_df) <- paste(rep(names(nums), each = 4), c("mean","sd","min","max"), sep = "_")
    datatable(round(summary_df,3), options = list(dom = 't'))
  })

  # Model UI: choose target variable
  output$target_ui <- renderUI({
    df <- data_r()
    req(df)
    nums <- names(df)[sapply(df, is.numeric)]
    selectInput("target","Target variable", choices = nums, selected = nums[1])
  })

  # Train model
  model_r <- reactiveVal(NULL)
  model_info <- reactiveVal(list())

  observeEvent(input$train_model, {
    df <- data_r()
    req(df)
    target <- input$target
    req(target)
    nums <- names(df)[sapply(df, is.numeric)]
    predictors <- setdiff(nums, target)
    if (length(predictors) < 1) {
      showNotification("Need at least one numeric predictor", type = "error")
      return()
    }
    # simple preprocessing: remove NA rows for chosen vars
    mdf <- df %>% select(all_of(c(target, predictors))) %>% na.omit()
    train_idx <- createDataPartition(mdf[[target]], p = input$train_frac, list = FALSE)
    tr <- mdf[train_idx, ]
    te <- mdf[-train_idx, ]

    if (input$model_type == "Linear Regression") {
      form <- as.formula(paste(target, "~ ."))
      fit <- lm(form, data = tr)
      preds <- predict(fit, newdata = te)
      model_r(list(type = "lm", fit = fit, test = te, preds = preds, target = target))
    } else if (input$model_type == "Random Forest") {
      # caret/ranger wrapper
      fit <- ranger::ranger(as.formula(paste(target, "~ .")), data = tr, importance = 'permutation')
      preds <- predict(fit, data = te)$predictions
      model_r(list(type = "rf", fit = fit, test = te, preds = preds, target = target))
    }
  })

  output$model_summary <- renderPrint({
    mod <- model_r()
    req(mod)
    if (mod$type == "lm") {
      print(summary(mod$fit))
    } else if (mod$type == "rf") {
      print(mod$fit)
      cat('\nVariable importance:\n')
      print(sort(mod$fit$variable.importance, decreasing = TRUE))
    }
  })

  output$model_plot <- renderPlot({
    mod <- model_r()
    req(mod)
    te <- mod$test
    preds <- mod$preds
    target <- mod$target
    dfp <- data.frame(obs = te[[target]], pred = preds)
    ggplot(dfp, aes(x = obs, y = pred)) + geom_point() + geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
      labs(x = 'Observed', y = 'Predicted') + theme_minimal()
  })

  # Player search
  output$player_table <- renderDT({
    input$search_btn
    isolate({
      df <- data_r()
      req(df)
      q <- input$player_search
      if (q == "" || is.null(q)) return(NULL)
      res <- df %>% filter(grepl(q, player, ignore.case = TRUE) | grepl(q, player_id, ignore.case = TRUE))
      datatable(res, options = list(pageLength = 10, scrollX = TRUE))
    })
  })

  # Export handlers
  output$download_data <- downloadHandler(
    filename = function() { paste0(tolower(input$sport), "_data.csv") },
    content = function(file) { write.csv(data_r(), file, row.names = FALSE) }
  )

  output$download_model <- downloadHandler(
    filename = function() { paste0('model_', Sys.Date(), '.rds') },
    content = function(file) { saveRDS(model_r(), file) }
  )

}

# ---------------------------
# Run app
# ---------------------------
shinyApp(ui, server)

# ---------------------------
# README suggestions (for GitHub):
# - Provide README.md explaining the expected CSV formats for each sport. Example columns:
#   Basketball: player_id, player, games, minutes, points, rebounds, assists
#   Football: player_id, player, games, attempts, yards, touchdowns
#   Baseball: player_id, player, games, at_bats, hits, home_runs, average
#   Soccer: player_id, player, matches, minutes, goals, assists
# - Add sample CSV files in data/ folder or rely on the "Use sample dataset" button.
# - Add LICENSE (e.g., MIT) and .gitignore for R (ignore .Rhistory, .RData, /data/*.csv if large).
# - Deploy with shinyapps.io or a Shiny Server. Include deployment notes in README.
