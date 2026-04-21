#' Interactive residual diagnostics demo
#'
#' Explore how different data problems show up in OLS diagnostics
#'
#' This interactive demo launches a \code{shiny} application that links a
#' scatterplot, an OLS fit, a residual plot, a residual histogram, and a Q-Q
#' plot. The app is designed to help students connect the visual assumptions of
#' linear regression to the patterns they see in standard diagnostic plots.
#'
#' Several scenario buttons generate characteristic problems such as curvature,
#' heteroscedasticity, outliers, and influential observations. This makes it
#' easier to compare a well-behaved regression with cases in which important
#' assumptions are questionable.
#'
#' @param launch.browser Logical, should the app open in a browser window?
#'   Defaults to \code{TRUE}.
#'
#' @return The result of \code{shiny::runApp()}, called for its side effect of
#'   starting the interactive demo.
#' @export
#'
#' @examples
#' \dontrun{
#' bootcamp::demo_residuals()
#' }
demo_residuals <- function(launch.browser = TRUE) {
  if (!base::requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with utils::install.packages('shiny').")
  }

  run_bootcamp_shiny_demo(
    app_builder = build_demo_residuals_app,
    launch.browser = launch.browser
  )
}




#' @keywords internal
build_demo_residuals_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Residual diagnostics for OLS"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::tags$p(
            "Choose a scenario and compare the data cloud with the diagnostic ",
            "plots that the fitted OLS model produces."
          ),
          shiny::selectInput(
            inputId = "scenario",
            label = "Scenario",
            choices = c(
              "Well-behaved" = "well_behaved",
              "Curvature" = "curvature",
              "Heteroscedasticity" = "heteroscedasticity",
              "Outlier" = "outlier",
              "Influential point" = "influential_point"
            ),
            selected = "well_behaved"
          ),
          shiny::sliderInput(
            inputId = "sample_size",
            label = "Sample size",
            min = 20,
            max = 200,
            value = 70,
            step = 1
          ),
          shiny::sliderInput(
            inputId = "noise_level",
            label = "Noise level",
            min = 0.2,
            max = 3,
            value = 1,
            step = 0.1
          ),
          shiny::actionButton("resimulate_residual_demo", "Resimulate"),
          shiny::tags$hr(),
          shiny::verbatimTextOutput("residual_summary_text")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::plotOutput("residual_scatter_plot", height = "280px")
            ),
            shiny::column(
              width = 6,
              shiny::plotOutput("residual_residual_plot", height = "280px")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::plotOutput("residual_hist_plot", height = "280px")
            ),
            shiny::column(
              width = 6,
              shiny::plotOutput("residual_qq_plot", height = "280px")
            )
          )
        )
      )
    ),
    server = function(input, output, session) {
      residual_state <- shiny::eventReactive(
        eventExpr = list(
          input$resimulate_residual_demo,
          input$scenario,
          input$sample_size,
          input$noise_level
        ),
        valueExpr = {
          get_demo_residual_state(
            scenario = input$scenario,
            sample_size = input$sample_size,
            noise_level = input$noise_level
          )
        },
        ignoreNULL = FALSE
      )

      output$residual_scatter_plot <- shiny::renderPlot({
        plot_demo_residual_scatter(residual_state())
      })

      output$residual_residual_plot <- shiny::renderPlot({
        plot_demo_residual_residuals(residual_state())
      })

      output$residual_hist_plot <- shiny::renderPlot({
        plot_demo_residual_histogram(residual_state())
      })

      output$residual_qq_plot <- shiny::renderPlot({
        plot_demo_residual_qq(residual_state())
      })

      output$residual_summary_text <- shiny::renderText({
        state <- residual_state()
        diagnostics <- state$diagnostics

        paste0(
          "Slope = ", formatC(state$slope, digits = 3, format = "f"),
          "\nIntercept = ", formatC(state$intercept, digits = 3, format = "f"),
          "\nR-squared = ", formatC(state$r_squared, digits = 3, format = "f"),
          "\nQuadratic gain in R-squared = ", formatC(diagnostics$quadratic_r_squared_gain, digits = 3, format = "f"),
          "\nResidual spread correlation = ", formatC(diagnostics$heteroscedasticity_metric, digits = 3, format = "f"),
          "\nLarge residuals detected = ", diagnostics$outlier_count,
          "\nInfluential points detected = ", diagnostics$influential_count,
          "\n\nInterpretation:\n",
          paste(diagnostics$messages, collapse = "\n")
        )
      })
    }
  )
}




#' @keywords internal
get_demo_residual_state <- function(scenario = c("well_behaved", "curvature", "heteroscedasticity", "outlier", "influential_point"),
                                    sample_size = 70L,
                                    noise_level = 1) {
  scenario <- match.arg(scenario)
  data <- simulate_demo_residual_data(
    scenario = scenario,
    sample_size = sample_size,
    noise_level = noise_level
  )
  fit <- stats::lm(y ~ x, data = data)
  diagnostics <- summarize_demo_residual_patterns(data = data, fit = fit)

  data$fitted <- stats::fitted(fit)
  data$residual <- stats::residuals(fit)
  data$standardized_residual <- stats::rstandard(fit)
  data$cooks_distance <- stats::cooks.distance(fit)

  coefficients <- stats::coef(fit)

  list(
    scenario = scenario,
    data = data,
    fit = fit,
    diagnostics = diagnostics,
    intercept = unname(coefficients[1]),
    slope = unname(coefficients[2]),
    r_squared = unname(summary(fit)$r.squared)
  )
}


#' @keywords internal
simulate_demo_residual_data <- function(scenario = c("well_behaved", "curvature", "heteroscedasticity", "outlier", "influential_point"),
                                        sample_size = 70L,
                                        noise_level = 1) {
  scenario <- match.arg(scenario)
  validate_demo_residual_inputs(
    sample_size = sample_size,
    noise_level = noise_level
  )

  x <- sort(stats::runif(sample_size, min = -3, max = 3))
  linear_signal <- 2 + 1.25 * x
  point_role <- rep("regular", sample_size)

  if (identical(scenario, "well_behaved")) {
    y <- linear_signal + stats::rnorm(sample_size, sd = noise_level)
  } else if (identical(scenario, "curvature")) {
    y <- linear_signal + 0.9 * (x^2 - mean(x^2)) + stats::rnorm(sample_size, sd = noise_level)
  } else if (identical(scenario, "heteroscedasticity")) {
    local_sd <- noise_level * (0.35 + 1.7 * (x - min(x)) / diff(range(x)))
    y <- linear_signal + stats::rnorm(sample_size, sd = local_sd)
  } else if (identical(scenario, "outlier")) {
    y <- linear_signal + stats::rnorm(sample_size, sd = noise_level)
    outlier_index <- max(2L, floor(sample_size / 2))
    y[outlier_index] <- y[outlier_index] + 7 * noise_level
    point_role[outlier_index] <- "outlier"
  } else {
    y <- linear_signal + stats::rnorm(sample_size, sd = noise_level)
    influential_index <- sample_size
    x[influential_index] <- max(x[-influential_index]) + 3.2
    y[influential_index] <- min(y[-influential_index]) - 5.5 * noise_level
    point_role[influential_index] <- "influential"
  }

  data.frame(
    x = x,
    y = y,
    point_role = point_role,
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
summarize_demo_residual_patterns <- function(data, fit = NULL) {
  validate_demo_residual_data(data)
  if (is.null(fit)) {
    fit <- stats::lm(y ~ x, data = data)
  }

  residuals <- stats::residuals(fit)
  fitted_values <- stats::fitted(fit)
  standardized_residuals <- stats::rstandard(fit)
  cooks_distance <- stats::cooks.distance(fit)
  quadratic_fit <- stats::lm(y ~ x + I(x^2), data = data)
  quadratic_gain <- summary(quadratic_fit)$r.squared - summary(fit)$r.squared
  heteroscedasticity_metric <- abs(stats::cor(abs(residuals), fitted_values))
  outlier_count <- sum(abs(standardized_residuals) > 2.5, na.rm = TRUE)
  influential_count <- sum(cooks_distance > 4 / nrow(data), na.rm = TRUE)

  messages <- character(0)
  if (quadratic_gain > 0.08) {
    messages <- c(messages, "- Residual pattern suggests non-linearity.")
  }
  if (heteroscedasticity_metric > 0.25) {
    messages <- c(messages, "- Residual spread changes across fitted values.")
  }
  if (outlier_count > 0L) {
    messages <- c(messages, "- One or more observations have unusually large residuals.")
  }
  if (influential_count > 0L) {
    messages <- c(messages, "- One or more observations have strong influence on the fitted line.")
  }
  if (length(messages) == 0L) {
    messages <- "- Diagnostics look broadly consistent with a well-behaved linear model."
  }

  list(
    quadratic_r_squared_gain = unname(quadratic_gain),
    heteroscedasticity_metric = unname(heteroscedasticity_metric),
    outlier_count = outlier_count,
    influential_count = influential_count,
    messages = messages
  )
}


#' @keywords internal
plot_demo_residual_scatter <- function(state) {
  data <- state$data
  highlighted <- data$point_role != "regular"

  graphics::plot(
    x = data$x,
    y = data$y,
    xlab = "Explanatory variable (x)",
    ylab = "Dependent variable (y)",
    pch = 19,
    col = ifelse(highlighted, "#D55E00", "#1F77B4"),
    main = "Data and fitted OLS line"
  )
  graphics::abline(state$fit, col = "#009E73", lwd = 3)

  if (any(highlighted)) {
    graphics::points(
      x = data$x[highlighted],
      y = data$y[highlighted],
      pch = 21,
      bg = "#D55E00",
      col = "#D55E00",
      cex = 1.8
    )
  }

  graphics::grid(col = "grey90")
}


#' @keywords internal
plot_demo_residual_residuals <- function(state) {
  data <- state$data
  highlighted <- data$point_role != "regular"

  graphics::plot(
    x = data$fitted,
    y = data$residual,
    xlab = "Fitted values",
    ylab = "Residuals",
    pch = 19,
    col = ifelse(highlighted, "#D55E00", "#1F77B4"),
    main = "Residual plot"
  )
  graphics::abline(h = 0, col = "#009E73", lwd = 2)
  graphics::grid(col = "grey90")
}


#' @keywords internal
plot_demo_residual_histogram <- function(state) {
  graphics::hist(
    x = state$data$residual,
    breaks = 15,
    col = "#C7E9C0",
    border = "white",
    main = "Residual histogram",
    xlab = "Residuals"
  )
  graphics::abline(v = 0, col = "#009E73", lwd = 2)
  graphics::grid(col = "grey90")
}


#' @keywords internal
plot_demo_residual_qq <- function(state) {
  stats::qqnorm(
    y = state$data$residual,
    pch = 19,
    col = "#1F77B4",
    main = "Normal Q-Q plot"
  )
  stats::qqline(state$data$residual, col = "#D55E00", lwd = 2)
  graphics::grid(col = "grey90")
}


#' @keywords internal
validate_demo_residual_inputs <- function(sample_size, noise_level) {
  if (!is.numeric(sample_size) || length(sample_size) != 1L || is.na(sample_size) ||
      sample_size < 10 || sample_size != as.integer(sample_size)) {
    stop("'sample_size' should be a whole number of at least 10")
  }
  if (!is.numeric(noise_level) || length(noise_level) != 1L || is.na(noise_level) ||
      noise_level <= 0) {
    stop("'noise_level' should be a positive number")
  }

  invisible(TRUE)
}


#' @keywords internal
validate_demo_residual_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("'data' should be a data.frame")
  }
  required_columns <- c("x", "y", "point_role")
  if (!all(required_columns %in% colnames(data))) {
    stop("'data' should contain 'x', 'y', and 'point_role'")
  }
  if (!is.numeric(data$x) || !is.numeric(data$y)) {
    stop("'x' and 'y' should be numeric")
  }

  invisible(TRUE)
}
