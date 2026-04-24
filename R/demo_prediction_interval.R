#' Interactive demo of confidence and prediction intervals in regression
#'
#' Explore why prediction intervals are wider than confidence intervals
#'
#' This interactive \code{shiny} demo shows a simple linear regression with two
#' uncertainty bands. The confidence interval reflects uncertainty about the
#' mean predicted value, whereas the prediction interval reflects uncertainty
#' about a new individual observation. Showing both at the same time helps
#' students see why the prediction interval is always wider.
#'
#' Students can vary the sample size, noise level, confidence level, and the
#' focal x-value at which the interval is summarised. The app redraws the data,
#' the fitted line, the confidence band, and the prediction band immediately.
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
#' bootcamp::demo_prediction_interval()
#' }
demo_prediction_interval <- function(launch.browser = TRUE) {
  if (!base::requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with utils::install.packages('shiny').")
  }

  run_bootcamp_shiny_demo(
    app_builder = build_demo_prediction_interval_app,
    launch.browser = launch.browser
  )
}




#' @keywords internal
build_demo_prediction_interval_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Confidence and prediction intervals in regression"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::tags$p(
            "Compare the confidence interval for the fitted mean with the ",
            "prediction interval for a new observation at the same x-value."
          ),
          shiny::sliderInput(
            inputId = "prediction_sample_size",
            label = "Sample size",
            min = 20,
            max = 200,
            value = 70,
            step = 1
          ),
          shiny::sliderInput(
            inputId = "prediction_noise_level",
            label = "Noise level",
            min = 0.2,
            max = 3,
            value = 1,
            step = 0.1
          ),
          shiny::sliderInput(
            inputId = "prediction_confidence_level",
            label = "Confidence level",
            min = 0.8,
            max = 0.99,
            value = 0.95,
            step = 0.01
          ),
          shiny::sliderInput(
            inputId = "prediction_focus_x",
            label = "Focal x-value",
            min = 0,
            max = 10,
            value = 5,
            step = 0.1
          ),
          shiny::actionButton("prediction_resimulate", "Resimulate"),
          shiny::tags$hr(),
          shiny::verbatimTextOutput("prediction_summary_text")
        ),
        shiny::mainPanel(
          shiny::plotOutput("prediction_interval_plot", height = "560px")
        )
      )
    ),
    server = function(input, output, session) {
      prediction_state <- shiny::eventReactive(
        eventExpr = list(
          input$prediction_resimulate,
          input$prediction_sample_size,
          input$prediction_noise_level,
          input$prediction_confidence_level,
          input$prediction_focus_x
        ),
        valueExpr = {
          get_demo_prediction_interval_state(
            sample_size = input$prediction_sample_size,
            noise_level = input$prediction_noise_level,
            confidence_level = input$prediction_confidence_level,
            x_focus = input$prediction_focus_x
          )
        },
        ignoreNULL = FALSE
      )

      output$prediction_interval_plot <- shiny::renderPlot({
        plot_demo_prediction_interval_state(prediction_state())
      })

      output$prediction_summary_text <- shiny::renderText({
        state <- prediction_state()
        paste0(
          "Focal x-value = ", formatC(state$x_focus, digits = 2, format = "f"),
          "\nPredicted mean = ", formatC(state$mean_prediction, digits = 3, format = "f"),
          "\nConfidence interval = [",
          formatC(state$confidence_interval[1], digits = 3, format = "f"), ", ",
          formatC(state$confidence_interval[2], digits = 3, format = "f"), "]",
          "\nPrediction interval = [",
          formatC(state$prediction_interval[1], digits = 3, format = "f"), ", ",
          formatC(state$prediction_interval[2], digits = 3, format = "f"), "]",
          "\nConfidence interval width = ",
          formatC(diff(state$confidence_interval), digits = 3, format = "f"),
          "\nPrediction interval width = ",
          formatC(diff(state$prediction_interval), digits = 3, format = "f"),
          "\n\nInterpretation:\nThe prediction interval is wider because it must cover ",
          "the uncertainty in the fitted line and the scatter of individual outcomes."
        )
      })
    }
  )
}




#' @keywords internal
get_demo_prediction_interval_state <- function(sample_size = 70L,
                                               noise_level = 1,
                                               confidence_level = 0.95,
                                               x_focus = 5) {
  data <- simulate_demo_prediction_data(
    sample_size = sample_size,
    noise_level = noise_level
  )

  summarize_demo_prediction_interval_state(
    data = data,
    x_focus = x_focus,
    confidence_level = confidence_level
  )
}


#' @keywords internal
simulate_demo_prediction_data <- function(sample_size = 70L, noise_level = 1) {
  validate_demo_prediction_inputs(
    sample_size = sample_size,
    noise_level = noise_level,
    confidence_level = 0.95,
    x_focus = 5
  )

  x <- sort(stats::runif(sample_size, min = 0, max = 10))
  y <- 3 + 0.9 * x + stats::rnorm(sample_size, sd = noise_level)

  data.frame(
    x = x,
    y = y,
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
summarize_demo_prediction_interval_state <- function(data,
                                                     x_focus = 5,
                                                     confidence_level = 0.95) {
  validate_demo_prediction_data(data)
  validate_demo_prediction_inputs(
    sample_size = nrow(data),
    noise_level = 1,
    confidence_level = confidence_level,
    x_focus = x_focus
  )

  fit <- stats::lm(y ~ x, data = data)
  newdata <- data.frame(x = x_focus)
  focus_confidence <- stats::predict(
    fit,
    newdata = newdata,
    interval = "confidence",
    level = confidence_level
  )
  focus_prediction <- stats::predict(
    fit,
    newdata = newdata,
    interval = "prediction",
    level = confidence_level
  )

  x_grid <- seq(min(data$x), max(data$x), length.out = 200L)
  grid_data <- data.frame(x = x_grid)
  confidence_band <- stats::predict(
    fit,
    newdata = grid_data,
    interval = "confidence",
    level = confidence_level
  )
  prediction_band <- stats::predict(
    fit,
    newdata = grid_data,
    interval = "prediction",
    level = confidence_level
  )

  list(
    data = data,
    fit = fit,
    x_focus = x_focus,
    confidence_level = confidence_level,
    mean_prediction = unname(focus_confidence[1, "fit"]),
    confidence_interval = unname(focus_confidence[1, c("lwr", "upr")]),
    prediction_interval = unname(focus_prediction[1, c("lwr", "upr")]),
    band_data = data.frame(
      x = x_grid,
      fit = confidence_band[, "fit"],
      conf_low = confidence_band[, "lwr"],
      conf_high = confidence_band[, "upr"],
      pred_low = prediction_band[, "lwr"],
      pred_high = prediction_band[, "upr"],
      stringsAsFactors = FALSE
    )
  )
}


#' @keywords internal
plot_demo_prediction_interval_state <- function(state) {
  data <- state$data
  band_data <- state$band_data
  y_limits <- range(
    c(data$y, band_data$pred_low, band_data$pred_high),
    finite = TRUE
  )

  graphics::plot(
    x = data$x,
    y = data$y,
    xlab = "Explanatory variable (x)",
    ylab = "Outcome (y)",
    pch = 19,
    col = "#1F77B4",
    main = "Prediction intervals are wider than confidence intervals",
    ylim = y_limits
  )

  # Draw the wider band first so the narrower confidence band remains visible on
  # top and students can compare the two uncertainty concepts directly.
  graphics::polygon(
    x = c(band_data$x, rev(band_data$x)),
    y = c(band_data$pred_low, rev(band_data$pred_high)),
    col = grDevices::adjustcolor("#F4A261", alpha.f = 0.25),
    border = NA
  )
  graphics::polygon(
    x = c(band_data$x, rev(band_data$x)),
    y = c(band_data$conf_low, rev(band_data$conf_high)),
    col = grDevices::adjustcolor("#2A9D8F", alpha.f = 0.35),
    border = NA
  )

  graphics::lines(band_data$x, band_data$fit, col = "#D55E00", lwd = 3)
  graphics::abline(v = state$x_focus, col = "grey40", lty = 3, lwd = 1.5)

  graphics::segments(
    x0 = state$x_focus,
    y0 = state$confidence_interval[1],
    x1 = state$x_focus,
    y1 = state$confidence_interval[2],
    col = "#2A9D8F",
    lwd = 5
  )
  graphics::segments(
    x0 = state$x_focus,
    y0 = state$prediction_interval[1],
    x1 = state$x_focus,
    y1 = state$prediction_interval[2],
    col = "#E76F51",
    lwd = 3
  )
  graphics::points(state$x_focus, state$mean_prediction, pch = 19, cex = 1.3, col = "#264653")
  graphics::grid(col = "grey90")
  graphics::legend(
    "topleft",
    legend = c(
      "Observed data",
      "Fitted line",
      "Confidence band for the mean",
      "Prediction band for a new case",
      "Focal prediction"
    ),
    col = c("#1F77B4", "#D55E00", "#2A9D8F", "#E76F51", "#264653"),
    pch = c(19, NA, 15, 15, 19),
    lwd = c(NA, 3, NA, NA, NA),
    pt.cex = c(1, NA, 2, 2, 1),
    bty = "n"
  )
}


#' @keywords internal
validate_demo_prediction_inputs <- function(sample_size,
                                            noise_level,
                                            confidence_level,
                                            x_focus) {
  if (!is.numeric(sample_size) || length(sample_size) != 1L || is.na(sample_size) ||
      sample_size < 10 || sample_size != as.integer(sample_size)) {
    stop("'sample_size' should be a whole number of at least 10")
  }
  if (!is.numeric(noise_level) || length(noise_level) != 1L || is.na(noise_level) ||
      noise_level <= 0) {
    stop("'noise_level' should be a positive number")
  }
  if (!is.numeric(confidence_level) || length(confidence_level) != 1L || is.na(confidence_level) ||
      confidence_level <= 0 || confidence_level >= 1) {
    stop("'confidence_level' should be a single number between 0 and 1")
  }
  if (!is.numeric(x_focus) || length(x_focus) != 1L || is.na(x_focus) || !is.finite(x_focus)) {
    stop("'x_focus' should be a single finite number")
  }

  invisible(TRUE)
}


#' @keywords internal
validate_demo_prediction_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("'data' should be a data.frame")
  }
  if (!all(c("x", "y") %in% colnames(data))) {
    stop("'data' should contain the columns 'x' and 'y'")
  }
  if (!is.numeric(data$x) || !is.numeric(data$y)) {
    stop("'x' and 'y' should be numeric")
  }
  if (nrow(data) < 2L) {
    stop("'data' should contain at least two observations")
  }

  invisible(TRUE)
}
