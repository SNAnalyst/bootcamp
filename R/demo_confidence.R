#' Interactive confidence interval demo
#'
#' Explore how sample size, variability, and confidence level affect confidence
#' intervals
#'
#' This interactive demo launches a \code{shiny} application that helps students
#' build intuition for confidence intervals. The app has two modes:
#'
#' \itemize{
#'   \item a mean-based mode for a continuous outcome, where the interval is
#'   built from a sample drawn from a normal population;
#'   \item a proportion-based mode for a binary outcome, where the interval is
#'   built from repeated Bernoulli trials.
#' }
#'
#' For both modes, students can change the sample size, confidence level, and
#' population characteristics, then immediately see how the interval width
#' changes. A second panel shows many repeated samples, which makes it easier to
#' explain that a confidence interval is part of a long-run procedure rather than
#' a probability statement about one fixed interval.
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
#' bootcamp::demo_confidence()
#' }
demo_confidence <- function(launch.browser = TRUE) {
  if (!base::requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with utils::install.packages('shiny').")
  }

  run_bootcamp_shiny_demo(
    app_builder = build_demo_confidence_app,
    launch.browser = launch.browser
  )
}




#' @keywords internal
build_demo_confidence_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Understanding confidence intervals"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::tags$p(
            "Change the population and sampling settings to see how interval ",
            "width and long-run coverage change."
          ),
          shiny::selectInput(
            inputId = "parameter_type",
            label = "Parameter",
            choices = c(
              "Mean of a continuous variable" = "mean",
              "Proportion of successes" = "proportion"
            ),
            selected = "mean"
          ),
          shiny::sliderInput(
            inputId = "n",
            label = "Sample size",
            min = 5,
            max = 250,
            value = 30,
            step = 1
          ),
          shiny::sliderInput(
            inputId = "conf_level",
            label = "Confidence level (%)",
            min = 80,
            max = 99,
            value = 95,
            step = 1
          ),
          shiny::sliderInput(
            inputId = "repetitions",
            label = "Repeated samples",
            min = 20,
            max = 200,
            value = 100,
            step = 10
          ),
          shiny::uiOutput("confidence_specific_controls"),
          shiny::actionButton("resample", "Draw new samples"),
          shiny::tags$hr(),
          shiny::verbatimTextOutput("confidence_summary_text")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::plotOutput("single_interval_plot", height = "430px")
            ),
            shiny::column(
              width = 6,
              shiny::plotOutput("repeated_interval_plot", height = "430px")
            )
          )
        )
      )
    ),
    server = function(input, output, session) {
      output$confidence_specific_controls <- shiny::renderUI({
        if (identical(input$parameter_type, "mean")) {
          shiny::tagList(
            shiny::sliderInput(
              inputId = "true_mean",
              label = "True population mean",
              min = -10,
              max = 10,
              value = 0,
              step = 0.5
            ),
            shiny::sliderInput(
              inputId = "population_sd",
              label = "Population standard deviation",
              min = 0.5,
              max = 8,
              value = 3,
              step = 0.5
            )
          )
        } else {
          shiny::sliderInput(
            inputId = "true_proportion",
            label = "True population proportion",
            min = 0.05,
            max = 0.95,
            value = 0.50,
            step = 0.01
          )
        }
      })

      confidence_data <- shiny::eventReactive(
        eventExpr = list(
          input$resample,
          input$parameter_type,
          input$n,
          input$conf_level,
          input$repetitions,
          input$true_mean,
          input$population_sd,
          input$true_proportion
        ),
        valueExpr = {
          simulate_demo_confidence(
            parameter_type = input$parameter_type,
            n = input$n,
            conf_level = input$conf_level / 100,
            repetitions = input$repetitions,
            true_mean = get_demo_confidence_input_value(input$true_mean, 0),
            population_sd = get_demo_confidence_input_value(input$population_sd, 3),
            true_proportion = get_demo_confidence_input_value(input$true_proportion, 0.5)
          )
        },
        ignoreNULL = FALSE
      )

      output$single_interval_plot <- shiny::renderPlot({
        plot_demo_confidence_single(confidence_data())
      })

      output$repeated_interval_plot <- shiny::renderPlot({
        plot_demo_confidence_repeated(confidence_data())
      })

      output$confidence_summary_text <- shiny::renderText({
        simulation <- confidence_data()
        interval_width <- simulation$single_interval["upper"] - simulation$single_interval["lower"]
        coverage_rate <- mean(simulation$repeated_intervals$covers)

        if (identical(simulation$parameter_type, "mean")) {
          return(paste0(
            "Single-sample mean = ",
            formatC(simulation$single_interval["estimate"], digits = 3, format = "f"),
            "\nConfidence interval = [",
            formatC(simulation$single_interval["lower"], digits = 3, format = "f"),
            ", ",
            formatC(simulation$single_interval["upper"], digits = 3, format = "f"),
            "]",
            "\nInterval width = ",
            formatC(interval_width, digits = 3, format = "f"),
            "\nIntervals covering the true mean in this simulation = ",
            formatC(coverage_rate * 100, digits = 1, format = "f"),
            "%"
          ))
        }

        paste0(
          "Single-sample proportion = ",
          formatC(simulation$single_interval["estimate"], digits = 3, format = "f"),
          "\nConfidence interval = [",
          formatC(simulation$single_interval["lower"], digits = 3, format = "f"),
          ", ",
          formatC(simulation$single_interval["upper"], digits = 3, format = "f"),
          "]",
          "\nInterval width = ",
          formatC(interval_width, digits = 3, format = "f"),
          "\nIntervals covering the true proportion in this simulation = ",
          formatC(coverage_rate * 100, digits = 1, format = "f"),
          "%"
        )
      })
    }
  )
}




#' @keywords internal
simulate_demo_confidence <- function(parameter_type = c("mean", "proportion"),
                                     n,
                                     conf_level,
                                     repetitions,
                                     true_mean = 0,
                                     population_sd = 1,
                                     true_proportion = 0.5) {
  parameter_type <- match.arg(parameter_type)
  validate_demo_confidence_inputs(
    n = n,
    conf_level = conf_level,
    repetitions = repetitions,
    population_sd = population_sd,
    true_proportion = true_proportion
  )

  if (identical(parameter_type, "mean")) {
    single_sample <- stats::rnorm(n = n, mean = true_mean, sd = population_sd)
    single_interval <- compute_mean_confidence_interval(single_sample, conf_level)
    repeated_intervals <- simulate_mean_confidence_intervals(
      n = n,
      conf_level = conf_level,
      repetitions = repetitions,
      true_mean = true_mean,
      population_sd = population_sd
    )

    return(list(
      parameter_type = "mean",
      true_value = true_mean,
      single_sample = single_sample,
      single_interval = single_interval,
      repeated_intervals = repeated_intervals,
      x_limits = get_demo_confidence_limits(
        values = c(single_sample, repeated_intervals$lower, repeated_intervals$upper, true_mean)
      )
    ))
  }

  single_sample <- stats::rbinom(n = n, size = 1, prob = true_proportion)
  single_interval <- compute_proportion_confidence_interval(sum(single_sample), n, conf_level)
  repeated_intervals <- simulate_proportion_confidence_intervals(
    n = n,
    conf_level = conf_level,
    repetitions = repetitions,
    true_proportion = true_proportion
  )

  list(
    parameter_type = "proportion",
    true_value = true_proportion,
    single_sample = single_sample,
    single_interval = single_interval,
    repeated_intervals = repeated_intervals,
    x_limits = c(0, 1)
  )
}


#' @keywords internal
compute_mean_confidence_interval <- function(sample_values, conf_level) {
  if (!is.numeric(sample_values) || length(sample_values) < 2L) {
    stop("'sample_values' should be a numeric vector with at least two observations")
  }

  estimate <- mean(sample_values)
  sample_sd <- stats::sd(sample_values)
  standard_error <- sample_sd / sqrt(length(sample_values))
  critical_value <- stats::qt(1 - (1 - conf_level) / 2, df = length(sample_values) - 1)
  margin <- critical_value * standard_error

  c(
    estimate = estimate,
    lower = estimate - margin,
    upper = estimate + margin
  )
}


#' @keywords internal
compute_proportion_confidence_interval <- function(successes, n, conf_level) {
  if (!is.numeric(successes) || length(successes) != 1L || is.na(successes)) {
    stop("'successes' should be a single non-missing number")
  }

  interval <- stats::prop.test(
    x = successes,
    n = n,
    conf.level = conf_level,
    correct = FALSE
  )$conf.int

  c(
    estimate = successes / n,
    lower = interval[1],
    upper = interval[2]
  )
}


#' @keywords internal
simulate_mean_confidence_intervals <- function(n,
                                               conf_level,
                                               repetitions,
                                               true_mean,
                                               population_sd) {
  intervals <- lapply(seq_len(repetitions), function(iteration) {
    sample_values <- stats::rnorm(n = n, mean = true_mean, sd = population_sd)
    ci <- compute_mean_confidence_interval(sample_values, conf_level)

    data.frame(
      iteration = iteration,
      estimate = ci["estimate"],
      lower = ci["lower"],
      upper = ci["upper"],
      covers = (ci["lower"] <= true_mean) && (ci["upper"] >= true_mean),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, intervals)
}


#' @keywords internal
simulate_proportion_confidence_intervals <- function(n,
                                                     conf_level,
                                                     repetitions,
                                                     true_proportion) {
  intervals <- lapply(seq_len(repetitions), function(iteration) {
    successes <- stats::rbinom(n = 1, size = n, prob = true_proportion)
    ci <- compute_proportion_confidence_interval(successes, n, conf_level)

    data.frame(
      iteration = iteration,
      estimate = ci["estimate"],
      lower = ci["lower"],
      upper = ci["upper"],
      covers = (ci["lower"] <= true_proportion) && (ci["upper"] >= true_proportion),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, intervals)
}


#' @keywords internal
plot_demo_confidence_single <- function(simulation) {
  sample_values <- simulation$single_sample
  single_interval <- simulation$single_interval
  jitter_y <- stats::runif(length(sample_values), min = 0.05, max = 0.30)

  graphics::plot(
    sample_values,
    jitter_y,
    xlim = simulation$x_limits,
    ylim = c(0, 1),
    xlab = if (identical(simulation$parameter_type, "mean")) "Observed values" else "Observed outcomes / parameter scale",
    ylab = "",
    yaxt = "n",
    pch = 19,
    col = "#1F77B4",
    main = "One sample and its confidence interval"
  )

  graphics::segments(
    x0 = single_interval["lower"],
    y0 = 0.72,
    x1 = single_interval["upper"],
    y1 = 0.72,
    lwd = 5,
    col = "#2C7FB8"
  )
  graphics::points(single_interval["estimate"], 0.72, pch = 21, bg = "#2C7FB8", cex = 1.8)
  graphics::abline(v = simulation$true_value, col = "#D55E00", lty = 2, lwd = 2)

  graphics::legend(
    "topright",
    legend = c("Observed data", "Confidence interval", "Point estimate", "True value"),
    col = c("#1F77B4", "#2C7FB8", "#2C7FB8", "#D55E00"),
    pch = c(19, NA, 21, NA),
    lty = c(NA, 1, NA, 2),
    lwd = c(NA, 5, NA, 2),
    bty = "n"
  )
  graphics::grid(col = "grey90")
}


#' @keywords internal
plot_demo_confidence_repeated <- function(simulation) {
  repeated_intervals <- simulation$repeated_intervals
  interval_colours <- ifelse(repeated_intervals$covers, "#2CA25F", "#D95F0E")

  graphics::plot(
    x = repeated_intervals$estimate,
    y = repeated_intervals$iteration,
    type = "n",
    xlim = simulation$x_limits,
    ylim = c(1, nrow(repeated_intervals)),
    xlab = if (identical(simulation$parameter_type, "mean")) "Parameter scale" else "Proportion scale",
    ylab = "Repeated samples",
    main = "Intervals from repeated samples"
  )

  graphics::segments(
    x0 = repeated_intervals$lower,
    y0 = repeated_intervals$iteration,
    x1 = repeated_intervals$upper,
    y1 = repeated_intervals$iteration,
    col = interval_colours,
    lwd = 2
  )
  graphics::points(
    x = repeated_intervals$estimate,
    y = repeated_intervals$iteration,
    pch = 19,
    cex = 0.6,
    col = interval_colours
  )
  graphics::abline(v = simulation$true_value, col = "#D55E00", lty = 2, lwd = 2)
  graphics::legend(
    "topright",
    legend = c("Covers the true value", "Misses the true value", "True value"),
    col = c("#2CA25F", "#D95F0E", "#D55E00"),
    lty = c(1, 1, 2),
    lwd = c(2, 2, 2),
    pch = c(19, 19, NA),
    bty = "n"
  )
  graphics::grid(col = "grey92")
}


#' @keywords internal
get_demo_confidence_limits <- function(values) {
  value_range <- range(values, na.rm = TRUE)
  padding <- max(diff(value_range) * 0.10, 0.5)
  c(value_range[1] - padding, value_range[2] + padding)
}


#' @keywords internal
get_demo_confidence_input_value <- function(value, default) {
  if (is.null(value) || length(value) == 0L || is.na(value)) {
    return(default)
  }

  value
}


#' @keywords internal
validate_demo_confidence_inputs <- function(n,
                                            conf_level,
                                            repetitions,
                                            population_sd,
                                            true_proportion) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 2 || n != as.integer(n)) {
    stop("'n' should be a positive whole number of at least 2")
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' should be a single number between 0 and 1")
  }
  if (!is.numeric(repetitions) || length(repetitions) != 1L || is.na(repetitions) || repetitions < 1 || repetitions != as.integer(repetitions)) {
    stop("'repetitions' should be a positive whole number")
  }
  if (!is.numeric(population_sd) || length(population_sd) != 1L || is.na(population_sd) || population_sd <= 0) {
    stop("'population_sd' should be a positive number")
  }
  if (!is.numeric(true_proportion) || length(true_proportion) != 1L || is.na(true_proportion) || true_proportion <= 0 || true_proportion >= 1) {
    stop("'true_proportion' should be between 0 and 1")
  }

  invisible(TRUE)
}
