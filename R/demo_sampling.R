#' Interactive sampling demo
#'
#' Explore sampling variation and the sampling distribution of a statistic
#'
#' This interactive demo launches a \code{shiny} application that helps
#' students build intuition for sampling variation. The app shows a population,
#' one current sample drawn from that population, and the sampling distribution
#' of a chosen statistic across repeated samples.
#'
#' Students can switch between several population shapes and compare how the
#' sample mean and sample median behave as the sample size and number of
#' repeated samples change. This makes it easier to see why a single sample can
#' differ from the population and why a sampling distribution becomes tighter as
#' the sample size increases.
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
#' bootcamp::demo_sampling()
#' }
demo_sampling <- function(launch.browser = TRUE) {
  if (!base::requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with utils::install.packages('shiny').")
  }

  run_bootcamp_shiny_demo(
    app_builder = build_demo_sampling_app,
    launch.browser = launch.browser
  )
}




#' @keywords internal
build_demo_sampling_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Sampling variation and sampling distributions"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::tags$p(
            "Compare the population, one current sample, and the sampling ",
            "distribution built from repeated samples."
          ),
          shiny::selectInput(
            inputId = "population_type",
            label = "Population shape",
            choices = c(
              "Normal" = "normal",
              "Right-skewed" = "right_skewed",
              "Bimodal" = "bimodal",
              "With outliers" = "with_outliers"
            ),
            selected = "normal"
          ),
          shiny::selectInput(
            inputId = "statistic",
            label = "Statistic",
            choices = c(
              "Mean" = "mean",
              "Median" = "median"
            ),
            selected = "mean"
          ),
          shiny::sliderInput(
            inputId = "sample_size",
            label = "Sample size",
            min = 5,
            max = 250,
            value = 30,
            step = 1
          ),
          shiny::sliderInput(
            inputId = "repetitions",
            label = "Repeated samples per batch",
            min = 10,
            max = 300,
            value = 100,
            step = 10
          ),
          shiny::actionButton("draw_one_sample", "Draw one sample"),
          shiny::actionButton("draw_many_samples", "Draw many samples"),
          shiny::actionButton("reset_sampling_demo", "Reset"),
          shiny::tags$hr(),
          shiny::verbatimTextOutput("sampling_summary_text")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::plotOutput("sampling_population_plot", height = "280px")
            ),
            shiny::column(
              width = 6,
              shiny::plotOutput("sampling_sample_plot", height = "280px")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::plotOutput("sampling_distribution_plot", height = "320px")
            )
          )
        )
      )
    ),
    server = function(input, output, session) {
      sampling_values <- shiny::reactiveValues(
        population = NULL,
        current_sample = NULL,
        sampled_statistics = numeric(0)
      )

      shiny::observeEvent(
        eventExpr = list(input$population_type, input$sample_size, input$statistic),
        handlerExpr = {
          sampling_values$population <- create_demo_sampling_population(
            population_type = input$population_type
          )
          sampling_values$current_sample <- draw_demo_sample(
            population = sampling_values$population,
            sample_size = input$sample_size
          )
          sampling_values$sampled_statistics <- numeric(0)
        },
        ignoreNULL = FALSE
      )

      shiny::observeEvent(input$draw_one_sample, {
        sampling_values$current_sample <- draw_demo_sample(
          population = sampling_values$population,
          sample_size = input$sample_size
        )
        sampling_values$sampled_statistics <- c(
          sampling_values$sampled_statistics,
          compute_demo_sample_statistic(
            sample_values = sampling_values$current_sample,
            statistic = input$statistic
          )
        )
      })

      shiny::observeEvent(input$draw_many_samples, {
        simulation <- simulate_demo_sampling_distribution(
          population = sampling_values$population,
          sample_size = input$sample_size,
          repetitions = input$repetitions,
          statistic = input$statistic
        )

        sampling_values$current_sample <- simulation$last_sample
        sampling_values$sampled_statistics <- c(
          sampling_values$sampled_statistics,
          simulation$statistics
        )
      })

      shiny::observeEvent(input$reset_sampling_demo, {
        sampling_values$population <- create_demo_sampling_population(
          population_type = input$population_type
        )
        sampling_values$current_sample <- draw_demo_sample(
          population = sampling_values$population,
          sample_size = input$sample_size
        )
        sampling_values$sampled_statistics <- numeric(0)
      })

      output$sampling_population_plot <- shiny::renderPlot({
        plot_demo_sampling_population(
          population = sampling_values$population,
          statistic = input$statistic
        )
      })

      output$sampling_sample_plot <- shiny::renderPlot({
        plot_demo_sampling_sample(
          current_sample = sampling_values$current_sample,
          statistic = input$statistic
        )
      })

      output$sampling_distribution_plot <- shiny::renderPlot({
        plot_demo_sampling_distribution(
          sampled_statistics = sampling_values$sampled_statistics,
          true_value = compute_demo_sample_statistic(
            sample_values = sampling_values$population,
            statistic = input$statistic
          ),
          statistic = input$statistic
        )
      })

      output$sampling_summary_text <- shiny::renderText({
        if (is.null(sampling_values$population) || is.null(sampling_values$current_sample)) {
          return("Loading the sampling demo...")
        }

        summary_values <- summarize_demo_sampling_state(
          population = sampling_values$population,
          current_sample = sampling_values$current_sample,
          sampled_statistics = sampling_values$sampled_statistics,
          statistic = input$statistic
        )

        paste0(
          "Population ", summary_values$statistic_label, " = ",
          formatC(summary_values$population_value, digits = 3, format = "f"),
          "\nCurrent sample ", summary_values$statistic_label, " = ",
          formatC(summary_values$current_sample_value, digits = 3, format = "f"),
          "\nRepeated samples stored = ",
          summary_values$n_repetitions,
          if (summary_values$n_repetitions > 1L) {
            paste0(
              "\nSampling distribution center = ",
              formatC(summary_values$sampling_center, digits = 3, format = "f"),
              "\nSampling distribution SD = ",
              formatC(summary_values$sampling_sd, digits = 3, format = "f")
            )
          } else {
            "\nDraw more samples to build a sampling distribution."
          }
        )
      })
    }
  )
}




#' @keywords internal
create_demo_sampling_population <- function(population_type = c("normal", "right_skewed", "bimodal", "with_outliers"),
                                            population_size = 5000L) {
  population_type <- match.arg(population_type)
  validate_demo_sampling_inputs(
    population_size = population_size
  )

  if (identical(population_type, "normal")) {
    return(stats::rnorm(n = population_size, mean = 0, sd = 1))
  }
  if (identical(population_type, "right_skewed")) {
    return(stats::rgamma(n = population_size, shape = 2.5, scale = 0.8))
  }
  if (identical(population_type, "bimodal")) {
    first_mode <- stats::rnorm(n = population_size / 2, mean = -1.5, sd = 0.7)
    second_mode <- stats::rnorm(n = population_size - length(first_mode), mean = 1.5, sd = 0.7)
    return(c(first_mode, second_mode))
  }

  base_population <- stats::rnorm(n = population_size - 25L, mean = 0, sd = 1)
  outliers <- stats::rnorm(n = 25L, mean = 7, sd = 0.6)
  c(base_population, outliers)
}


#' @keywords internal
draw_demo_sample <- function(population, sample_size) {
  validate_demo_sampling_inputs(
    population = population,
    sample_size = sample_size
  )

  sample(population, size = sample_size, replace = FALSE)
}


#' @keywords internal
compute_demo_sample_statistic <- function(sample_values, statistic = c("mean", "median")) {
  statistic <- match.arg(statistic)
  if (!is.numeric(sample_values) || length(sample_values) < 1L) {
    stop("'sample_values' should be a numeric vector with at least one observation")
  }

  if (identical(statistic, "mean")) {
    return(mean(sample_values))
  }

  stats::median(sample_values)
}


#' @keywords internal
simulate_demo_sampling_distribution <- function(population,
                                                sample_size,
                                                repetitions,
                                                statistic = c("mean", "median")) {
  statistic <- match.arg(statistic)
  validate_demo_sampling_inputs(
    population = population,
    sample_size = sample_size,
    repetitions = repetitions
  )

  drawn_samples <- lapply(seq_len(repetitions), function(i) {
    draw_demo_sample(population = population, sample_size = sample_size)
  })
  sampled_statistics <- vapply(
    drawn_samples,
    compute_demo_sample_statistic,
    numeric(1),
    statistic = statistic
  )

  list(
    statistics = sampled_statistics,
    last_sample = drawn_samples[[length(drawn_samples)]]
  )
}


#' @keywords internal
summarize_demo_sampling_state <- function(population,
                                          current_sample,
                                          sampled_statistics,
                                          statistic = c("mean", "median")) {
  statistic <- match.arg(statistic)
  validate_demo_sampling_inputs(
    population = population,
    current_sample = current_sample
  )
  if (!is.numeric(sampled_statistics)) {
    stop("'sampled_statistics' should be numeric")
  }

  summary_values <- list(
    statistic_label = statistic,
    population_value = compute_demo_sample_statistic(population, statistic),
    current_sample_value = compute_demo_sample_statistic(current_sample, statistic),
    n_repetitions = length(sampled_statistics),
    sampling_center = NA_real_,
    sampling_sd = NA_real_
  )

  if (length(sampled_statistics) > 0L) {
    summary_values$sampling_center <- mean(sampled_statistics)
  }
  if (length(sampled_statistics) > 1L) {
    summary_values$sampling_sd <- stats::sd(sampled_statistics)
  }

  summary_values
}


#' @keywords internal
plot_demo_sampling_population <- function(population, statistic) {
  if (is.null(population)) {
    graphics::plot(
      NA,
      xlim = c(-1, 1),
      ylim = c(0, 1),
      xlab = "Population values",
      ylab = "Frequency",
      main = "Population"
    )
    graphics::text(0, 0.5, "Loading population...")
    graphics::grid(col = "grey90")
    return(invisible(NULL))
  }

  true_value <- compute_demo_sample_statistic(population, statistic)
  graphics::hist(
    x = population,
    breaks = 30,
    col = "#CFE8F3",
    border = "white",
    main = "Population",
    xlab = "Population values"
  )
  graphics::abline(v = true_value, col = "#D55E00", lwd = 3)
  graphics::grid(col = "grey90")
}


#' @keywords internal
plot_demo_sampling_sample <- function(current_sample, statistic) {
  if (is.null(current_sample)) {
    graphics::plot(
      NA,
      xlim = c(-1, 1),
      ylim = c(0, 1),
      xlab = "Sample values",
      ylab = "Frequency",
      main = "Current sample"
    )
    graphics::text(0, 0.5, "Drawing the first sample...")
    graphics::grid(col = "grey90")
    return(invisible(NULL))
  }

  sample_value <- compute_demo_sample_statistic(current_sample, statistic)
  graphics::hist(
    x = current_sample,
    breaks = min(20L, max(5L, floor(length(current_sample) / 2))),
    col = "#A6D854",
    border = "white",
    main = "Current sample",
    xlab = "Sample values"
  )
  graphics::abline(v = sample_value, col = "#1B7837", lwd = 3)
  graphics::grid(col = "grey90")
}


#' @keywords internal
plot_demo_sampling_distribution <- function(sampled_statistics, true_value, statistic) {
  if (length(sampled_statistics) == 0L) {
    graphics::plot(
      NA,
      xlim = c(true_value - 1, true_value + 1),
      ylim = c(0, 1),
      xlab = paste0("Sample ", statistic),
      ylab = "",
      yaxt = "n",
      main = "Sampling distribution"
    )
    graphics::text(
      x = true_value,
      y = 0.5,
      labels = "Draw repeated samples to build the sampling distribution."
    )
    graphics::abline(v = true_value, col = "#D55E00", lwd = 2, lty = 2)
    graphics::grid(col = "grey90")
    return(invisible(NULL))
  }

  graphics::hist(
    x = sampled_statistics,
    breaks = min(30L, max(8L, floor(length(sampled_statistics) / 4))),
    col = "#80B1D3",
    border = "white",
    main = "Sampling distribution",
    xlab = paste0("Sample ", statistic)
  )
  graphics::abline(v = true_value, col = "#D55E00", lwd = 3, lty = 2)
  graphics::abline(v = mean(sampled_statistics), col = "#1F78B4", lwd = 3)
  graphics::grid(col = "grey90")
}


#' @keywords internal
validate_demo_sampling_inputs <- function(population = NULL,
                                          current_sample = NULL,
                                          sample_size = NULL,
                                          repetitions = NULL,
                                          population_size = NULL) {
  if (!is.null(population)) {
    if (!is.numeric(population) || length(population) < 2L) {
      stop("'population' should be a numeric vector with at least two values")
    }
  }
  if (!is.null(current_sample)) {
    if (!is.numeric(current_sample) || length(current_sample) < 1L) {
      stop("'current_sample' should be a numeric vector with at least one value")
    }
  }
  if (!is.null(sample_size)) {
    if (!is.numeric(sample_size) || length(sample_size) != 1L || is.na(sample_size) ||
        sample_size < 1 || sample_size != as.integer(sample_size)) {
      stop("'sample_size' should be a positive whole number")
    }
    if (!is.null(population) && sample_size > length(population)) {
      stop("'sample_size' cannot be larger than the population size")
    }
  }
  if (!is.null(repetitions)) {
    if (!is.numeric(repetitions) || length(repetitions) != 1L || is.na(repetitions) ||
        repetitions < 1 || repetitions != as.integer(repetitions)) {
      stop("'repetitions' should be a positive whole number")
    }
  }
  if (!is.null(population_size)) {
    if (!is.numeric(population_size) || length(population_size) != 1L || is.na(population_size) ||
        population_size < 100 || population_size != as.integer(population_size)) {
      stop("'population_size' should be a whole number of at least 100")
    }
  }

  invisible(TRUE)
}
