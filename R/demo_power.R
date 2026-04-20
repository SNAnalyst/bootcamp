#' Interactive power demo
#'
#' Explore Type I error, Type II error, and statistical power
#'
#' This interactive demo launches a \code{shiny} application that visualizes the
#' rejection rule of a standardized hypothesis test together with the sampling
#' distributions under \eqn{H_0} and \eqn{H_1}. The goal is to help students see
#' that Type I error, Type II error, and power are connected parts of the same
#' decision problem.
#'
#' The demo uses a standardized \eqn{z}-test framing. Under \eqn{H_0}, the test
#' statistic follows a standard normal distribution. Under \eqn{H_1}, the center
#' of the distribution shifts according to the chosen mean difference, sample
#' size, and population standard deviation. This keeps the visual explanation
#' simple while still showing the core trade-offs clearly.
#'
#' Students can change the alternative hypothesis, significance level, sample
#' size, true mean difference, and population standard deviation. The app then
#' updates the rejection region and the shaded regions corresponding to Type I
#' error, Type II error, and power.
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
#' bootcamp::demo_power()
#' }
demo_power <- function(launch.browser = TRUE) {
  if (!base::is.logical(launch.browser) || length(launch.browser) != 1L || is.na(launch.browser)) {
    stop("'launch.browser' should be TRUE or FALSE")
  }
  if (!base::requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with utils::install.packages('shiny').")
  }

  shiny::runApp(
    appObj = build_demo_power_app(),
    launch.browser = launch.browser
  )
}




#' @keywords internal
build_demo_power_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Type I error, Type II error, and power"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::tags$p(
            "This demo uses a standardized z-test view. The blue curve shows the ",
            "test statistic under H0 and the orange curve shows the test statistic ",
            "under H1."
          ),
          shiny::radioButtons(
            inputId = "alternative",
            label = "Alternative hypothesis",
            choices = c(
              "Two-sided" = "two.sided",
              "One-sided (greater)" = "greater",
              "One-sided (less)" = "less"
            ),
            selected = "two.sided"
          ),
          shiny::sliderInput(
            inputId = "mean_difference",
            label = "True mean difference under H1",
            min = -2,
            max = 2,
            value = 0.6,
            step = 0.05
          ),
          shiny::sliderInput(
            inputId = "population_sd",
            label = "Population standard deviation",
            min = 0.5,
            max = 5,
            value = 1.5,
            step = 0.1
          ),
          shiny::sliderInput(
            inputId = "n",
            label = "Sample size",
            min = 5,
            max = 250,
            value = 40,
            step = 1
          ),
          shiny::sliderInput(
            inputId = "alpha",
            label = "Significance level (alpha)",
            min = 0.01,
            max = 0.20,
            value = 0.05,
            step = 0.01
          ),
          shiny::actionButton("flip_mean_difference", "Flip effect direction"),
          shiny::tags$hr(),
          shiny::verbatimTextOutput("power_summary_text")
        ),
        shiny::mainPanel(
          shiny::plotOutput("power_plot", height = "560px")
        )
      )
    ),
    server = function(input, output, session) {
      shiny::observeEvent(input$flip_mean_difference, {
        shiny::updateSliderInput(
          session = session,
          inputId = "mean_difference",
          value = -input$mean_difference
        )
      })

      power_state <- shiny::reactive({
        get_demo_power_state(
          mean_difference = input$mean_difference,
          population_sd = input$population_sd,
          n = input$n,
          alpha = input$alpha,
          alternative = input$alternative
        )
      })

      output$power_plot <- shiny::renderPlot({
        plot_demo_power_state(power_state())
      })

      output$power_summary_text <- shiny::renderText({
        state <- power_state()
        critical_label <- if (length(state$critical_values) == 2L) {
          paste0(
            "[",
            formatC(state$critical_values[1], digits = 3, format = "f"),
            ", ",
            formatC(state$critical_values[2], digits = 3, format = "f"),
            "]"
          )
        } else {
          formatC(state$critical_values[1], digits = 3, format = "f")
        }

        paste0(
          "Alternative = ", format_demo_null_alternative(state$alternative),
          "\nEffect size (mean difference / SD) = ",
          formatC(state$effect_size, digits = 3, format = "f"),
          "\nNon-central shift on the test-statistic scale = ",
          formatC(state$noncentrality, digits = 3, format = "f"),
          "\nCritical value(s) = ", critical_label,
          "\nType I error = ", formatC(state$type1_error, digits = 3, format = "f"),
          "\nType II error = ", formatC(state$type2_error, digits = 3, format = "f"),
          "\nPower = ", formatC(state$power, digits = 3, format = "f")
        )
      })
    }
  )
}




#' @keywords internal
get_demo_power_state <- function(mean_difference,
                                 population_sd,
                                 n,
                                 alpha,
                                 alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  validate_demo_power_inputs(
    mean_difference = mean_difference,
    population_sd = population_sd,
    n = n,
    alpha = alpha
  )

  effect_size <- mean_difference / population_sd
  noncentrality <- effect_size * sqrt(n)
  critical_values <- compute_demo_power_critical_values(alpha, alternative)
  metrics <- compute_demo_power_metrics(
    noncentrality = noncentrality,
    alpha = alpha,
    alternative = alternative
  )

  x_limits <- get_demo_power_limits(noncentrality, critical_values)
  x_grid <- seq(from = x_limits[1], to = x_limits[2], length.out = 1501L)

  list(
    mean_difference = mean_difference,
    population_sd = population_sd,
    n = n,
    alpha = alpha,
    alternative = alternative,
    effect_size = effect_size,
    noncentrality = noncentrality,
    critical_values = critical_values,
    type1_error = metrics$type1_error,
    type2_error = metrics$type2_error,
    power = metrics$power,
    x_grid = x_grid,
    h0_density = stats::dnorm(x_grid, mean = 0, sd = 1),
    h1_density = stats::dnorm(x_grid, mean = noncentrality, sd = 1)
  )
}


#' @keywords internal
compute_demo_power_metrics <- function(noncentrality,
                                       alpha,
                                       alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  if (!is.numeric(noncentrality) || length(noncentrality) != 1L || is.na(noncentrality)) {
    stop("'noncentrality' should be a single non-missing number")
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' should be a single number between 0 and 1")
  }

  critical_values <- compute_demo_power_critical_values(alpha, alternative)

  if (identical(alternative, "two.sided")) {
    type2_error <- stats::pnorm(critical_values[2], mean = noncentrality, sd = 1) -
      stats::pnorm(critical_values[1], mean = noncentrality, sd = 1)
  } else if (identical(alternative, "greater")) {
    type2_error <- stats::pnorm(critical_values[1], mean = noncentrality, sd = 1)
  } else {
    type2_error <- 1 - stats::pnorm(critical_values[1], mean = noncentrality, sd = 1)
  }

  list(
    type1_error = alpha,
    type2_error = type2_error,
    power = 1 - type2_error
  )
}


#' @keywords internal
compute_demo_power_critical_values <- function(alpha,
                                               alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' should be a single number between 0 and 1")
  }

  if (identical(alternative, "two.sided")) {
    upper_value <- stats::qnorm(1 - alpha / 2)
    return(c(-upper_value, upper_value))
  }
  if (identical(alternative, "greater")) {
    return(stats::qnorm(1 - alpha))
  }

  stats::qnorm(alpha)
}


#' @keywords internal
plot_demo_power_state <- function(state) {
  y_limits <- c(0, max(c(state$h0_density, state$h1_density)) * 1.12)

  graphics::plot(
    x = state$x_grid,
    y = state$h0_density,
    type = "l",
    lwd = 3,
    col = "#1F77B4",
    ylim = y_limits,
    xlab = "Standardized test statistic",
    ylab = "Density",
    main = "Error rates and power on the test-statistic scale"
  )

  add_demo_power_shading(state)
  graphics::lines(state$x_grid, state$h0_density, lwd = 3, col = "#1F77B4")
  graphics::lines(state$x_grid, state$h1_density, lwd = 3, col = "#D55E00")

  for (critical_value in state$critical_values) {
    graphics::abline(v = critical_value, col = "#009E73", lty = 2, lwd = 2)
  }

  graphics::legend(
    "topright",
    legend = c("Distribution under H0", "Distribution under H1", "Critical value(s)", "Type I error", "Type II error", "Power"),
    col = c("#1F77B4", "#D55E00", "#009E73", "#56B4E9", "#F0E442", "#009E73"),
    lwd = c(3, 3, 2, NA, NA, NA),
    lty = c(1, 1, 2, NA, NA, NA),
    pch = c(NA, NA, NA, 15, 15, 15),
    pt.cex = c(NA, NA, NA, 2, 2, 2),
    bty = "n"
  )
  graphics::grid(col = "grey90")
}


#' @keywords internal
add_demo_power_shading <- function(state) {
  x_grid <- state$x_grid

  if (identical(state$alternative, "two.sided")) {
    add_demo_power_polygon(
      x_grid = x_grid,
      density_values = state$h0_density,
      keep = x_grid <= state$critical_values[1] | x_grid >= state$critical_values[2],
      fill = "#56B4E9"
    )
    add_demo_power_polygon(
      x_grid = x_grid,
      density_values = state$h1_density,
      keep = x_grid >= state$critical_values[1] & x_grid <= state$critical_values[2],
      fill = "#F0E442"
    )
    add_demo_power_polygon(
      x_grid = x_grid,
      density_values = state$h1_density,
      keep = x_grid <= state$critical_values[1] | x_grid >= state$critical_values[2],
      fill = "#009E73"
    )
    return(invisible(NULL))
  }

  if (identical(state$alternative, "greater")) {
    add_demo_power_polygon(
      x_grid = x_grid,
      density_values = state$h0_density,
      keep = x_grid >= state$critical_values[1],
      fill = "#56B4E9"
    )
    add_demo_power_polygon(
      x_grid = x_grid,
      density_values = state$h1_density,
      keep = x_grid <= state$critical_values[1],
      fill = "#F0E442"
    )
    add_demo_power_polygon(
      x_grid = x_grid,
      density_values = state$h1_density,
      keep = x_grid >= state$critical_values[1],
      fill = "#009E73"
    )
    return(invisible(NULL))
  }

  add_demo_power_polygon(
    x_grid = x_grid,
    density_values = state$h0_density,
    keep = x_grid <= state$critical_values[1],
    fill = "#56B4E9"
  )
  add_demo_power_polygon(
    x_grid = x_grid,
    density_values = state$h1_density,
    keep = x_grid >= state$critical_values[1],
    fill = "#F0E442"
  )
  add_demo_power_polygon(
    x_grid = x_grid,
    density_values = state$h1_density,
    keep = x_grid <= state$critical_values[1],
    fill = "#009E73"
  )
}


#' @keywords internal
add_demo_power_polygon <- function(x_grid, density_values, keep, fill) {
  if (!any(keep)) {
    return(invisible(NULL))
  }

  graphics::polygon(
    x = c(x_grid[keep][1], x_grid[keep], x_grid[keep][length(x_grid[keep])]),
    y = c(0, density_values[keep], 0),
    col = grDevices::adjustcolor(fill, alpha.f = 0.42),
    border = NA
  )
}


#' @keywords internal
get_demo_power_limits <- function(noncentrality, critical_values) {
  relevant_values <- c(-4, 4, noncentrality - 4, noncentrality + 4, critical_values)
  range_values <- range(relevant_values)
  padding <- 0.4
  c(range_values[1] - padding, range_values[2] + padding)
}


#' @keywords internal
validate_demo_power_inputs <- function(mean_difference,
                                       population_sd,
                                       n,
                                       alpha) {
  if (!is.numeric(mean_difference) || length(mean_difference) != 1L || is.na(mean_difference)) {
    stop("'mean_difference' should be a single non-missing number")
  }
  if (!is.numeric(population_sd) || length(population_sd) != 1L || is.na(population_sd) || population_sd <= 0) {
    stop("'population_sd' should be a positive number")
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 2 || n != as.integer(n)) {
    stop("'n' should be a positive whole number of at least 2")
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' should be a single number between 0 and 1")
  }

  invisible(TRUE)
}
