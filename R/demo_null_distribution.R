#' Interactive null distribution demo
#'
#' Explore how p-values change across one-sided and two-sided tests
#'
#' This interactive demo launches a \code{shiny} application that visualizes a
#' null distribution together with an observed test statistic. Students can
#' switch between one-sided and two-sided hypotheses and immediately see how the
#' shaded p-value region changes.
#'
#' The app supports both a standard normal distribution and a Student's
#' \eqn{t}-distribution. For the \eqn{t}-distribution, the degrees of freedom can
#' be changed to show how the heavier tails affect p-values and critical values.
#'
#' A dedicated button flips the sign of the observed test statistic. This makes
#' it easier to demonstrate that one-sided tests depend on the predicted
#' direction of the effect.
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
#' bootcamp::demo_null_distribution()
#' }
demo_null_distribution <- function(launch.browser = TRUE) {
  if (!base::is.logical(launch.browser) || length(launch.browser) != 1L || is.na(launch.browser)) {
    stop("'launch.browser' should be TRUE or FALSE")
  }
  if (!base::requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with utils::install.packages('shiny').")
  }

  shiny::runApp(
    appObj = build_demo_null_distribution_app(),
    launch.browser = launch.browser
  )
}




#' @keywords internal
build_demo_null_distribution_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Null distributions and p-values"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::tags$p(
            "Move the observed test statistic and switch between one-sided and ",
            "two-sided alternatives to see how the shaded p-value changes."
          ),
          shiny::selectInput(
            inputId = "distribution",
            label = "Reference distribution",
            choices = c(
              "Standard normal (z)" = "z",
              "Student t" = "t"
            ),
            selected = "z"
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
            inputId = "observed_statistic",
            label = "Observed test statistic",
            min = -4,
            max = 4,
            value = 2,
            step = 0.05
          ),
          shiny::sliderInput(
            inputId = "alpha",
            label = "Significance level (alpha)",
            min = 0.01,
            max = 0.20,
            value = 0.05,
            step = 0.01
          ),
          shiny::conditionalPanel(
            condition = "input.distribution == 't'",
            shiny::sliderInput(
              inputId = "degrees_freedom",
              label = "Degrees of freedom",
              min = 2,
              max = 120,
              value = 20,
              step = 1
            )
          ),
          shiny::actionButton("flip_direction", "Flip effect direction"),
          shiny::tags$hr(),
          shiny::verbatimTextOutput("null_distribution_summary")
        ),
        shiny::mainPanel(
          shiny::plotOutput("null_distribution_plot", height = "560px")
        )
      )
    ),
    server = function(input, output, session) {
      shiny::observeEvent(input$flip_direction, {
        shiny::updateSliderInput(
          session = session,
          inputId = "observed_statistic",
          value = -input$observed_statistic
        )
      })

      null_state <- shiny::reactive({
        get_demo_null_distribution_state(
          observed_statistic = input$observed_statistic,
          alpha = input$alpha,
          alternative = input$alternative,
          distribution = input$distribution,
          degrees_freedom = get_demo_null_input_value(input$degrees_freedom, 20)
        )
      })

      output$null_distribution_plot <- shiny::renderPlot({
        plot_demo_null_distribution(null_state())
      })

      output$null_distribution_summary <- shiny::renderText({
        state <- null_state()
        critical_values <- state$critical_values
        critical_label <- if (length(critical_values) == 2L) {
          paste0(
            "[",
            formatC(critical_values[1], digits = 3, format = "f"),
            ", ",
            formatC(critical_values[2], digits = 3, format = "f"),
            "]"
          )
        } else {
          formatC(critical_values[1], digits = 3, format = "f")
        }

        paste0(
          "Observed test statistic = ",
          formatC(state$observed_statistic, digits = 3, format = "f"),
          "\nAlternative = ",
          format_demo_null_alternative(state$alternative),
          "\nP-value = ",
          formatC(state$p_value, digits = 4, format = "f"),
          "\nCritical value(s) for alpha = ",
          formatC(state$alpha, digits = 2, format = "f"),
          ": ",
          critical_label
        )
      })
    }
  )
}




#' @keywords internal
get_demo_null_distribution_state <- function(observed_statistic,
                                             alpha,
                                             alternative = c("two.sided", "greater", "less"),
                                             distribution = c("z", "t"),
                                             degrees_freedom = 20) {
  alternative <- match.arg(alternative)
  distribution <- match.arg(distribution)
  validate_demo_null_inputs(
    observed_statistic = observed_statistic,
    alpha = alpha,
    distribution = distribution,
    degrees_freedom = degrees_freedom
  )

  x_grid <- seq(from = -5, to = 5, length.out = 1201L)
  density_values <- get_demo_null_density(x_grid, distribution, degrees_freedom)

  list(
    observed_statistic = observed_statistic,
    alpha = alpha,
    alternative = alternative,
    distribution = distribution,
    degrees_freedom = degrees_freedom,
    x_grid = x_grid,
    density_values = density_values,
    p_value = compute_demo_null_p_value(
      observed_statistic = observed_statistic,
      alternative = alternative,
      distribution = distribution,
      degrees_freedom = degrees_freedom
    ),
    critical_values = compute_demo_null_critical_values(
      alpha = alpha,
      alternative = alternative,
      distribution = distribution,
      degrees_freedom = degrees_freedom
    )
  )
}


#' @keywords internal
compute_demo_null_p_value <- function(observed_statistic,
                                      alternative = c("two.sided", "greater", "less"),
                                      distribution = c("z", "t"),
                                      degrees_freedom = 20) {
  alternative <- match.arg(alternative)
  distribution <- match.arg(distribution)
  validate_demo_null_inputs(
    observed_statistic = observed_statistic,
    alpha = 0.05,
    distribution = distribution,
    degrees_freedom = degrees_freedom
  )

  tail_probability <- get_demo_null_tail_probability(
    q = observed_statistic,
    distribution = distribution,
    degrees_freedom = degrees_freedom
  )

  if (identical(alternative, "two.sided")) {
    return(2 * min(tail_probability, 1 - tail_probability))
  }
  if (identical(alternative, "greater")) {
    return(1 - tail_probability)
  }

  tail_probability
}


#' @keywords internal
compute_demo_null_critical_values <- function(alpha,
                                              alternative = c("two.sided", "greater", "less"),
                                              distribution = c("z", "t"),
                                              degrees_freedom = 20) {
  alternative <- match.arg(alternative)
  distribution <- match.arg(distribution)
  validate_demo_null_inputs(
    observed_statistic = 0,
    alpha = alpha,
    distribution = distribution,
    degrees_freedom = degrees_freedom
  )

  if (identical(alternative, "two.sided")) {
    upper_value <- get_demo_null_quantile(
      p = 1 - alpha / 2,
      distribution = distribution,
      degrees_freedom = degrees_freedom
    )
    return(c(-upper_value, upper_value))
  }

  critical_probability <- if (identical(alternative, "greater")) 1 - alpha else alpha
  get_demo_null_quantile(
    p = critical_probability,
    distribution = distribution,
    degrees_freedom = degrees_freedom
  )
}


#' @keywords internal
plot_demo_null_distribution <- function(state) {
  graphics::plot(
    x = state$x_grid,
    y = state$density_values,
    type = "l",
    lwd = 3,
    col = "#1F77B4",
    xlab = "Test statistic",
    ylab = "Density",
    main = "The p-value is the shaded tail area under the null"
  )

  add_demo_null_shading(state)
  graphics::lines(state$x_grid, state$density_values, lwd = 3, col = "#1F77B4")
  graphics::abline(v = state$observed_statistic, col = "#D55E00", lwd = 2)

  for (critical_value in state$critical_values) {
    graphics::abline(v = critical_value, col = "#009E73", lty = 2, lwd = 2)
  }

  graphics::legend(
    "topright",
    legend = c("Null distribution", "Observed statistic", "Critical value(s)", "P-value region"),
    col = c("#1F77B4", "#D55E00", "#009E73", "#56B4E9"),
    lwd = c(3, 2, 2, NA),
    lty = c(1, 1, 2, NA),
    pch = c(NA, NA, NA, 15),
    pt.cex = c(NA, NA, NA, 2),
    bty = "n"
  )
  graphics::grid(col = "grey90")
}


#' @keywords internal
add_demo_null_shading <- function(state) {
  x_grid <- state$x_grid
  density_values <- state$density_values

  if (identical(state$alternative, "two.sided")) {
    add_demo_null_polygon(
      x_grid = x_grid,
      density_values = density_values,
      keep = x_grid <= -abs(state$observed_statistic)
    )
    add_demo_null_polygon(
      x_grid = x_grid,
      density_values = density_values,
      keep = x_grid >= abs(state$observed_statistic)
    )
    return(invisible(NULL))
  }

  if (identical(state$alternative, "greater")) {
    add_demo_null_polygon(
      x_grid = x_grid,
      density_values = density_values,
      keep = x_grid >= state$observed_statistic
    )
    return(invisible(NULL))
  }

  add_demo_null_polygon(
    x_grid = x_grid,
    density_values = density_values,
    keep = x_grid <= state$observed_statistic
  )
}


#' @keywords internal
add_demo_null_polygon <- function(x_grid, density_values, keep) {
  if (!any(keep)) {
    return(invisible(NULL))
  }

  graphics::polygon(
    x = c(x_grid[keep][1], x_grid[keep], x_grid[keep][length(x_grid[keep])]),
    y = c(0, density_values[keep], 0),
    col = grDevices::adjustcolor("#56B4E9", alpha.f = 0.55),
    border = NA
  )
}


#' @keywords internal
get_demo_null_density <- function(x, distribution = c("z", "t"), degrees_freedom = 20) {
  distribution <- match.arg(distribution)
  if (identical(distribution, "z")) {
    return(stats::dnorm(x))
  }

  stats::dt(x, df = degrees_freedom)
}


#' @keywords internal
get_demo_null_tail_probability <- function(q, distribution = c("z", "t"), degrees_freedom = 20) {
  distribution <- match.arg(distribution)
  if (identical(distribution, "z")) {
    return(stats::pnorm(q))
  }

  stats::pt(q, df = degrees_freedom)
}


#' @keywords internal
get_demo_null_quantile <- function(p, distribution = c("z", "t"), degrees_freedom = 20) {
  distribution <- match.arg(distribution)
  if (identical(distribution, "z")) {
    return(stats::qnorm(p))
  }

  stats::qt(p, df = degrees_freedom)
}


#' @keywords internal
format_demo_null_alternative <- function(alternative) {
  if (identical(alternative, "two.sided")) {
    return("Two-sided")
  }
  if (identical(alternative, "greater")) {
    return("One-sided (greater)")
  }

  "One-sided (less)"
}


#' @keywords internal
get_demo_null_input_value <- function(value, default) {
  if (is.null(value) || length(value) == 0L || is.na(value)) {
    return(default)
  }

  value
}


#' @keywords internal
validate_demo_null_inputs <- function(observed_statistic,
                                      alpha,
                                      distribution,
                                      degrees_freedom) {
  if (!is.numeric(observed_statistic) || length(observed_statistic) != 1L || is.na(observed_statistic)) {
    stop("'observed_statistic' should be a single non-missing number")
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' should be a single number between 0 and 1")
  }
  if (!distribution %in% c("z", "t")) {
    stop("'distribution' should be either 'z' or 't'")
  }
  if (identical(distribution, "t")) {
    if (!is.numeric(degrees_freedom) || length(degrees_freedom) != 1L || is.na(degrees_freedom) ||
        degrees_freedom < 1 || degrees_freedom != as.integer(degrees_freedom)) {
      stop("'degrees_freedom' should be a positive whole number")
    }
  }

  invisible(TRUE)
}
