#' Interactive OLS demo
#'
#' Explore how point placement influences an ordinary least squares line
#'
#' This interactive demo launches a small \code{shiny} application in which
#' students can manipulate a simple two-variable dataset and immediately see how
#' the fitted ordinary least squares regression line changes. The app is meant
#' to build intuition for leverage, unusual observations, and why a single
#' influential point can noticeably change the estimated slope and intercept.
#'
#' The demo starts with a small synthetic dataset. Students can click the plot
#' to select a point, move the selected point, add a new point, or delete the
#' nearest point. A convenience button also adds an influential observation so
#' the effect of an extreme case can be demonstrated quickly during class.
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
#' bootcamp::demo_OLS()
#' }
demo_OLS <- function(launch.browser = TRUE) {
  if (!base::is.logical(launch.browser) || length(launch.browser) != 1L || is.na(launch.browser)) {
    stop("'launch.browser' should be TRUE or FALSE")
  }
  if (!base::requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install it with utils::install.packages('shiny').")
  }

  shiny::runApp(
    appObj = build_demo_OLS_app(),
    launch.browser = launch.browser
  )
}




#' @keywords internal
build_demo_OLS_app <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("OLS and influential observations"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::tags$p(
            "Click inside the plot to interact with the data. ",
            "Use the buttons below to select, move, add, or remove points."
          ),
          shiny::radioButtons(
            inputId = "point_action",
            label = "Click action",
            choices = c(
              "Select nearest point" = "select",
              "Move selected point" = "move",
              "Add a new point" = "add",
              "Delete nearest point" = "delete"
            ),
            selected = "select"
          ),
          shiny::actionButton("add_influential", "Add influential point"),
          shiny::actionButton("clear_selection", "Clear selection"),
          shiny::actionButton("reset_data", "Reset dataset"),
          shiny::tags$hr(),
          shiny::tableOutput("ols_fit_table"),
          shiny::verbatimTextOutput("ols_selection_text")
        ),
        shiny::mainPanel(
          shiny::plotOutput(
            outputId = "ols_plot",
            click = "ols_plot_click",
            height = "560px"
          )
        )
      )
    ),
    server = function(input, output, session) {
      demo_values <- shiny::reactiveValues(
        data = create_demo_OLS_data(),
        selected = NA_integer_
      )

      output$ols_plot <- shiny::renderPlot({
        plot_demo_OLS_state(
          data = demo_values$data,
          selected = demo_values$selected
        )
      })

      output$ols_fit_table <- shiny::renderTable({
        fit_summary <- summarize_demo_OLS_fit(demo_values$data)
        data.frame(
          metric = c("Observations", "Intercept", "Slope", "R-squared"),
          value = c(
            fit_summary$n,
            fit_summary$intercept_label,
            fit_summary$slope_label,
            fit_summary$r_squared_label
          ),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }, striped = TRUE, bordered = TRUE, digits = 3)

      output$ols_selection_text <- shiny::renderText({
        if (is.na(demo_values$selected)) {
          return("No point selected. Choose 'Select nearest point' and click the plot.")
        }

        selected_row <- demo_values$data[demo_values$selected, , drop = FALSE]
        paste0(
          "Selected point: #", selected_row$point_id,
          "\nCurrent x = ", formatC(selected_row$x, digits = 3, format = "f"),
          "\nCurrent y = ", formatC(selected_row$y, digits = 3, format = "f")
        )
      })

      shiny::observeEvent(input$ols_plot_click, {
        click <- input$ols_plot_click
        if (is.null(click)) {
          return()
        }

        if (identical(input$point_action, "select")) {
          demo_values$selected <- nearest_demo_OLS_point(demo_values$data, click$x, click$y)
          return()
        }

        if (identical(input$point_action, "move")) {
          if (is.na(demo_values$selected)) {
            shiny::showNotification(
              "Select a point first, then switch to 'Move selected point'.",
              type = "message"
            )
            return()
          }

          demo_values$data <- move_demo_OLS_point(
            data = demo_values$data,
            index = demo_values$selected,
            new_x = click$x,
            new_y = click$y
          )
          return()
        }

        if (identical(input$point_action, "add")) {
          demo_values$data <- add_demo_OLS_point(
            data = demo_values$data,
            new_x = click$x,
            new_y = click$y
          )
          demo_values$selected <- nrow(demo_values$data)
          return()
        }

        delete_index <- nearest_demo_OLS_point(demo_values$data, click$x, click$y)
        if (nrow(demo_values$data) <= 1L) {
          shiny::showNotification(
            "Keep at least one point in the dataset so you can continue the demo.",
            type = "message"
          )
          return()
        }
        demo_values$data <- delete_demo_OLS_point(demo_values$data, delete_index)

        if (nrow(demo_values$data) == 0L) {
          demo_values$selected <- NA_integer_
        } else if (!is.na(demo_values$selected)) {
          if (demo_values$selected == delete_index) {
            demo_values$selected <- NA_integer_
          } else if (demo_values$selected > delete_index) {
            demo_values$selected <- demo_values$selected - 1L
          }
        }
      })

      shiny::observeEvent(input$add_influential, {
        demo_values$data <- add_demo_OLS_influential_point(demo_values$data)
        demo_values$selected <- nrow(demo_values$data)
      })

      shiny::observeEvent(input$clear_selection, {
        demo_values$selected <- NA_integer_
      })

      shiny::observeEvent(input$reset_data, {
        demo_values$data <- create_demo_OLS_data()
        demo_values$selected <- NA_integer_
      })
    }
  )
}




#' @keywords internal
create_demo_OLS_data <- function() {
  data.frame(
    point_id = seq_len(12L),
    x = c(1.0, 1.8, 2.4, 3.2, 3.8, 4.5, 5.1, 5.8, 6.3, 6.9, 7.6, 8.2),
    y = c(2.4, 2.8, 3.7, 4.1, 4.9, 5.4, 5.9, 6.2, 6.8, 7.1, 7.8, 8.0),
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
nearest_demo_OLS_point <- function(data, x, y) {
  validate_demo_OLS_data(data)
  if (nrow(data) == 0L) {
    return(NA_integer_)
  }

  distances <- (data$x - x)^2 + (data$y - y)^2
  which.min(distances)
}


#' @keywords internal
move_demo_OLS_point <- function(data, index, new_x, new_y) {
  validate_demo_OLS_data(data)
  validate_demo_OLS_index(index, data)

  data$x[index] <- new_x
  data$y[index] <- new_y
  data
}


#' @keywords internal
add_demo_OLS_point <- function(data, new_x, new_y) {
  validate_demo_OLS_data(data)

  next_id <- if (nrow(data) == 0L) 1L else max(data$point_id) + 1L
  new_row <- data.frame(
    point_id = next_id,
    x = new_x,
    y = new_y,
    stringsAsFactors = FALSE
  )

  rbind(data, new_row)
}


#' @keywords internal
delete_demo_OLS_point <- function(data, index) {
  validate_demo_OLS_data(data)
  validate_demo_OLS_index(index, data)

  data[-index, , drop = FALSE]
}


#' @keywords internal
add_demo_OLS_influential_point <- function(data) {
  validate_demo_OLS_data(data)

  fit_summary <- summarize_demo_OLS_fit(data)
  x_range <- range(data$x)
  x_span <- diff(x_range)
  if (x_span == 0) {
    x_span <- 1
  }

  x_new <- x_range[2] + 1.4 * x_span
  y_shift <- max(stats::sd(data$y), 1)
  y_expected <- if (fit_summary$can_fit) {
    fit_summary$intercept + fit_summary$slope * x_new
  } else {
    mean(data$y)
  }

  # The goal is not realism but pedagogy: this point should sit far enough from
  # the existing cloud to change the fitted line immediately and visibly.
  add_demo_OLS_point(
    data = data,
    new_x = x_new,
    new_y = y_expected + 2.75 * y_shift
  )
}


#' @keywords internal
summarize_demo_OLS_fit <- function(data) {
  validate_demo_OLS_data(data)

  if (nrow(data) < 2L) {
    return(list(
      n = nrow(data),
      can_fit = FALSE,
      intercept = NA_real_,
      slope = NA_real_,
      r_squared = NA_real_,
      intercept_label = "Not available",
      slope_label = "Not available",
      r_squared_label = "Not available"
    ))
  }

  if (diff(range(data$x)) == 0) {
    return(list(
      n = nrow(data),
      can_fit = FALSE,
      intercept = NA_real_,
      slope = NA_real_,
      r_squared = NA_real_,
      intercept_label = "Undefined (all x values are equal)",
      slope_label = "Undefined (all x values are equal)",
      r_squared_label = "Not available"
    ))
  }

  fit <- stats::lm(y ~ x, data = data)
  fit_summary <- summary(fit)
  coefficients <- stats::coef(fit)

  list(
    n = nrow(data),
    can_fit = TRUE,
    intercept = unname(coefficients[1]),
    slope = unname(coefficients[2]),
    r_squared = unname(fit_summary$r.squared),
    intercept_label = formatC(unname(coefficients[1]), digits = 3, format = "f"),
    slope_label = formatC(unname(coefficients[2]), digits = 3, format = "f"),
    r_squared_label = formatC(unname(fit_summary$r.squared), digits = 3, format = "f")
  )
}


#' @keywords internal
plot_demo_OLS_state <- function(data, selected = NA_integer_) {
  validate_demo_OLS_data(data)

  plot_limits <- get_demo_OLS_plot_limits(data)
  if (nrow(data) == 0L) {
    graphics::plot(
      NA,
      xlim = plot_limits$x,
      ylim = plot_limits$y,
      xlab = "Explanatory variable (x)",
      ylab = "Dependent variable (y)",
      main = "Add points to begin the demo"
    )
    graphics::grid(col = "grey90")
    return(invisible(NULL))
  }

  fit_summary <- summarize_demo_OLS_fit(data)

  graphics::plot(
    data$x,
    data$y,
    xlim = plot_limits$x,
    ylim = plot_limits$y,
    xlab = "Explanatory variable (x)",
    ylab = "Dependent variable (y)",
    pch = 19,
    col = "#1F77B4",
    cex = 1.2,
    main = "Click to reshape the dataset"
  )

  if (!is.na(selected) && selected >= 1L && selected <= nrow(data)) {
    graphics::points(
      data$x[selected],
      data$y[selected],
      pch = 21,
      bg = "#D55E00",
      col = "#D55E00",
      cex = 2.2,
      lwd = 2
    )
  }

  if (fit_summary$can_fit) {
    graphics::abline(
      a = fit_summary$intercept,
      b = fit_summary$slope,
      col = "#D55E00",
      lwd = 3
    )
  }

  graphics::grid(col = "grey90")
}


#' @keywords internal
get_demo_OLS_plot_limits <- function(data) {
  validate_demo_OLS_data(data)
  if (nrow(data) == 0L) {
    return(list(x = c(0, 10), y = c(0, 10)))
  }

  x_range <- range(data$x)
  y_range <- range(data$y)

  x_pad <- max(diff(x_range) * 0.15, 1)
  y_pad <- max(diff(y_range) * 0.15, 1)

  list(
    x = c(x_range[1] - x_pad, x_range[2] + x_pad),
    y = c(y_range[1] - y_pad, y_range[2] + y_pad)
  )
}


#' @keywords internal
validate_demo_OLS_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("'data' should be a data.frame")
  }
  required_columns <- c("point_id", "x", "y")
  if (!all(required_columns %in% colnames(data))) {
    stop("'data' should contain the columns 'point_id', 'x', and 'y'")
  }
  if (!is.numeric(data$x) || !is.numeric(data$y)) {
    stop("'x' and 'y' should be numeric")
  }

  invisible(TRUE)
}


#' @keywords internal
validate_demo_OLS_index <- function(index, data) {
  if (!is.numeric(index) || length(index) != 1L || is.na(index) || index < 1 || index != as.integer(index)) {
    stop("'index' should be a single positive whole number")
  }
  if (index > nrow(data)) {
    stop("'index' is outside the available row range")
  }

  invisible(TRUE)
}
