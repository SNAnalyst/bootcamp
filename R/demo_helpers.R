#' Run a bootcamp Shiny demo
#'
#' Internal helper used by the exported demo functions to start a Shiny app in a
#' consistent and regression-testable way.
#'
#' @param app_builder Function returning a \code{shiny.appobj}.
#' @param launch.browser Logical, should the app open in a browser window?
#' @param runner Function used to launch the app. When omitted, the helper uses
#'   \code{shiny::runApp()}.
#'
#' @return The value returned by \code{runner()}.
#' @keywords internal
run_bootcamp_shiny_demo <- function(app_builder, launch.browser, runner = NULL) {
  if (!is.function(app_builder)) {
    stop("'app_builder' should be a function")
  }
  if (!is.logical(launch.browser) || length(launch.browser) != 1L || is.na(launch.browser)) {
    stop("'launch.browser' should be TRUE or FALSE")
  }
  if (is.null(runner)) {
    runner <- shiny::runApp
  }
  if (!is.function(runner)) {
    stop("'runner' should be a function")
  }

  runner(app_builder(), launch.browser = launch.browser)
}
