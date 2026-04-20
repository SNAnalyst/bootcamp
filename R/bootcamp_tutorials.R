#' Pick a tutorial
#'
#' Pick a tutorial from the bootcamp package
#'
#' Shows a list of currently available tutorials in the
#' \code{bootcamp} package. The user can pick the preferred tutorial by
#' entering the number that corresponds to the preferred tutorial. The
#' selected tutorial is then started with \code{learnr::run_tutorial()}.
#'
#' When \code{graphics} is \code{TRUE}, a graphical choice menu is shown. If that
#' is not preferred, or if the user's machine lacks the graphical tools needed,
#' setting \code{graphics} to \code{FALSE} will show the list of tutorials in the
#' R console.
#'
#' @param graphics logical, should the list of options be shown as a clickable
#' graphical menu?
#'
#' @return Invisibly returns the selected tutorial name. If the user cancels the
#' selection, \code{NULL} is returned invisibly.
#' @export
#' @examples
#' \dontrun{
#' bootcamp_tutorials()
#' }
bootcamp_tutorials <- function(graphics = TRUE) {
  if (!base::is.logical(graphics) || length(graphics) != 1L || base::is.na(graphics)) {
    stop("You need to set 'graphics' to TRUE or FALSE only (without parentheses)")
  }
  if (!base::requireNamespace("learnr", quietly = TRUE)) {
    stop("Package 'learnr' is required to run tutorials. Install it with utils::install.packages('learnr').")
  }

  all_tuts <- learnr::available_tutorials("bootcamp")
  if (base::nrow(all_tuts) == 0L) {
    stop("No tutorials are currently available in the 'bootcamp' package.")
  }

  base::cat("\n\nPlease pick which tutorial you want to run, it will open in your default browser.\n")
  base::cat("The following tutorials are currently available to pick from:\n")
  pick <- utils::menu(all_tuts$title, graphics = graphics)

  # `menu()` returns 0 when the user cancels; we keep that path quiet and explicit.
  if (pick == 0L) {
    return(invisible(NULL))
  }

  run_bootcamp_tutorial(all_tuts$name[pick])
  invisible(all_tuts$name[pick])
}




#' Run a bootcamp tutorial
#'
#' Internal helper used by \code{bootcamp_tutorials()} to keep the launching
#' logic in one place. The helper accepts an optional \code{runner} so the
#' behavior can be regression-tested without starting a real tutorial session.
#'
#' @param tutorial_name Character scalar with the tutorial identifier as returned
#' by \code{learnr::available_tutorials()}.
#' @param runner Function used to start the tutorial. When omitted, the helper
#' uses \code{learnr::run_tutorial()}.
#'
#' @return Invisibly returns \code{tutorial_name}.
#' @keywords internal
run_bootcamp_tutorial <- function(tutorial_name, runner = NULL) {
  if (!base::is.character(tutorial_name) || length(tutorial_name) != 1L || base::is.na(tutorial_name)) {
    stop("'tutorial_name' should be a single, non-missing character value.")
  }
  if (base::is.null(runner)) {
    runner <- learnr::run_tutorial
  }

  runner(name = tutorial_name, package = "bootcamp")
  invisible(tutorial_name)
}

