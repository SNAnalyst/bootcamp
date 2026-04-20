


#' Check bootcamp version
#'
#' Check the installed bootcamp version
#'
#' With this function you can check if you have the most recent version of
#' the bootcamp package installed on GitHub.
#'
#' If you are current, the function returns a message that tells you so.
#' If there is a more recent version on Github, the function will inform you
#' that an update is available. It will also offer you the option to update
#' the package by simply pressing the "1" or "Y" key.
#'
#' The function checks the repository configured for this package:
#' \code{SNAnalyst/bootcamp} on the \code{main} branch.
#'
#' @return nothing relevant, this function is useful for its side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' check_bootcamp()
#' }
check_bootcamp <- function() {
  check_and_update_github("SNAnalyst/bootcamp")
}





check_and_update_github <- function(pkg) {
  check <- check_github(pkg = pkg)

  if (isTRUE(check$check_failed)) {
    message(check$message)
    return(invisible(NA))
  }

  if (is.na(check$installed_version)) {
    base::print(base::paste0("The ", check$package, " package is not installed\n"))
    choice <- utils::menu(c("Y", "N"), title = base::paste("Do you want me to install", pkg, "?"))
    if (choice == 1) {
      remotes::install_github(repo = pkg, dependencies = TRUE)
    }
    invisible(NA)
  } else if (check$up_to_date) {
    base::print(base::paste0("The installed ", check$package, " package is up-to-date"))
    invisible(TRUE)
  } else {
    base::print(base::paste0("You do not have the latest version of the ", check$package, " package."))

    choice <- utils::menu(c("Y", "N"), title = "Do you want me to update bootcamp?")
    if (choice == 1) {
      remotes::install_github(repo = pkg, dependencies = TRUE)
    }
    invisible(FALSE)
  }
}





check_github <- function(pkg) {
  package_name <- sub(".*/", "", pkg)
  installed_version <- tryCatch(utils::packageVersion(package_name), error = function(e) NA)

  url <- paste0("https://raw.githubusercontent.com/", pkg, "/main/DESCRIPTION")

  x <- tryCatch(base::readLines(url, warn = FALSE), error = function(e) NULL)
  if (is.null(x)) {
    return(list(
      package = package_name,
      installed_version = installed_version,
      latest_version = NA,
      up_to_date = NA,
      check_failed = TRUE,
      message = base::paste0("Could not read the remote DESCRIPTION for '", pkg, "'.")
    ))
  }

  remote_version <- gsub("Version:\\s*", "", x[grep("^Version:", x)])
  remote_version <- tryCatch(utils::packageVersion(remote_version), error = function(e) NA)
  if (is.na(remote_version)) {
    return(list(
      package = package_name,
      installed_version = installed_version,
      latest_version = NA,
      up_to_date = NA,
      check_failed = TRUE,
      message = base::paste0("Could not parse the remote version for '", pkg, "'.")
    ))
  }

  res <- list(package = package_name,
              installed_version = installed_version,
              latest_version = remote_version,
              up_to_date = NA,
              check_failed = FALSE,
              message = NULL)

  if (is.na(installed_version)) {
    message(base::paste("##", package_name, "is not installed..."))
  } else {
    # Package versions need semantic comparison; plain string comparison breaks
    # on versions such as 0.10 versus 0.9.
    comparison <- utils::compareVersion(as.character(remote_version), as.character(installed_version))
    if (comparison > 0) {
      res$up_to_date <- FALSE
    } else if (comparison == 0) {
      res$up_to_date <- TRUE
    }
  }

  return(res)
}

