

#' Check for presence of packages
#'
#' Checks whether a set of packages is present and of the correct version
#'
#' Useful inside a learnr tutorial.
#'
#' \code{reqs} is a data.frame with column names "pkg", "version", "where".
#' All items are \code{character}!
#'
#' \describe{
#' \item{pkg}{names of the packages}
#' \item{version}{the minimally required version}
#' \item{where}{location of the package for download}
#' }
#'
#' For \code{where} there are two options. If the package resides on CRAN,
#' it should be "CRAN". If it is on github, it should be "username/reponame" (ie.
#' whatever would go inside \code{remotes::install_github(where)}).
#'
#' First, the function checks if a package is installed and, if installed, whether
#' it has the required version number or higher.
#' If eigher of these checks fail, it will attempt to download/upgrade the package.
#'
#' After a first pass along all packages (as described above), a second check is
#' performed if a package is now installed and, if installed, whether
#' it has the required version number or higher.
#'
#' If a package is still not installed, a message is returned that tells the user
#' how to install it manually.
#' If a package still does not at least have the required version,
#' a message is returned that tells the user how to upgrade it manually.
#'
#' @param reqs data.frame (see details)
#'
#' @return character string, summarizing the results
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' pkgs <- matrix(c(
#' "gradethis", "0.2.3.9001", "rstudio/gradethis",
#' "igraph", "1.2.6", "CRAN",
#' "influenceR", "0.1.0", "CRAN",
#' "intergraph", "2.0-2", "CRAN",
#' "network", "1.17.1", "CRAN",
#' "networkD3", "0.4", "CRAN",
#' "sna", "2.6", "CRAN",
#' "SNA4DSData", "0.9.9000", "SNAnalyst/SNA4DSData"
#' ), byrow = TRUE, ncol = 3) |>
#'   as.data.frame() |>
#'   setNames(c("pkg", "version", "where"))
#'
#' check_packages(pkgs)
#' }
check_packages <- function (reqs) {
  ok <- 0
  all_installed <- utils::installed.packages()
  cat("...checking individual packages now...\n")
  for (pak in 1:nrow(reqs)) {
    installed <- unname(which(all_installed[, "Package"] ==
                                reqs[pak, "pkg"]))
    if (length(installed) == 0) {
      if (reqs[pak, "where"] == "CRAN") {
        try(utils::install.packages(reqs[pak, "pkg"],
                                    dependencies = TRUE),
            silent = TRUE)
      }
      else {
        cat("...attempting to download a package from github...\n")
        try(remotes::install_github(reqs[pak, "where"]),
            silent = TRUE)
      }
    }
    else {
      if (utils::compareVersion(all_installed[installed, "Version"], reqs[pak, "version"]) < 0) {
        if (reqs[pak, "where"] == "CRAN") {
          try(utils::install.packages(reqs[pak, "pkg"],
                                      dependencies = TRUE),
              silent = TRUE)
        }
        else {
          cat("...attempting to download a package from github...\n")
          try(remotes::install_github(reqs[pak, "where"]),
              silent = TRUE)
        }
      }
      else {
        ok <- ok + 1
      }
    }
  }
  pkg_missing <- NULL
  pkg_low <- NULL
  if (ok == nrow(reqs)) {
    verdict <- "Wonderful, all is fine."
    return(verdict)
  }
  else {
    all_installed <- utils::installed.packages()
    for (pak in 1:nrow(reqs)) {
      installed <- unname(which(all_installed[, "Package"] ==
                                  reqs[pak, "pkg"]))
      if (length(installed) == 0) {
        pkg_missing <- c(pkg_missing, pak)
      }
      else {
        if (utils::compareVersion(all_installed[installed, "Version"], reqs[pak, "version"]) < 0) {
          pkg_low <- c(pkg_low, pak)
        }
      }
    }
  }
  if ((is.null(pkg_low)) & (is.null(pkg_missing))) {
    verdict <- "Deficiencies have been fixed, all is fine now."
    return(verdict)
  }
  verdict <- logical(0)
  if (!is.null(pkg_missing)) {
    names_missing <- paste0("'", reqs[pkg_missing, "pkg"],
                            "'", collapse = ", ")
    verdict <-
      c(c(
        verdict,
        paste0("The following packages are missing: ",
               names_missing)
      ), "Install these using:")
    reqs_missing <- reqs[pkg_missing,]
    reqs_missing_cran <- reqs_missing[reqs_missing[, "where"] ==
                                        "CRAN", "pkg", drop = TRUE]
    if (nrow(reqs_missing_cran) > 0) {
      verdict <-
        c(
          verdict,
          glue::glue("     install.packages('\"{package}\"')",
                     package = reqs_missing_cran)
        )
    }
    reqs_missing_github <- reqs_missing[reqs_missing[, "where"] !=
                                          "CRAN",]
    if (nrow(reqs_missing_github) > 0) {
      verdict <-
        c(c(
          verdict,
          glue::glue("     remotes::install_github('{location}')",
                     location = reqs_missing_github[, "where", drop = TRUE])
        ),
        "", "")
    }
  }
  if (!is.null(pkg_low)) {
    pkgs_low <- reqs[pkg_low, "pkg"]
    names_low <- paste0("'", pkgs_low, "'", collapse = ", ")
    verdict <-
      c(c(c(
        verdict,
        "",
        "",
        paste0("The version of the following packages is too low:",
               names_low)
      ), "Upgrade using:"), (
        glue::glue("      update.packages(\"{package}\")",
                   package = pkgs_low)
      ))
  }
  return(noquote(verdict))
}








#' Check for correct version of R or RStudio
#'
#' Check for correct version of R or RStudio
#'
#' Checks whether the user is using the correct version of RStudio or R
#'
#' The result is a logical (\code{TRUE} or \code{FALSE}), which is returned
#' invisibly.
#'
#' If required, a verdict can be printed to the console as well (when \code{verdict = TRUE})
#'
#' The functions \code{check_rstudio_equal} and  \code{check_r_equal} check
#' whether RStudio or R have the exact version asked for.
#'
#' The functions \code{check_rstudio_equal_or_larger} and \code{check_r_equal_or_larger} check
#' whether RStudio or R have at least the version asked for (havig a higher version is fine).
#'
#' @param version required version number to test against
#' @param verdict logical, whether a text with the verdict needs to be printed
#' to the console
#'
#' @return logical (invisibly) and, if asked for, a printed verdict
#' @keywords internal
#' @name version_check
NULL


#' @describeIn  version_check
#' @export
check_rstudio_equal <- function(version = "2023.6.1", verdict = TRUE) {
  ver <- rstudioapi::versionInfo()$version
  # limit to three levels
  ver_split <- strsplit(as.character(ver), ".", fixed = TRUE)[[1]][1:3]
  ver <- paste0(ver_split, collapse = ".")
  version_split <- strsplit(version, ".", fixed = TRUE)[[1]][1:3]
  version <- paste0(version_split, collapse = ".")

  if (ver == version) {
    if (verdict) cat("Your version of RStudio is perfectly fine")
    invisible(TRUE)
  } else {
    if (verdict) cat(paste0("You need to upgrade your RStudio version to ", version))
    invisible(FALSE)
  }
}


#' @describeIn  version_check
#' @export
check_r_equal <- function(version = "4.3.1", verdict = TRUE) {
  major_r <- R.Version()$major
  minor_r <- R.Version()$minor
  ver <- paste0(major_r, ".", minor_r)
  if (ver == version) {
    if (verdict) cat("Your version of R is exactly right")
    invisible(TRUE)
  } else {
    if (verdict) cat(paste0("You need to upgrade your R version to at least ", version))
    invisible(FALSE)
  }
}





#' @describeIn  version_check
#' @export
check_rstudio_equal_or_larger <- function(version = "2023.6.1.524", verdict = TRUE) {
  ver <- rstudioapi::versionInfo()$version
  if (utils::compareVersion(as.character(ver), version) >= 0) {
    if (verdict) cat("Your version of RStudio is fine")
    invisible(TRUE)
  } else {
    if (verdict) cat(paste0("You need to upgrade your RStudio version to at least ", version))
    invisible(FALSE)
  }
}


#' @describeIn  version_check
#' @export
check_r_equal_or_larger <- function(version = "4.3.1", verdict = TRUE) {
  major_r <- R.Version()$major
  minor_r <- R.Version()$minor
  ver <- paste0(major_r, ".", minor_r)
  if (utils::compareVersion(ver, version) >= 0) {
    if (verdict) cat("Your version of R is fine")
    invisible(TRUE)
  } else {
    if (verdict) cat(paste0("You need to upgrade your R version to at least ", version))
    invisible(FALSE)
  }
}
