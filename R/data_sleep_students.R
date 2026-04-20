#' Nightly sleep in first year of college
#'
#' This data set includes data for 284 first year college students.
#' 
#' The data were used for analyzing the relationship between
#' academic achievement in the first year of college and the students' sleep.
#' This dataset includes cohorts 3 and 4 from the original data set and most of 
#' the variables. Two cases have been deleted, these had missings for \code{Gender}
#' and \code{Race}.
#'
#' @format A data frame with 284 observations on the following 7 variables.
#' \describe{
#'   \item{TermGPA}{GPA for the Spring term}
#'   \item{TotalSleepTime}{Total sleep per day (in minutes)}
#'   \item{CumGPA}{prior cumulative GPA}
#'   \item{DaytimeSleep}{Total sleep per day (in minutes)}
#'   \item{Gender}{Gender, dichotomous (exact coding not reported in the study)}
#'   \item{Race}{binary label for underrepresented (0) and non-underrepresented 
#'   (1) students. 
#'   Students were considered underrepresented if either parent was Black, 
#'   Hispanic or Latino, Native American, or Pacific Islander. 
#'   Students were non-underrepresented if neither parent was from an 
#'   underrepresented category (i.e., both parents had White and/or Asian ancestry)}
#'   \item{cohort}{binary, 1 for cohort "uw2" and 0 for cohort "nh"}
#' }
#' @references Creswell, et al. (2023). Nightly sleep duration predicts grade 
#' point average in the first year of college. Proceedings of the National 
#' Academy of Sciences, 120(8), e2209123120.

#' @keywords datasets
#' @name sleep_students
#' @docType data
NULL
