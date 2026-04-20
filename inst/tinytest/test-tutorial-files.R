tutorial_dir <- system.file("tutorials", package = "bootcamp")
if (!nzchar(tutorial_dir) && dir.exists("inst/tutorials")) {
  tutorial_dir <- "inst/tutorials"
}

expect_true(dir.exists(tutorial_dir))

tutorial_files <- list.files(
  tutorial_dir,
  pattern = "\\.Rmd$",
  recursive = TRUE,
  full.names = TRUE
)
tutorial_names <- basename(tutorial_files)
expected_tutorials <- c(
  "Day01_Hypotheses.Rmd",
  "Day02_Explore.Rmd",
  "Day03_GLM.Rmd",
  "Day04_Inference.Rmd"
)

expect_equal(sort(tutorial_names), sort(expected_tutorials))
expect_equal(length(unique(tutorial_names)), length(expected_tutorials))

for (tutorial_file in tutorial_files) {
  tutorial_lines <- readLines(tutorial_file, warn = FALSE)
  expect_true(length(tutorial_lines) > 20L)
  expect_equal(tutorial_lines[1], "---")
  expect_true(any(grepl("^title:\\s*\"", tutorial_lines)))
  expect_true(any(grepl("^description:\\s*\"", tutorial_lines)))
  expect_true(any(grepl("learnr::tutorial", tutorial_lines, fixed = TRUE)))
  expect_true(any(grepl("^runtime:\\s+shiny_prerendered$", tutorial_lines)))
  expect_true(any(grepl("source\\(\"\\.\\./R/helper_code\\.R\"\\)", tutorial_lines)))
  expect_true(any(grepl("exercise\\s*=\\s*TRUE", tutorial_lines)))
}

helper_code_path <- file.path(tutorial_dir, "R", "helper_code.R")
expect_true(file.exists(helper_code_path))

helper_code <- readLines(helper_code_path, warn = FALSE)
expect_true(any(grepl("^h_col <- function", helper_code)))
expect_true(any(grepl("^rproj <- function", helper_code)))
expect_true(any(grepl("^rstudio <- function", helper_code)))
expect_true(any(grepl("^smilebeam <- function", helper_code)))
