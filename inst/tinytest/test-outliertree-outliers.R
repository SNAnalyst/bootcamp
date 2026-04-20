# We capture the fitting output because outliertree prints a short report during
# model creation, which would otherwise add noise to the test output.
# tinytest injects the expectation helpers directly into the test environment.
# We therefore use the bare helper names here so the framework can register the
# results correctly.

set.seed(1)
demo_outlier_data <- data.frame(
  a = c(stats::rnorm(100), 10, 11),
  b = c(stats::rnorm(100), 10, 11),
  c = c(sample(c("x", "y"), 100, replace = TRUE), "x", "y"),
  stringsAsFactors = FALSE
)
invisible(utils::capture.output(
  saved_outlier_model <- outliertree::outlier.tree(demo_outlier_data, save_outliers = TRUE)
))

outlier_summary <- bootcamp::outliertree_outliers(saved_outlier_model)
expect_equal(outlier_summary$row, c(101L, 102L))
expect_equal(outlier_summary$suspicious_column, c("a", "a"))
expect_equal(outlier_summary$suspicious_value, c(10, 11))
expect_true(all(
  c("tree_depth", "uses_NA_branch", "outlier_score") %in% colnames(outlier_summary)
))

expect_equal(
  bootcamp::outliertree_outliers(saved_outlier_model, output = "rows"),
  c(101L, 102L)
)

expect_equal(
  bootcamp::outliertree_outliers(saved_outlier_model, which_data_row = 101L)$row,
  101L
)
expect_equal(
  bootcamp::outliertree_outliers(saved_outlier_model, which_number = 2L)$row,
  102L
)
expect_warning(
  bootcamp::outliertree_outliers(
    saved_outlier_model,
    which_data_row = c(101L, 200L)
  ),
  pattern = "were not flagged as outliers"
)
expect_equal(
  suppressWarnings(
    bootcamp::outliertree_outliers(saved_outlier_model, which_data_row = c(101L, 200L))$row
  ),
  101L
)

flagged_subset <- bootcamp::outliertree_outliers(
  saved_outlier_model,
  x = demo_outlier_data,
  output = "subset"
)
expect_equal(flagged_subset$.outlier_row, c(101L, 102L))
expect_equal(flagged_subset$a, c(10, 11))
expect_equal(nrow(flagged_subset), 2L)

flagged_subset_plain <- bootcamp::outliertree_outliers(
  saved_outlier_model,
  x = demo_outlier_data,
  output = "subset",
  keep_summary = FALSE
)
expect_equal(colnames(flagged_subset_plain), colnames(demo_outlier_data))
expect_equal(nrow(flagged_subset_plain), 2L)

printed_output <- utils::capture.output(
  printed_summary <- bootcamp::outliertree_outliers(
    saved_outlier_model,
    output = "print",
    which_number = 1L
  )
)
expect_equal(printed_summary$row, 101L)
expect_true(any(grepl("row \\[101\\]", printed_output)))
expect_true(any(grepl("suspicious column: \\[a\\]", printed_output)))

invisible(utils::capture.output(
  empty_outlier_model <- outliertree::outlier.tree(iris, save_outliers = TRUE)
))
empty_summary <- bootcamp::outliertree_outliers(empty_outlier_model)
expect_equal(nrow(empty_summary), 0L)
expect_equal(
  bootcamp::outliertree_outliers(empty_outlier_model, output = "rows"),
  integer(0)
)
expect_equal(
  nrow(bootcamp::outliertree_outliers(empty_outlier_model, x = iris, output = "subset")),
  0L
)

empty_print_message <- utils::capture.output(
  bootcamp::outliertree_outliers(empty_outlier_model, output = "print"),
  type = "message"
)
expect_true(any(grepl("No flagged outliers matched", empty_print_message)))

invisible(utils::capture.output(
  unsaved_outlier_model <- outliertree::outlier.tree(demo_outlier_data, save_outliers = FALSE)
))
expect_error(
  bootcamp::outliertree_outliers(unsaved_outlier_model),
  pattern = "save_outliers = TRUE"
)
expect_error(
  bootcamp::outliertree_outliers(saved_outlier_model, output = "subset"),
  pattern = "original training data"
)
expect_error(
  bootcamp::outliertree_outliers(saved_outlier_model, which_number = 3L),
  pattern = "beyond the number of detected outliers"
)
expect_error(
  bootcamp::outliertree_outliers(saved_outlier_model, which_data_row = 1.5),
  pattern = "positive whole numbers"
)
