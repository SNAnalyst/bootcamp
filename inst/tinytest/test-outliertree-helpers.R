set.seed(1)
demo_outlier_data <- data.frame(
  a = c(stats::rnorm(100), 10, 11),
  b = c(stats::rnorm(100), 10, 11),
  c = c(sample(c("x", "y"), 100, replace = TRUE), "x", "y"),
  stringsAsFactors = FALSE
)

saved_outlier_model <- outliertree::outlier.tree(demo_outlier_data, save_outliers = TRUE)
outlier_summary <- bootcamp::extract_outliertree_outliers(saved_outlier_model)

expect_equal(outlier_summary$row, c(101, 102))
expect_equal(outlier_summary$suspicious_column, c("a", "a"))
expect_equal(outlier_summary$suspicious_value, c(10, 11))
expect_true(all(c("tree_depth", "uses_NA_branch", "outlier_score") %in% colnames(outlier_summary)))

flagged_subset <- bootcamp::subset_outliertree_outliers(demo_outlier_data, saved_outlier_model)
expect_equal(flagged_subset$.outlier_row, c(101, 102))
expect_equal(flagged_subset$a, c(10, 11))
expect_equal(nrow(flagged_subset), 2)

flagged_subset_plain <- bootcamp::subset_outliertree_outliers(
  demo_outlier_data,
  saved_outlier_model,
  keep_summary = FALSE
)
expect_equal(colnames(flagged_subset_plain), colnames(demo_outlier_data))
expect_equal(nrow(flagged_subset_plain), 2)

empty_outlier_model <- outliertree::outlier.tree(iris, save_outliers = TRUE)
empty_summary <- bootcamp::extract_outliertree_outliers(empty_outlier_model)
expect_equal(nrow(empty_summary), 0)
expect_equal(nrow(bootcamp::subset_outliertree_outliers(iris, empty_outlier_model)), 0)

unsaved_outlier_model <- outliertree::outlier.tree(demo_outlier_data, save_outliers = FALSE)
expect_error(
  bootcamp::extract_outliertree_outliers(unsaved_outlier_model),
  pattern = "save_outliers = TRUE"
)
