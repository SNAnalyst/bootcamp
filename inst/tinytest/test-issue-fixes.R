regression_partial <- bootcamp::partial_corr_regression(mtcars[, 1:4])
inverse_partial <- bootcamp::partial_corr(mtcars[, 1:4])
expect_equal(regression_partial, inverse_partial, tolerance = 1e-6)

rank_partial <- bootcamp::partial_corr_regression(mtcars[, 1:4], method = "spearman")
expect_equal(dim(rank_partial), c(4, 4))
expect_true(all(diag(rank_partial) == 1))

expect_error(
  bootcamp::partial_corr_regression(mtcars[, 1:4], method = "kendall"),
  pattern = "pearson' and 'spearman"
)

corr_order <- getFromNamespace("get_corrmat_display_order", "bootcamp")
identity_order <- corr_order(stats::cor(mtcars[, 1:4]), clust = FALSE)
expect_equal(identity_order, 1:4)

formatted_corr <- getFromNamespace("format_betterpairs_correlation", "bootcamp")(
  x = 1:5,
  y = 5:1,
  digits = 2
)
expect_true(grepl("-", formatted_corr$label, fixed = TRUE))
expect_true(formatted_corr$correlation < 0)

long_formula_data <- as.data.frame(matrix(stats::rnorm(4000), ncol = 40))
colnames(long_formula_data) <- paste0("very_long_predictor_name_", sprintf("%03d", seq_len(ncol(long_formula_data))))
long_formula <- stats::as.formula(
  paste(colnames(long_formula_data)[1], "~", paste(colnames(long_formula_data)[-1], collapse = " + "))
)
long_fit <- stats::lm(long_formula, data = long_formula_data)
compare_result <- bootcamp::compareLM(long_fit)

expect_equal(nrow(compare_result$Models), 1)
expect_true(nchar(compare_result[[1]][1]) > 500)
