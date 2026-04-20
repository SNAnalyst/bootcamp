cor_df <- as.data.frame(stats::cor(mtcars[, 1:4]))
expected_partial <- getFromNamespace("partial_from_cor", "bootcamp")(cor_df)
actual_partial <- bootcamp::partial_corr(cor_df)

expect_equal(actual_partial, expected_partial, tolerance = 1e-8)
