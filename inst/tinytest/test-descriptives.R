constant_df <- data.frame(
  constant = rep(1, 10),
  varying = seq_len(10)
)

expect_silent(
  normal_res <- bootcamp::descriptives(
    constant_df,
    normal = TRUE,
    print = FALSE
  )
)
expect_true(all(c("normal.w", "normal.p") %in% colnames(normal_res)))
expect_true(is.na(normal_res["constant", "normal.w"]))
expect_true(is.na(normal_res["constant", "normal.p"]))
expect_true(!is.na(normal_res["varying", "normal.w"]))

complete_case_df <- data.frame(
  a = c(1, 2, NA),
  b = c(3, 4, 5)
)

complete_case_res <- bootcamp::descriptives(
  complete_case_df,
  complete_cases = TRUE,
  print = FALSE
)
expect_equal(unname(complete_case_res[, "n_valid"]), c(2, 2))
expect_equal(unname(complete_case_res[, "n_na"]), c(0, 0))

quantile_res <- bootcamp::descriptives(
  complete_case_df,
  quantiles = 0.5,
  print = FALSE
)
expect_true("Q0.5" %in% colnames(quantile_res))
