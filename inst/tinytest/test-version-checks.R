expect_true(
  bootcamp::check_r_equal_or_larger(version = "4.0.0", verdict = FALSE)
)
expect_false(
  bootcamp::check_r_equal_or_larger(version = "99.0.0", verdict = FALSE)
)
