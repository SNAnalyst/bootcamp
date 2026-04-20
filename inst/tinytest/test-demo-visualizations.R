create_demo_OLS_data <- getFromNamespace("create_demo_OLS_data", "bootcamp")
nearest_demo_OLS_point <- getFromNamespace("nearest_demo_OLS_point", "bootcamp")
move_demo_OLS_point <- getFromNamespace("move_demo_OLS_point", "bootcamp")
add_demo_OLS_point <- getFromNamespace("add_demo_OLS_point", "bootcamp")
delete_demo_OLS_point <- getFromNamespace("delete_demo_OLS_point", "bootcamp")
add_demo_OLS_influential_point <- getFromNamespace("add_demo_OLS_influential_point", "bootcamp")
summarize_demo_OLS_fit <- getFromNamespace("summarize_demo_OLS_fit", "bootcamp")
build_demo_OLS_app <- getFromNamespace("build_demo_OLS_app", "bootcamp")

compute_mean_confidence_interval <- getFromNamespace("compute_mean_confidence_interval", "bootcamp")
compute_proportion_confidence_interval <- getFromNamespace("compute_proportion_confidence_interval", "bootcamp")
simulate_demo_confidence <- getFromNamespace("simulate_demo_confidence", "bootcamp")
build_demo_confidence_app <- getFromNamespace("build_demo_confidence_app", "bootcamp")

ols_data <- create_demo_OLS_data()
expect_equal(nrow(ols_data), 12L)
expect_true(all(c("point_id", "x", "y") %in% colnames(ols_data)))

nearest_index <- nearest_demo_OLS_point(ols_data, x = 1.1, y = 2.5)
expect_equal(nearest_index, 1L)

moved_data <- move_demo_OLS_point(ols_data, index = 1L, new_x = 20, new_y = 30)
expect_equal(moved_data$x[1], 20)
expect_equal(moved_data$y[1], 30)

expanded_data <- add_demo_OLS_point(ols_data, new_x = 15, new_y = -3)
expect_equal(nrow(expanded_data), 13L)
expect_equal(expanded_data$point_id[13], max(ols_data$point_id) + 1L)

reduced_data <- delete_demo_OLS_point(expanded_data, index = 13L)
expect_equal(nrow(reduced_data), nrow(ols_data))

influential_data <- add_demo_OLS_influential_point(ols_data)
expect_equal(nrow(influential_data), nrow(ols_data) + 1L)
expect_true(tail(influential_data$x, 1) > max(ols_data$x))
expect_true(tail(influential_data$y, 1) > max(ols_data$y))

fit_summary <- summarize_demo_OLS_fit(ols_data)
expect_true(fit_summary$can_fit)
expect_true(is.numeric(fit_summary$slope))
expect_true(fit_summary$r_squared > 0)

flat_x_data <- data.frame(
  point_id = 1:3,
  x = c(1, 1, 1),
  y = c(2, 3, 4)
)
flat_fit <- summarize_demo_OLS_fit(flat_x_data)
expect_true(!flat_fit$can_fit)
expect_true(grepl("all x values are equal", flat_fit$slope_label))

expect_true(inherits(build_demo_OLS_app(), "shiny.appobj"))
expect_true(inherits(build_demo_confidence_app(), "shiny.appobj"))

mean_sample_small <- c(1, 2, 3, 4, 5)
mean_ci_small <- compute_mean_confidence_interval(mean_sample_small, conf_level = 0.95)
expect_true(mean_ci_small["lower"] < mean_ci_small["estimate"])
expect_true(mean_ci_small["upper"] > mean_ci_small["estimate"])

set.seed(123)
mean_ci_low_n <- compute_mean_confidence_interval(stats::rnorm(20, sd = 3), conf_level = 0.95)
set.seed(123)
mean_ci_high_n <- compute_mean_confidence_interval(stats::rnorm(200, sd = 3), conf_level = 0.95)
expect_true(
  (mean_ci_high_n["upper"] - mean_ci_high_n["lower"]) <
    (mean_ci_low_n["upper"] - mean_ci_low_n["lower"])
)

prop_ci <- compute_proportion_confidence_interval(successes = 30, n = 50, conf_level = 0.95)
expect_true(prop_ci["lower"] < prop_ci["estimate"])
expect_true(prop_ci["upper"] > prop_ci["estimate"])

set.seed(42)
mean_simulation <- simulate_demo_confidence(
  parameter_type = "mean",
  n = 40,
  conf_level = 0.95,
  repetitions = 60,
  true_mean = 2,
  population_sd = 4
)
expect_equal(mean_simulation$parameter_type, "mean")
expect_equal(length(mean_simulation$single_sample), 40L)
expect_equal(nrow(mean_simulation$repeated_intervals), 60L)
expect_true(all(c("iteration", "estimate", "lower", "upper", "covers") %in% colnames(mean_simulation$repeated_intervals)))

set.seed(42)
proportion_simulation <- simulate_demo_confidence(
  parameter_type = "proportion",
  n = 80,
  conf_level = 0.95,
  repetitions = 50,
  true_proportion = 0.35
)
expect_equal(proportion_simulation$parameter_type, "proportion")
expect_equal(length(proportion_simulation$single_sample), 80L)
expect_equal(nrow(proportion_simulation$repeated_intervals), 50L)
expect_true(all(proportion_simulation$repeated_intervals$lower >= 0))
expect_true(all(proportion_simulation$repeated_intervals$upper <= 1))

expect_error(
  compute_mean_confidence_interval(1, conf_level = 0.95),
  pattern = "at least two observations"
)
expect_error(
  simulate_demo_confidence(
    parameter_type = "mean",
    n = 20,
    conf_level = 1.2,
    repetitions = 10
  ),
  pattern = "between 0 and 1"
)
