build_demo_null_distribution_app <- getFromNamespace("build_demo_null_distribution_app", "bootcamp")
compute_demo_null_p_value <- getFromNamespace("compute_demo_null_p_value", "bootcamp")
compute_demo_null_critical_values <- getFromNamespace("compute_demo_null_critical_values", "bootcamp")
get_demo_null_distribution_state <- getFromNamespace("get_demo_null_distribution_state", "bootcamp")

build_demo_power_app <- getFromNamespace("build_demo_power_app", "bootcamp")
compute_demo_power_metrics <- getFromNamespace("compute_demo_power_metrics", "bootcamp")
compute_demo_power_critical_values <- getFromNamespace("compute_demo_power_critical_values", "bootcamp")
get_demo_power_state <- getFromNamespace("get_demo_power_state", "bootcamp")

expect_true(inherits(build_demo_null_distribution_app(), "shiny.appobj"))
expect_true(inherits(build_demo_power_app(), "shiny.appobj"))

two_sided_p <- compute_demo_null_p_value(
  observed_statistic = 2,
  alternative = "two.sided",
  distribution = "z"
)
greater_p <- compute_demo_null_p_value(
  observed_statistic = 2,
  alternative = "greater",
  distribution = "z"
)
less_p <- compute_demo_null_p_value(
  observed_statistic = 2,
  alternative = "less",
  distribution = "z"
)

expect_true(greater_p < two_sided_p)
expect_true(less_p > 0.9)
expect_equal(two_sided_p, 2 * greater_p, tolerance = 1e-8)

z_critical <- compute_demo_null_critical_values(
  alpha = 0.05,
  alternative = "two.sided",
  distribution = "z"
)
expect_equal(round(z_critical, 2), c(-1.96, 1.96))

t_critical <- compute_demo_null_critical_values(
  alpha = 0.05,
  alternative = "greater",
  distribution = "t",
  degrees_freedom = 10
)
expect_true(t_critical > 1.7)

null_state <- get_demo_null_distribution_state(
  observed_statistic = -1.5,
  alpha = 0.05,
  alternative = "less",
  distribution = "t",
  degrees_freedom = 15
)
expect_equal(null_state$alternative, "less")
expect_equal(length(null_state$x_grid), 1201L)
expect_true(null_state$p_value < 0.1)

expect_error(
  compute_demo_null_p_value(
    observed_statistic = 2,
    alternative = "greater",
    distribution = "t",
    degrees_freedom = 0
  ),
  pattern = "positive whole number"
)

critical_two_sided <- compute_demo_power_critical_values(alpha = 0.05, alternative = "two.sided")
expect_equal(round(critical_two_sided, 2), c(-1.96, 1.96))

zero_effect_metrics <- compute_demo_power_metrics(
  noncentrality = 0,
  alpha = 0.05,
  alternative = "greater"
)
expect_equal(zero_effect_metrics$type1_error, 0.05, tolerance = 1e-10)
expect_equal(zero_effect_metrics$power, 0.05, tolerance = 1e-10)
expect_equal(zero_effect_metrics$type2_error, 0.95, tolerance = 1e-10)

low_power_state <- get_demo_power_state(
  mean_difference = 0.4,
  population_sd = 2,
  n = 20,
  alpha = 0.05,
  alternative = "greater"
)
high_power_state <- get_demo_power_state(
  mean_difference = 0.4,
  population_sd = 2,
  n = 120,
  alpha = 0.05,
  alternative = "greater"
)
expect_true(high_power_state$power > low_power_state$power)
expect_true(high_power_state$type2_error < low_power_state$type2_error)

low_sd_state <- get_demo_power_state(
  mean_difference = 0.6,
  population_sd = 1,
  n = 50,
  alpha = 0.05,
  alternative = "greater"
)
high_sd_state <- get_demo_power_state(
  mean_difference = 0.6,
  population_sd = 3,
  n = 50,
  alpha = 0.05,
  alternative = "greater"
)
expect_true(low_sd_state$power > high_sd_state$power)

strict_alpha_state <- get_demo_power_state(
  mean_difference = 0.6,
  population_sd = 1.5,
  n = 50,
  alpha = 0.01,
  alternative = "two.sided"
)
loose_alpha_state <- get_demo_power_state(
  mean_difference = 0.6,
  population_sd = 1.5,
  n = 50,
  alpha = 0.10,
  alternative = "two.sided"
)
expect_true(loose_alpha_state$power > strict_alpha_state$power)

expect_error(
  get_demo_power_state(
    mean_difference = 0.5,
    population_sd = 0,
    n = 40,
    alpha = 0.05,
    alternative = "greater"
  ),
  pattern = "positive number"
)
