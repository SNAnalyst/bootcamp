build_demo_sampling_app <- getFromNamespace("build_demo_sampling_app", "bootcamp")
create_demo_sampling_population <- getFromNamespace("create_demo_sampling_population", "bootcamp")
draw_demo_sample <- getFromNamespace("draw_demo_sample", "bootcamp")
compute_demo_sample_statistic <- getFromNamespace("compute_demo_sample_statistic", "bootcamp")
simulate_demo_sampling_distribution <- getFromNamespace("simulate_demo_sampling_distribution", "bootcamp")
summarize_demo_sampling_state <- getFromNamespace("summarize_demo_sampling_state", "bootcamp")

build_demo_residuals_app <- getFromNamespace("build_demo_residuals_app", "bootcamp")
simulate_demo_residual_data <- getFromNamespace("simulate_demo_residual_data", "bootcamp")
summarize_demo_residual_patterns <- getFromNamespace("summarize_demo_residual_patterns", "bootcamp")
get_demo_residual_state <- getFromNamespace("get_demo_residual_state", "bootcamp")

expect_true(inherits(build_demo_sampling_app(), "shiny.appobj"))
expect_true(inherits(build_demo_residuals_app(), "shiny.appobj"))

set.seed(100)
normal_population <- create_demo_sampling_population("normal", population_size = 3000L)
expect_equal(length(normal_population), 3000L)

set.seed(101)
skewed_population <- create_demo_sampling_population("right_skewed", population_size = 3000L)
expect_true(mean(skewed_population) > stats::median(skewed_population))

set.seed(102)
outlier_population <- create_demo_sampling_population("with_outliers", population_size = 3000L)
expect_true(max(outlier_population) > mean(outlier_population) + 4 * stats::sd(outlier_population))

set.seed(103)
demo_sample <- draw_demo_sample(normal_population, sample_size = 40L)
expect_equal(length(demo_sample), 40L)

expect_equal(compute_demo_sample_statistic(1:5, "mean"), 3)
expect_equal(compute_demo_sample_statistic(1:5, "median"), 3)

set.seed(104)
small_n_distribution <- simulate_demo_sampling_distribution(
  population = normal_population,
  sample_size = 20L,
  repetitions = 150L,
  statistic = "mean"
)
set.seed(104)
large_n_distribution <- simulate_demo_sampling_distribution(
  population = normal_population,
  sample_size = 120L,
  repetitions = 150L,
  statistic = "mean"
)
expect_equal(length(small_n_distribution$statistics), 150L)
expect_true(stats::sd(large_n_distribution$statistics) < stats::sd(small_n_distribution$statistics))

sampling_summary <- summarize_demo_sampling_state(
  population = normal_population,
  current_sample = demo_sample,
  sampled_statistics = small_n_distribution$statistics,
  statistic = "mean"
)
expect_equal(sampling_summary$n_repetitions, 150L)
expect_true(!is.na(sampling_summary$sampling_center))
expect_true(!is.na(sampling_summary$sampling_sd))

expect_error(
  draw_demo_sample(normal_population, sample_size = 5001L),
  pattern = "cannot be larger"
)

set.seed(200)
well_behaved_data <- simulate_demo_residual_data(
  scenario = "well_behaved",
  sample_size = 80L,
  noise_level = 0.8
)
expect_true(all(c("x", "y", "point_role") %in% colnames(well_behaved_data)))
expect_true(all(well_behaved_data$point_role == "regular"))

set.seed(201)
curvature_data <- simulate_demo_residual_data(
  scenario = "curvature",
  sample_size = 80L,
  noise_level = 0.5
)
curvature_summary <- summarize_demo_residual_patterns(curvature_data)
expect_true(curvature_summary$quadratic_r_squared_gain > 0.08)

set.seed(202)
hetero_data <- simulate_demo_residual_data(
  scenario = "heteroscedasticity",
  sample_size = 80L,
  noise_level = 0.8
)
hetero_summary <- summarize_demo_residual_patterns(hetero_data)
expect_true(hetero_summary$heteroscedasticity_metric > 0.25)

set.seed(203)
outlier_data <- simulate_demo_residual_data(
  scenario = "outlier",
  sample_size = 80L,
  noise_level = 0.8
)
outlier_summary <- summarize_demo_residual_patterns(outlier_data)
expect_true(any(outlier_data$point_role == "outlier"))
expect_true(outlier_summary$outlier_count >= 1L)

set.seed(204)
influential_data <- simulate_demo_residual_data(
  scenario = "influential_point",
  sample_size = 80L,
  noise_level = 0.8
)
influential_summary <- summarize_demo_residual_patterns(influential_data)
expect_true(any(influential_data$point_role == "influential"))
expect_true(influential_summary$influential_count >= 1L)

set.seed(205)
residual_state <- get_demo_residual_state(
  scenario = "well_behaved",
  sample_size = 60L,
  noise_level = 1
)
expect_true(is.list(residual_state))
expect_true(all(c("fitted", "residual", "standardized_residual", "cooks_distance") %in% colnames(residual_state$data)))
expect_true(is.numeric(residual_state$r_squared))

expect_error(
  simulate_demo_residual_data(
    scenario = "well_behaved",
    sample_size = 8L,
    noise_level = 1
  ),
  pattern = "at least 10"
)
