simulate_demo_residual_data <- getFromNamespace("simulate_demo_residual_data", "bootcamp")
build_demo_prediction_interval_app <- getFromNamespace("build_demo_prediction_interval_app", "bootcamp")
simulate_demo_prediction_data <- getFromNamespace("simulate_demo_prediction_data", "bootcamp")
summarize_demo_prediction_interval_state <- getFromNamespace("summarize_demo_prediction_interval_state", "bootcamp")
get_demo_prediction_interval_state <- getFromNamespace("get_demo_prediction_interval_state", "bootcamp")

pdf_file <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_file)
on.exit({
  grDevices::dev.off()
  unlink(pdf_file)
}, add = TRUE)

set.seed(300)
hist_values <- stats::rnorm(120, mean = 5, sd = 1.5)
hist_result <- bootcamp::annotated_histogram(
  hist_values,
  show_sd = TRUE,
  show_rug = TRUE
)
expect_equal(hist_result$n, 120L)
expect_true(abs(hist_result$mean - mean(hist_values)) < 1e-10)
expect_true(abs(hist_result$median - stats::median(hist_values)) < 1e-10)

expect_error(
  bootcamp::annotated_histogram(c(NA_real_, NA_real_), na.rm = TRUE),
  pattern = "at least one finite value"
)

set.seed(301)
scatter_x <- stats::rnorm(100)
scatter_y <- 1 + 0.9 * scatter_x + stats::rnorm(100, sd = 0.4)
scatter_result <- bootcamp::annotated_scatterplot(
  scatter_x,
  scatter_y,
  highlight_n = 2L
)
expect_true(!is.na(scatter_result$correlation))
expect_true(scatter_result$correlation > 0.6)
expect_equal(length(scatter_result$highlighted), 2L)
expect_true(!is.na(scatter_result$slope))

grouped_values <- list(
  Group_A = c(stats::rnorm(40), 7),
  Group_B = stats::rnorm(41, mean = 1)
)
box_result <- bootcamp::annotated_boxplot(grouped_values, horizontal = FALSE)
expect_equal(nrow(box_result), 2L)
expect_true(all(c("group", "n", "mean", "median", "iqr", "outliers") %in% colnames(box_result)))
expect_true(any(box_result$outliers >= 1L))

set.seed(302)
x <- stats::rnorm(150)
z <- stats::rnorm(150)
y <- 3 + 1.1 * x - 0.5 * z + stats::rnorm(150, sd = 0.7)
fit <- stats::lm(y ~ x + z)

lm_explanation <- bootcamp::explain_lm(fit, print = FALSE)
expect_true(is.list(lm_explanation))
expect_true(all(c("model_summary", "coefficient_summary") %in% names(lm_explanation)))
expect_true("interpretation" %in% colnames(lm_explanation$coefficient_summary))
expect_true(any(grepl("Holding the other predictors constant", lm_explanation$coefficient_summary$interpretation)))

set.seed(303)
well_behaved_data <- simulate_demo_residual_data(
  scenario = "well_behaved",
  sample_size = 90L,
  noise_level = 0.6
)
well_behaved_fit <- stats::lm(y ~ x, data = well_behaved_data)
well_behaved_checks <- bootcamp::check_lm_assumptions(well_behaved_fit, print = FALSE)
expect_true(is.data.frame(well_behaved_checks))
expect_true(all(c("assumption", "metric", "value", "guideline", "status", "interpretation") %in% colnames(well_behaved_checks)))
expect_equal(well_behaved_checks$status[well_behaved_checks$assumption == "Independence"], "not_assessed")

set.seed(304)
curvature_data <- simulate_demo_residual_data(
  scenario = "curvature",
  sample_size = 90L,
  noise_level = 0.5
)
curvature_fit <- stats::lm(y ~ x, data = curvature_data)
curvature_checks <- bootcamp::check_lm_assumptions(curvature_fit, print = FALSE)
expect_equal(curvature_checks$status[curvature_checks$assumption == "Linearity"], "check")

set.seed(305)
hetero_data <- simulate_demo_residual_data(
  scenario = "heteroscedasticity",
  sample_size = 90L,
  noise_level = 0.7
)
hetero_fit <- stats::lm(y ~ x, data = hetero_data)
hetero_checks <- bootcamp::check_lm_assumptions(hetero_fit, print = FALSE)
expect_equal(hetero_checks$status[hetero_checks$assumption == "Equal variance"], "check")

expect_true(inherits(build_demo_prediction_interval_app(), "shiny.appobj"))

set.seed(306)
prediction_data <- simulate_demo_prediction_data(
  sample_size = 80L,
  noise_level = 1
)
expect_true(all(c("x", "y") %in% colnames(prediction_data)))
expect_equal(nrow(prediction_data), 80L)

prediction_state <- summarize_demo_prediction_interval_state(
  data = prediction_data,
  x_focus = 5,
  confidence_level = 0.95
)
expect_true(is.list(prediction_state))
expect_true(diff(prediction_state$prediction_interval) > diff(prediction_state$confidence_interval))
expect_true(all(c("x", "fit", "conf_low", "conf_high", "pred_low", "pred_high") %in% colnames(prediction_state$band_data)))

set.seed(307)
prediction_state_full <- get_demo_prediction_interval_state(
  sample_size = 70L,
  noise_level = 0.8,
  confidence_level = 0.9,
  x_focus = 6
)
expect_true(is.numeric(prediction_state_full$mean_prediction))
expect_true(prediction_state_full$x_focus == 6)

expect_error(
  summarize_demo_prediction_interval_state(
    data = prediction_data,
    x_focus = NA_real_,
    confidence_level = 0.95
  ),
  pattern = "single finite number"
)
