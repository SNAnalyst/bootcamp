runner_args <- new.env(parent = emptyenv())
runner_args$name <- NULL
runner_args$package <- NULL

run_bootcamp_tutorial <- getFromNamespace("run_bootcamp_tutorial", "bootcamp")
selected_tutorial <- run_bootcamp_tutorial(
  "demo-tutorial",
  runner = function(name, package) {
    runner_args$name <- name
    runner_args$package <- package
    invisible(NULL)
  }
)

expect_equal(selected_tutorial, "demo-tutorial")
expect_equal(runner_args$name, "demo-tutorial")
expect_equal(runner_args$package, "bootcamp")

expect_error(
  bootcamp::bootcamp_tutorials(graphics = NA),
  pattern = "TRUE or FALSE"
)
