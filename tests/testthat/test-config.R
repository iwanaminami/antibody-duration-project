# The configuration is where every number a result depends on lives, so the
# tests check that the handout's values arrive intact and that the derived
# fields are computed rather than duplicated.

test_that("the packaged configuration loads and derives its fields", {
  cfg <- read_config()
  expect_equal(cfg$calibration$biphasic$c0, 0.92)
  expect_equal(cfg$calibration$biphasic$h2_days, 581)
  expect_equal(cfg$calibration$biphasic$h1_days, 35)
  expect_equal(cfg$calibration$biphasic$ts_days, 75)
  expect_equal(cfg$calibration$observation$sigma_log, 0.20)
  expect_equal(cfg$calibration$heterogeneity$omega_0, 0.30)
  # Resolved from the named thresholds rather than written twice.
  expect_equal(cfg$calibration$c_thr, 0.202)
  # Months are labels; days are what the code uses.
  expect_equal(cfg$design$followup_days, round(months_to_days(c(3, 6, 9, 12, 18, 24))))
})

test_that("overrides merge into nested entries without dropping siblings", {
  cfg <- read_config(overrides = list(design = list(n_replicates = 2)))
  expect_equal(cfg$design$n_replicates, 2)
  expect_equal(cfg$design$visits_per_year, c(12L, 4L))

  cfg <- read_config(overrides = list(
    calibration = list(default_threshold = "severe")
  ))
  expect_equal(cfg$calibration$c_thr, 0.030)
  expect_equal(cfg$calibration$biphasic$c0, 0.92)
})

test_that("an unknown threshold name is refused rather than defaulted", {
  expect_error(
    read_config(overrides = list(calibration = list(default_threshold = "mild"))),
    "default_threshold"
  )
})

test_that("the configuration expands into a valid design grid", {
  cfg <- read_config(overrides = list(design = list(n_replicates = 2)))
  cells <- config_design_grid(cfg)
  expect_length(cells, 6 * 2 * 1 * 1 * 1 * 2)
  for (cell in cells) expect_silent(validate_design_cell(cell))
  expect_true(all(vapply(cells, `[[`, numeric(1), "c_thr") == 0.202))
})

test_that("the threshold sits below the early antibody level", {
  # If it did not, every duration would be zero -- worth failing loudly.
  cfg <- read_config()
  expect_lt(cfg$calibration$c_thr, cfg$calibration$biphasic$c0)
})
