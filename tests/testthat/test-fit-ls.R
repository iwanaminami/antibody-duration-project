# Workstream (b). The assertions are properties, not numbers: the exact
# estimate depends on the RNG, but a fit that inverts the half-lives, hides a
# non-crossing replicate inside a finite number or reports draws that fail the
# schema is wrong however plausible the number looks.

cfg_test <- read_config()

test_that("fit_ls recovers the generating parameters and respects h1 < h2", {
  # 60 participants at monthly visits for two years: enough follow-up that the
  # slow phase is observed, so the truth should be recovered.
  cell <- design_cell(followup_days = 730, visits_per_year = 12,
                      n_participants = 60, sigma_log = 0.05, c_thr = 0.202)
  data <- mock_sim_dataset(cell)
  fit <- fit_ls(data, "biphasic", cfg_test)

  expect_equal(fit$optim$convergence, 0L)
  expect_lt(fit$par$h1, fit$par$h2)
  expect_equal(fit$par$c0, 0.92, tolerance = 0.1)
  expect_equal(fit$par$h2, 581, tolerance = 0.3)
  # The residual SD estimates the assay error the cell was simulated with.
  expect_equal(fit$sigma_log, cell$sigma_log, tolerance = 0.3)
})

test_that("fit_ls drops censored rows and says how many", {
  data <- mock_sim_dataset(mock_design_cell(n_participants = 20),
                           censor_frac = 0.2)
  fit <- fit_ls(data, "biphasic", cfg_test)

  n_censored <- sum(data$obs$censor != "none")
  expect_gt(n_censored, 0)
  expect_equal(fit$n_dropped, n_censored)
  expect_equal(fit$n_obs, nrow(data$obs) - n_censored)
  expect_false(any(is.na(fit$obs$y_obs)))
})

test_that("fit_ls reads only the observations, never the truth", {
  data <- mock_sim_dataset(mock_design_cell(n_participants = 20))
  stripped <- data
  stripped$truth <- NULL
  stripped$meta <- NULL
  expect_equal(fit_ls(stripped, "biphasic", cfg_test)$par,
               fit_ls(data, "biphasic", cfg_test)$par)
})

test_that("fit_ls_bootstrap emits a valid fit_result of population draws", {
  data <- mock_sim_dataset(mock_design_cell(n_participants = 30))
  out <- fit_ls_bootstrap(data, "biphasic", cfg_test, n_boot = 40L)

  expect_no_error(validate_fit_result(out))
  expect_equal(out$method, "ls")
  expect_equal(out$model, "biphasic")
  expect_true(all(out$tstar_draws$estimand == "population"))
  expect_true(all(is.na(out$tstar_draws$participant_id)))
  expect_equal(nrow(out$tstar_draws), out$diagnostics$n_draws)
  expect_setequal(unique(out$params$param), curve_params("biphasic"))
  expect_true(is.finite(out$ic$aic))

  # The draws are the point of the exercise: they must summarise like any
  # other method's.
  s <- summarise_tstar(out, estimand = "population")
  expect_true(s$tstar_lo <= s$tstar_med)
  expect_true(s$tstar_med <= s$tstar_hi)
})

test_that("bootstrap draws are reproducible from the design seed", {
  data <- mock_sim_dataset(mock_design_cell(n_participants = 20))
  a <- fit_ls_bootstrap(data, "biphasic", cfg_test, n_boot = 25L)
  b <- fit_ls_bootstrap(data, "biphasic", cfg_test, n_boot = 25L)
  expect_equal(a$tstar_draws$tstar_days, b$tstar_draws$tstar_days)
})

test_that("bootstrap leaves the caller's RNG state alone", {
  data <- mock_sim_dataset(mock_design_cell(n_participants = 20))
  set.seed(1)
  before <- .Random.seed
  invisible(fit_ls_bootstrap(data, "biphasic", cfg_test, n_boot = 10L))
  expect_identical(.Random.seed, before)
})

test_that("a threshold that is never reached gives Inf, not a big number", {
  # A threshold far below anything the curve reaches within the search cap.
  cell <- design_cell(followup_days = 180, visits_per_year = 12,
                      n_participants = 20, sigma_log = 0.20, c_thr = 1e-9)
  data <- mock_sim_dataset(cell)
  out <- fit_ls_bootstrap(data, "biphasic", cfg_test, n_boot = 20L)

  expect_true(all(is.infinite(out$tstar_draws$tstar_days)))
  expect_true(all(!out$tstar_draws$crossed))
  expect_equal(summarise_tstar(out, "population")$prob_no_cross, 1)
})

test_that("the delta interval is ordered and matches the summary columns", {
  data <- mock_sim_dataset(mock_design_cell(n_participants = 30))
  fit <- fit_ls(data, "biphasic", cfg_test)
  d <- tstar_delta_interval(fit, cfg_test, level = 0.90)

  expect_equal(nrow(d), 1L)
  expect_true(all(c("tstar_med", "tstar_lo", "tstar_hi", "interval_level") %in%
                    names(d)))
  expect_equal(d$interval_level, 0.90)
  expect_true(d$tstar_lo <= d$tstar_med)
  expect_true(d$tstar_med <= d$tstar_hi)
  expect_gt(d$tstar_lo, 0)   # log scale keeps the interval positive
})

test_that("a non-crossing point estimate gets no finite delta interval", {
  cell <- design_cell(followup_days = 180, visits_per_year = 12,
                      n_participants = 20, sigma_log = 0.20, c_thr = 1e-9)
  fit <- fit_ls(mock_sim_dataset(cell), "biphasic", cfg_test)
  d <- tstar_delta_interval(fit, cfg_test)

  expect_true(is.infinite(d$tstar_med))
  expect_true(is.infinite(d$tstar_hi))
})
