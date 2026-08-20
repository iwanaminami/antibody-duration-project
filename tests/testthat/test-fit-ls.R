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

# --- the other candidate families ------------------------------------------
#
# `fit_ls()` accepts any of CANDIDATE_MODELS, so all four need cover. They
# cannot be exercised through `mock_sim_dataset()`, whose truth is always
# biphasic, and TRUTH_MODELS does not admit powerlaw or plateau as a truth --
# by design, since a fitted family and a generating mechanism are different
# things. So the data are built here from the frozen curves. `fit_ls()` reads
# `$obs` and `$cell` and nothing else, which is what makes the stand-in safe.

FAMILY_TRUTH <- list(
  exponential = list(c0 = 0.90, k = 0.005),
  biphasic    = list(c0 = 0.92, h1 = 35, h2 = 581, ts = 75),
  powerlaw    = list(c0 = 0.90, alpha = 0.04, beta = 0.80),
  plateau     = list(c0 = 0.90, A_inf = 0.12, k = 0.02)
)

family_data <- function(model, par = FAMILY_TRUTH[[model]], sigma = 0.02,
                        n = 40L, days = 730, c_thr = 0.202, seed = 11L) {
  times <- seq(0, days, by = 30)
  cell <- design_cell(followup_days = days, visits_per_year = 12,
                      n_participants = n, sigma_log = sigma, c_thr = c_thr)
  obs <- .with_seed(seed, do.call(rbind, lapply(seq_len(n), function(i) {
    mu <- curve_eval(model, times, par)
    data.frame(participant_id = as.integer(i),
               time_days = as.numeric(times),
               y_obs = as.numeric(mu * exp(rnorm(length(times), 0, sigma))),
               censor = "none", stringsAsFactors = FALSE)
  })))
  list(cell = cell, obs = tibble::as_tibble(obs))
}

test_that("every candidate family recovers its own parameters", {
  # 5% is loose against the ~1% actually achieved on this data, but the point
  # is that the optimiser lands in the right place, not that it lands to four
  # decimals on one seed.
  for (model in CANDIDATE_MODELS) {
    fit <- fit_ls(family_data(model), model, cfg_test)
    expect_equal(fit$optim$convergence, 0L,
                 info = paste(model, "did not converge"))
    expect_equal(unlist(fit$par), unlist(FAMILY_TRUTH[[model]]),
                 tolerance = 0.05, info = model)
  }
})

test_that("fitted parameters satisfy each family's own constraints", {
  # The working parameterisation exists to make these hold by construction;
  # if one ever fails, the transform and its inverse have drifted apart.
  for (model in CANDIDATE_MODELS) {
    par <- fit_ls(family_data(model), model, cfg_test)$par
    expect_true(all(unlist(par) > 0), info = model)
    if (model == "biphasic") expect_lt(par$h1, par$h2)
    if (model == "plateau") expect_lt(par$A_inf, par$c0)
  }
})

test_that("every candidate family emits a valid fit_result", {
  for (model in CANDIDATE_MODELS) {
    out <- fit_ls_bootstrap(family_data(model), model, cfg_test, n_boot = 20L)
    expect_no_error(validate_fit_result(out))
    expect_equal(out$model, model)
    expect_setequal(unique(out$params$param), curve_params(model))
    expect_true(is.finite(out$ic$aic), info = model)
  }
})

test_that("misspecification is costly, and costly in a direction", {
  # Biphasic truth. A single exponential fitted to a curve that flattens must
  # extrapolate the early rate of decline, so it crosses the threshold too
  # soon and reports T* too long -- and pays for it in AIC. This is the number
  # workstream (c) compares its model selection against.
  d <- mock_sim_dataset(mock_design_cell(n_participants = 100,
                                         followup_days = 365))
  right <- fit_ls_bootstrap(d, "biphasic", cfg_test, n_boot = 20L)
  wrong <- fit_ls_bootstrap(d, "exponential", cfg_test, n_boot = 20L)

  truth <- tstar("biphasic", list(c0 = 0.92, h1 = 35, h2 = 581, ts = 75),
                 0.202)
  t_right <- summarise_tstar(right, "population")$tstar_med
  t_wrong <- summarise_tstar(wrong, "population")$tstar_med

  expect_lt(abs(t_right - truth), abs(t_wrong - truth))
  expect_gt(t_wrong, truth)
  expect_lt(right$ic$aic, wrong$ic$aic)
})

test_that("a plateau settling above the threshold never crosses", {
  # The family that generates genuine non-crossing draws. A_inf above c_thr
  # means the trajectory stops before it reaches the threshold, and that must
  # surface as Inf rather than as a very large finite duration.
  par <- list(c0 = 0.90, A_inf = 0.30, k = 0.02)
  d <- family_data("plateau", par = par, c_thr = 0.202)
  out <- fit_ls_bootstrap(d, "plateau", cfg_test, n_boot = 20L)

  expect_true(all(is.infinite(out$tstar_draws$tstar_days)))
  expect_true(all(!out$tstar_draws$crossed))
  expect_equal(summarise_tstar(out, "population")$prob_no_cross, 1)
})

test_that("the interval variants agree with the project's own summary", {
  # `summarise_tstar()` stays the single place draws become the scored
  # interval. If the percentile row here ever drifts from it, the diagnostic
  # has stopped describing the thing it is meant to put in context.
  d <- mock_sim_dataset(mock_design_cell(n_participants = 40))
  fit <- fit_ls(d, "biphasic", cfg_test)
  boot <- fit_ls_bootstrap(d, "biphasic", cfg_test, n_boot = 60L)

  v <- tstar_interval_variants(fit, boot, cfg_test, level = 0.90)
  s <- summarise_tstar(boot, "population", level = 0.90)
  pc <- v[v$basis == "percentile", ]

  expect_setequal(v$basis, c("percentile", "basic", "bc", "delta"))
  expect_equal(pc$tstar_lo, s$tstar_lo)
  expect_equal(pc$tstar_hi, s$tstar_hi)
  expect_equal(v$prob_no_cross, rep(s$prob_no_cross, 4))
  for (i in which(!is.na(v$tstar_lo))) expect_lte(v$tstar_lo[i], v$tstar_hi[i])
})

test_that("constructions that cannot express a non-crossing draw say so", {
  # Reflecting Inf about the point estimate gives -Inf, and a bias-corrected
  # quantile cannot be placed past it. Returning NA records that those
  # definitions have no answer here, rather than inventing one.
  cell <- design_cell(followup_days = 180, visits_per_year = 12,
                      n_participants = 20, sigma_log = 0.20, c_thr = 1e-9)
  d <- mock_sim_dataset(cell)
  fit <- fit_ls(d, "biphasic", cfg_test)
  boot <- fit_ls_bootstrap(d, "biphasic", cfg_test, n_boot = 20L)

  v <- tstar_interval_variants(fit, boot, cfg_test)
  expect_true(all(is.na(v$tstar_lo[v$basis %in% c("basic", "bc")])))
  expect_true(all(is.na(v$tstar_hi[v$basis %in% c("basic", "bc")])))
  expect_equal(v$prob_no_cross[[1]], 1)
})
