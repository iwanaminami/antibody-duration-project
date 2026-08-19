# The shared contract. Each validator must accept a well-formed artefact and
# reject the specific mistakes that would otherwise surface much later, in
# someone else's workstream.

make_cell <- function(...) mock_design_cell(...)

# --- design_cell -----------------------------------------------------------

test_that("a constructed design cell validates", {
  expect_silent(validate_design_cell(make_cell()))
})

test_that("cell_id identifies the design and excludes the replicate", {
  a <- make_cell(replicate = 1L)
  b <- make_cell(replicate = 2L)
  expect_identical(a$cell_id, b$cell_id)
  expect_false(identical(a$seed, b$seed))
})

test_that("cell_id changes when the design changes", {
  expect_false(identical(make_cell()$cell_id,
                         make_cell(followup_days = 365)$cell_id))
})

test_that("seeds are deterministic across sessions", {
  expect_identical(derive_seed("abc", 1L), derive_seed("abc", 1L))
  expect_false(identical(derive_seed("abc", 1L), derive_seed("abc", 2L)))
})

test_that("design_cell rejects impossible designs", {
  expect_error(validate_design_cell(structure(list(), class = "design_cell")),
               "missing required element")
  bad <- make_cell(); bad$n_participants <- 0L
  expect_error(validate_design_cell(bad), "n_participants")
  bad <- make_cell(); bad$c_thr <- -1
  expect_error(validate_design_cell(bad), "c_thr")
  bad <- make_cell(); bad$lloq <- 5; bad$uloq <- 1
  expect_error(validate_design_cell(bad), "less than")
  bad <- make_cell(); bad$heterogeneity <- NA
  expect_error(validate_design_cell(bad), "heterogeneity")
})

test_that("design_grid crosses every factor with the replicates", {
  cells <- design_grid(followup_days = c(180, 365), visits_per_year = c(4, 12),
                       n_participants = 100, sigma_log = 0.2, c_thr = 0.202,
                       n_replicates = 3)
  expect_length(cells, 2 * 2 * 3)
  expect_length(unique(vapply(cells, `[[`, character(1), "cell_id")), 4)
  for (cell in cells) expect_silent(validate_design_cell(cell))
})

# --- check_cols ------------------------------------------------------------

test_that("check_cols reports the specific problem it found", {
  spec <- list(a = list(type = "double", min = 0),
               b = list(type = "character", values = c("x", "y")))
  expect_silent(check_cols(data.frame(a = 1, b = "x"), spec))
  expect_error(check_cols(data.frame(a = 1), spec), "missing required column")
  expect_error(check_cols(data.frame(a = -1, b = "x"), spec), "must be >= 0")
  expect_error(check_cols(data.frame(a = 1, b = "z"), spec), "unexpected value")
  expect_error(check_cols(data.frame(a = "s", b = "x"), spec), "must be double")
  expect_error(check_cols(data.frame(a = NA_real_, b = "x"), spec),
               "must not contain NA")
  # Extra columns are fine: a workstream may carry its own diagnostics.
  expect_silent(check_cols(data.frame(a = 1, b = "x", extra = TRUE), spec))
})

test_that("NaN is never acceptable but Inf can be", {
  spec <- list(a = list(type = "double", inf_ok = TRUE))
  expect_error(check_cols(data.frame(a = NaN), spec), "NaN")
  expect_silent(check_cols(data.frame(a = Inf), spec))
  expect_error(check_cols(data.frame(a = Inf), list(a = list(type = "double"))),
               "must be finite")
})

test_that("duration columns must be named _days and be non-negative", {
  expect_silent(check_days_cols(data.frame(tstar_days = c(1, Inf))))
  expect_error(check_days_cols(data.frame(tstar_days = -1)), "must be >= 0")
  expect_error(check_days_cols(data.frame(tstar_days = "x")), "must be numeric")
})

# --- sim_dataset -----------------------------------------------------------

test_that("a constructed simulated dataset validates", {
  expect_silent(validate_sim_dataset(mock_sim_dataset()))
})

test_that("censored rows must carry NA rather than the limit", {
  d <- mock_sim_dataset(censor_frac = 0.1)
  expect_silent(validate_sim_dataset(d))
  expect_true(all(is.na(d$obs$y_obs[d$obs$censor != "none"])))

  bad <- d
  bad$obs$y_obs[bad$obs$censor == "left"][1] <- 0.01
  expect_error(validate_sim_dataset(bad), "censored rows must have y_obs = NA")

  bad <- mock_sim_dataset()
  bad$obs$y_obs[1] <- NA_real_
  expect_error(validate_sim_dataset(bad), "non-missing y_obs")
})

test_that("sim_dataset rejects data that contradicts its own design cell", {
  bad <- mock_sim_dataset()
  bad$obs$time_days[1] <- bad$cell$followup_days + 1
  expect_error(validate_sim_dataset(bad), "followup_days")

  bad <- mock_sim_dataset()
  bad$truth <- bad$truth[-1, ]
  expect_error(validate_sim_dataset(bad), "differ|participants")

  bad <- mock_sim_dataset()
  bad$obs$censor[1] <- "sideways"
  expect_error(validate_sim_dataset(bad), "unexpected value")
})

test_that("an infinite true duration is permitted", {
  d <- mock_sim_dataset()
  d$truth$tstar_days[1] <- Inf
  expect_silent(validate_sim_dataset(d))
})

# --- fit_result ------------------------------------------------------------

test_that("a constructed fit result validates", {
  expect_silent(validate_fit_result(mock_fit_result()))
})

test_that("crossed must agree with the finiteness of the duration", {
  # The invariant the whole non-crossing convention rests on.
  bad <- mock_fit_result()
  bad$tstar_draws$crossed[1] <- !bad$tstar_draws$crossed[1]
  expect_error(validate_fit_result(bad), "crossed")

  bad <- mock_fit_result()
  # Substituting a big number for Inf is the mistake this catches.
  inf_row <- which(!is.finite(bad$tstar_draws$tstar_days))[1]
  bad$tstar_draws$tstar_days[inf_row] <- 1e9
  expect_error(validate_fit_result(bad), "never a large finite number")
})

test_that("the estimand determines whether a participant id is present", {
  bad <- mock_fit_result()
  bad$tstar_draws$participant_id[bad$tstar_draws$estimand == "population"][1] <- 1L
  expect_error(validate_fit_result(bad), "population rows")

  bad <- mock_fit_result()
  bad$tstar_draws$participant_id[bad$tstar_draws$estimand == "individual"][1] <- NA_integer_
  expect_error(validate_fit_result(bad), "individual rows")
})

test_that("parameters must belong to the fitted family", {
  bad <- mock_fit_result(model = "exponential")
  bad$params$param[1] <- "h2"
  expect_error(validate_fit_result(bad), "unexpected")
})

test_that("a fit result must carry draws even when the fit failed", {
  bad <- mock_fit_result()
  bad$tstar_draws <- bad$tstar_draws[0, ]
  expect_error(validate_fit_result(bad), "no draws")
})

test_that("summarise_tstar gives one row per estimand and counts Inf draws", {
  fit <- mock_fit_result(no_cross_frac = 0.1)
  s <- summarise_tstar(fit, "population", level = 0.90)
  expect_equal(nrow(s), 1)
  expect_equal(s$interval_level, 0.90)
  expect_equal(s$prob_no_cross, 0.1, tolerance = 0.01)
  expect_true(s$tstar_lo <= s$tstar_med)
  expect_error(summarise_tstar(mock_fit_result(participant_ids = integer(0)),
                               "individual"),
               "no 'individual' draws")
})

test_that("summarise_tstar propagates Inf rather than dropping it", {
  fit <- mock_fit_result(no_cross_frac = 0.9)
  s <- summarise_tstar(fit, "population", level = 0.90)
  expect_identical(s$tstar_hi, Inf)
  expect_false(is.nan(s$tstar_med))
})

# --- score_rows ------------------------------------------------------------

test_that("a constructed score table validates", {
  expect_silent(validate_score_rows(mock_score_rows()))
  expect_s3_class(empty_score_rows(), "score_rows")
  expect_equal(nrow(empty_score_rows()), 0)
})

test_that("score rows must be unique on their key", {
  s <- mock_score_rows()
  expect_error(score_rows(rbind(s, s)), "unique")
})

test_that("score rows reject an inverted interval and a bad estimand", {
  s <- mock_score_rows()
  s$tstar_hi[1] <- s$tstar_lo[1] - 1
  expect_error(score_rows(s), "tstar_hi")

  s <- mock_score_rows()
  s$estimand[1] <- "somewhere in between"
  expect_error(score_rows(s), "unexpected value")
})

test_that("every duration column in the score schema is named _days", {
  duration_cols <- grep("^tstar_(true|med|lo|hi)", SCORE_COLUMNS, value = TRUE)
  expect_true(all(grepl("_days$", grep("^tstar_(true)", duration_cols,
                                       value = TRUE))))
})
