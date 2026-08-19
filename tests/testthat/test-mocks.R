# The mocks are what workstreams (b) to (e) build against before the real
# simulator exists. If a mock stops satisfying its validator, everyone
# downstream is building against a contract that no longer holds -- so these
# tests matter more than their size suggests.

test_that("every mock passes its own validator", {
  expect_silent(validate_design_cell(mock_design_cell()))
  expect_silent(validate_sim_dataset(mock_sim_dataset()))
  expect_silent(validate_fit_result(mock_fit_result()))
  expect_silent(validate_score_rows(mock_score_rows()))
})

test_that("mocks cover every candidate family", {
  for (m in CANDIDATE_MODELS) {
    expect_silent(validate_fit_result(mock_fit_result(model = m)))
  }
})

test_that("mocks are deterministic and leave the caller's RNG alone", {
  set.seed(1)
  before <- .Random.seed
  a <- mock_sim_dataset()
  expect_identical(.Random.seed, before)
  b <- mock_sim_dataset()
  expect_equal(a$obs, b$obs)
})

test_that("the mock dataset honours its design cell", {
  cell <- mock_design_cell(n_participants = 7L, followup_days = 365)
  d <- mock_sim_dataset(cell)
  expect_equal(length(unique(d$obs$participant_id)), 7)
  expect_true(max(d$obs$time_days) <= 365)
  expect_silent(validate_sim_dataset(d))
})

test_that("heterogeneity in the mock produces differing individual durations", {
  d <- mock_sim_dataset(mock_design_cell(n_participants = 20L,
                                         heterogeneity = TRUE))
  expect_true(length(unique(d$truth$tstar_days)) > 1)
  expect_silent(validate_sim_dataset(d))
})

test_that("mock fits can be asked for non-crossing draws", {
  fit <- mock_fit_result(no_cross_frac = 0.25)
  expect_true(any(!fit$tstar_draws$crossed))
  expect_true(all(is.infinite(fit$tstar_draws$tstar_days[!fit$tstar_draws$crossed])))
  expect_silent(validate_fit_result(fit))
})
