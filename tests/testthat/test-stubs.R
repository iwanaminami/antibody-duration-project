# Unimplemented functions must fail loudly and say who owns them. A stub that
# returned something plausible would quietly reach a results table.

test_that("stubs refuse to run and name their workstream", {
  expect_error(fit_ls(NULL, "biphasic", NULL), "not implemented")
  expect_error(fit_ls(NULL, "biphasic", NULL), "\\(b\\) simple analysis")
  expect_error(fit_mcmc(NULL, "biphasic", NULL), "\\(c\\) alternate models")
  expect_error(fit_nlme(NULL, "biphasic", NULL), "\\(d\\) NLME")
  expect_error(check_calibration(NULL), "\\(e\\) calibration")
  expect_error(simulate_full(NULL, NULL), "\\(a\\) coordinator")
  expect_error(score_fit(NULL, NULL, NULL), "\\(a\\) coordinator")
})

test_that("stubs point at the mocks to build against", {
  expect_error(fit_ls(NULL, "biphasic", NULL), "mock_sim_dataset")
})
