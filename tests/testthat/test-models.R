# The candidate families and the duration estimand. These are the numbers
# every workstream depends on, so the tests check the properties the handout
# states rather than values anyone has to trust.

test_that("the biphasic parameterisation returns c0 at t = 0", {
  # The point of the handout's parameterisation: c(0) = c0 exactly, for any
  # half-lives and switching time.
  expect_equal(curve_biphasic(0, 0.92, 35, 581, 75), 0.92)
  expect_equal(curve_biphasic(0, 2.5, 10, 1000, 200), 2.5)
})

test_that("the two biphasic components contribute equally at ts", {
  l2 <- log(2)
  ts <- 75
  fast <- exp(-l2 * (ts / 35 + ts / 581))
  slow <- exp(-l2 * (ts / 581 + ts / 35))
  expect_equal(fast, slow)
})

test_that("biphasic decay requires the fast half-life to be the faster one", {
  expect_error(curve_biphasic(0, 0.92, 581, 35, 75), "must be less than")
})

test_that("every candidate family is positive and non-increasing", {
  t <- seq(0, 2000, by = 10)
  curves <- list(
    exponential = curve_exponential(t, 0.92, log(2) / 300),
    biphasic = curve_biphasic(t, 0.92, 35, 581, 75),
    powerlaw = curve_powerlaw(t, 0.92, 0.01, 1.2),
    plateau = curve_plateau(t, 0.92, 0.05, log(2) / 100)
  )
  for (nm in names(curves)) {
    expect_true(all(curves[[nm]] > 0), info = nm)
    expect_true(all(diff(curves[[nm]]) <= 1e-12), info = nm)
  }
})

test_that("curve_eval dispatches to the same values as the curves", {
  expect_equal(
    curve_eval("biphasic", c(0, 100, 500),
               list(c0 = 0.92, h1 = 35, h2 = 581, ts = 75)),
    curve_biphasic(c(0, 100, 500), 0.92, 35, 581, 75)
  )
  expect_error(curve_eval("biphasic", 0, list(c0 = 0.92)), "needs parameter")
})

test_that("tstar matches the closed form for the exponential", {
  k <- log(2) / 300
  expect_equal(tstar("exponential", list(c0 = 0.92, k = k), 0.202),
               log(0.92 / 0.202) / k)
})

test_that("tstar solves the biphasic curve numerically", {
  p <- list(c0 = 0.92, h1 = 35, h2 = 581, ts = 75)
  tt <- tstar("biphasic", p, 0.202)
  expect_true(is.finite(tt) && tt > 0)
  # The defining property: the curve is at the threshold at that time.
  expect_equal(curve_biphasic(tt, 0.92, 35, 581, 75), 0.202, tolerance = 1e-6)
})

test_that("tstar is Inf when the trajectory never reaches the threshold", {
  # A plateau above the threshold never crosses. This must be Inf and not a
  # large finite number: everything downstream keys on that distinction.
  expect_identical(
    tstar("plateau", list(c0 = 0.92, A_inf = 0.5, k = log(2) / 100), 0.202),
    Inf
  )
  # ... and it does cross when the set point is below the threshold.
  expect_true(is.finite(
    tstar("plateau", list(c0 = 0.92, A_inf = 0.05, k = log(2) / 100), 0.202)
  ))
})

test_that("tstar is 0 when the trajectory starts at or below the threshold", {
  expect_identical(tstar("exponential", list(c0 = 0.1, k = 0.01), 0.202), 0)
})

test_that("tstar vectorises over per-participant parameters", {
  h2 <- c(300, 581, 900)
  tt <- tstar("biphasic", list(c0 = 0.92, h1 = 35, h2 = h2, ts = 75), 0.202)
  expect_length(tt, 3)
  # A slower tail must give a longer duration.
  expect_true(all(diff(tt) > 0))
})

test_that("a finite horizon reports a later crossing as Inf", {
  p <- list(c0 = 0.92, h1 = 35, h2 = 581, ts = 75)
  expect_true(is.finite(tstar("biphasic", p, 0.202, horizon_days = 5000)))
  expect_identical(tstar("biphasic", p, 0.202, horizon_days = 100), Inf)
})

test_that("months and days convert round trip", {
  expect_equal(days_to_months(months_to_days(6)), 6)
  expect_equal(months_to_days(12), 365.25)
})
