# ---------------------------------------------------------------------------
# Simple analysis: biphasic model by least squares.
#
# OWNER: workstream (b). Contract in INTERFACES.md.
#
# The reference point everything else is judged against. Fit the biphasic
# family of handout Equation (2) to the log measurements by least squares, and
# turn the fitted parameters into a duration with an interval.
#
# The interval is the substantive part. `tstar()` is a strongly non-linear
# function of the parameters, and the slow half-life h2 -- the parameter that
# governs the tail -- is poorly identified until the fast phase has subsided.
# A naive curvature-based interval will be too narrow at exactly the short
# follow-up times the project cares most about. Prefer a bootstrap, and if a
# delta-method interval is used as well, report both so the difference is
# visible rather than assumed away.
#
# Emit draws, not just an interval: `fit_result$tstar_draws` is the common
# currency, and bootstrap replicates are draws. See R/schema-fit.R.
#
# --- decisions taken here, so that they are visible rather than implied -----
#
# CONSTRAINTS BY REPARAMETERISATION. `curve_biphasic()` stops when h1 >= h2,
# and an unconstrained optimiser walks into that region on the way to the
# optimum. Rather than penalise it after the fact, the search runs on
# theta = (log c0, log h1, log(h2 - h1), log ts), which cannot represent an
# invalid parameter vector. The same idea gives positivity for the other
# families. All reporting is on the natural scale.
#
# CENSORED ROWS ARE DROPPED, AND THE COUNT IS REPORTED. Least squares has no
# way to use a row that says only "below the limit", so those rows leave the
# fit. This is not neutral: dropping left-censored observations removes the
# lowest late measurements, which flattens the tail, biases h2 upward and so
# biases T* long -- in the same direction the project is trying to measure.
# The number dropped is recorded in `$diagnostics$message` so that a run with
# censoring cannot be mistaken for a run without it.
#
# h2 IS UNIDENTIFIED AT SHORT FOLLOW-UP, AND THAT IS WHERE THE SKEW COMES
# FROM. On six months of data, bootstrap resamples return slow half-lives
# spanning ten orders of magnitude -- a few hundred days to 1e10 -- at
# residual sums of squares that differ in the fifth decimal. The objective is
# flat along that direction, so a search stops wherever it happens to enter
# the ridge. A resample that lands at the far end describes a tail that never
# decays, and its T* runs away with it: that is the mechanism behind the
# heavy right tail in the duration draws (skewness of log T* around 9 on a
# three-month cell), and behind the non-crossing replicates that make the
# upper end of the interval Inf.
#
# Two consequences. Compare fits by their objective and their T*, never by
# their parameters -- comparing parameters asserts an identifiability the
# data do not have. And do not start the resamples from the point estimate to
# save time: anchoring them on the ridge stops the search reaching the far
# end, which silently narrows the interval and turns Inf into a large finite
# number. That was measured, and reverted, on this branch.
#
# THE ESTIMAND IS THE POPULATION. A pooled least-squares fit has no random
# effects, so one curve is fitted to everybody and the duration it implies is
# a statement about the population trajectory, not about any participant.
# Under heterogeneity that target is itself biased, because a non-linear curve
# through pooled data is not the curve of the mean parameters -- which is the
# gap workstream (d) exists to close. Labelling these draws `population` keeps
# the comparison honest.
# ---------------------------------------------------------------------------

# --- working parameterisation ----------------------------------------------

# Natural parameters -> unconstrained working vector. See the header note.
.ls_to_working <- function(p, model) {
  switch(
    model,
    exponential = c(log(p$c0), log(p$k)),
    biphasic    = c(log(p$c0), log(p$h1), log(p$h2 - p$h1), log(p$ts)),
    powerlaw    = c(log(p$c0), log(p$alpha), log(p$beta)),
    plateau     = c(log(p$c0), stats::qlogis(p$A_inf / p$c0), log(p$k))
  )
}

# Working vector -> natural parameters, in `curve_params()` order.
.ls_from_working <- function(theta, model) {
  switch(
    model,
    exponential = list(c0 = exp(theta[1]), k = exp(theta[2])),
    biphasic = {
      h1 <- exp(theta[2])
      list(c0 = exp(theta[1]), h1 = h1, h2 = h1 + exp(theta[3]),
           ts = exp(theta[4]))
    },
    powerlaw = list(c0 = exp(theta[1]), alpha = exp(theta[2]),
                    beta = exp(theta[3])),
    plateau = {
      c0 <- exp(theta[1])
      list(c0 = c0, A_inf = c0 * stats::plogis(theta[2]), k = exp(theta[3]))
    }
  )
}

# Residual sum of squares on the log scale. A parameter vector the curve
# refuses to evaluate returns a large finite penalty rather than an error, so
# that the optimiser can back out of the region instead of the fit failing.
.LS_PENALTY <- 1e10

.ls_ssr <- function(theta, model, t, logy) {
  if (any(!is.finite(theta))) return(.LS_PENALTY)
  p <- .ls_from_working(theta, model)
  mu <- tryCatch(curve_eval(model, t, p), error = function(e) NULL)
  if (is.null(mu) || any(!is.finite(mu)) || any(mu <= 0)) return(.LS_PENALTY)
  sum((logy - log(mu))^2)
}

# Nelder-Mead to get into the basin, BFGS to polish. The objective is cheap
# and the starting values are only as good as the handout's calibration, so
# the two-stage search costs little and fails far less often than either
# method alone.
.ls_fit_core <- function(t, logy, model, start_par) {
  theta0 <- .ls_to_working(start_par, model)
  obj <- function(theta) .ls_ssr(theta, model, t, logy)
  nm <- stats::optim(theta0, obj, method = "Nelder-Mead",
                     control = list(maxit = 1000L, reltol = 1e-10))
  bf <- stats::optim(nm$par, obj, method = "BFGS",
                     control = list(maxit = 500L, reltol = 1e-12))
  grad_calls <- unname(bf$counts[2])
  list(
    theta = bf$par,
    ssr = bf$value,
    convergence = bf$convergence,
    counts = c(function_calls = unname(nm$counts[1]) + unname(bf$counts[1]),
               gradient_calls = if (is.na(grad_calls)) 0L else grad_calls),
    message = if (is.null(bf$message)) NA_character_ else bf$message
  )
}

# Starting values. The handout supplies them for the biphasic family only;
# anything else starts from a log-linear regression, which is crude but is at
# least derived from the data in hand rather than invented here.
.ls_start <- function(t, logy, model, cfg) {
  cal <- cfg$calibration$biphasic
  if (identical(model, "biphasic")) {
    return(list(c0 = cal$c0, h1 = cal$h1_days, h2 = cal$h2_days,
                ts = cal$ts_days))
  }
  co <- stats::coef(stats::lm(logy ~ t))
  c0 <- exp(unname(co[1]))
  k <- max(-unname(co[2]), 1e-6)
  switch(
    model,
    exponential = list(c0 = c0, k = k),
    powerlaw = list(c0 = c0, alpha = k, beta = 1),
    plateau = list(c0 = c0, A_inf = 0.5 * min(exp(logy)), k = k)
  )
}

# Central differences on the working scale, for the delta method. Base R only:
# numDeriv would be more accurate, but adding a dependency for one gradient of
# a smooth four-parameter function is not worth the lockfile churn.
.ls_num_grad <- function(f, theta) {
  h <- 1e-5 * pmax(1, abs(theta))
  vapply(seq_along(theta), function(j) {
    up <- theta; up[j] <- up[j] + h[j]
    dn <- theta; dn[j] <- dn[j] - h[j]
    (f(up) - f(dn)) / (2 * h[j])
  }, numeric(1))
}

# The threshold belongs to the design cell. Reading `$cell` is not reading the
# answer: the cell is what the trial designed, `$truth` is what only the
# simulator knows, and this file never touches `$truth` or `$meta`.
.ls_c_thr <- function(data, cfg) {
  thr <- data$cell$c_thr
  if (is.null(thr) || !is.finite(thr)) thr <- cfg$calibration$c_thr
  thr
}

# --- fitting ---------------------------------------------------------------

#' Fit a candidate family by least squares on the log scale.
#'
#' Least squares on `log y` is the maximum-likelihood fit under the handout's
#' log-normal observation model, which keeps this comparable with the
#' likelihood-based methods in workstreams (c) and (d).
#'
#' Censored observations carry `NA` in `y_obs`; decide and document how they
#' are handled, because dropping them is itself an assumption.
#'
#' @param data a [sim_dataset()].
#' @param model one of [CANDIDATE_MODELS].
#' @param cfg configuration from [read_config()].
#' @param start named list of starting values; `NULL` uses
#'   `cfg$calibration$biphasic`.
#' @return a list with the point estimate, the residual SD and the optimiser's
#'   own report -- convergence code, objective value, number of evaluations.
#'   Check it: a result that looks reasonable but came from an optimiser that
#'   had not converged is the failure mode that is hardest to spot later.
#'   Also carries `$vcov`, the working-scale covariance that
#'   [tstar_delta_interval()] needs, and `$obs`, the rows the fit actually
#'   used, so that the bootstrap resamples exactly what was fitted.
fit_ls <- function(data, model = "biphasic", cfg, start = NULL) {
  model <- match.arg(model, CANDIDATE_MODELS)
  if (is.null(cfg)) {
    stop("fit_ls(): cfg is required; see read_config()", call. = FALSE)
  }
  obs <- data$obs
  n_all <- nrow(obs)
  keep <- obs$censor == "none" & !is.na(obs$y_obs) & obs$y_obs > 0
  obs <- obs[keep, , drop = FALSE]
  n_dropped <- n_all - nrow(obs)

  n_par <- length(curve_params(model))
  if (nrow(obs) <= n_par) {
    stop(sprintf(
      "fit_ls(): %d usable observations for a %d-parameter model",
      nrow(obs), n_par
    ), call. = FALSE)
  }

  t <- obs$time_days
  logy <- log(obs$y_obs)
  if (is.null(start)) start <- .ls_start(t, logy, model, cfg)
  core <- .ls_fit_core(t, logy, model, start)

  par <- .ls_from_working(core$theta, model)
  sigma_log <- sqrt(core$ssr / (nrow(obs) - n_par))

  # Var(theta) ~= 2 sigma^2 H^-1, with H the Hessian of the residual sum of
  # squares: for the Gaussian log-likelihood the information is H / (2 sigma^2).
  # A singular Hessian is a real result about identifiability, so it is
  # recorded as a missing covariance rather than patched with a pseudo-inverse.
  hess <- tryCatch(
    stats::optimHess(core$theta, function(th) .ls_ssr(th, model, t, logy)),
    error = function(e) NULL
  )
  vcov <- if (is.null(hess)) {
    NULL
  } else {
    tryCatch(2 * sigma_log^2 * solve(hess), error = function(e) NULL)
  }

  list(
    model = model,
    par = par,
    theta = core$theta,
    vcov = vcov,
    sigma_log = sigma_log,
    c_thr = .ls_c_thr(data, cfg),
    n_obs = nrow(obs),
    n_dropped = n_dropped,
    obs = obs,
    optim = list(convergence = core$convergence, objective = core$ssr,
                 counts = core$counts, message = core$message)
  )
}

# Log-likelihood of the log-normal observation model, on the scale of y. The
# Jacobian term -sum(log y) is included so that these information criteria can
# be compared with the likelihood-based fits from (c) and (d), which work with
# the density of y rather than of log y.
.ls_ic <- function(fit) {
  mu <- curve_eval(fit$model, fit$obs$time_days, fit$par)
  logy <- log(fit$obs$y_obs)
  n <- length(logy)
  sigma_ml <- sqrt(sum((logy - log(mu))^2) / n)
  loglik <- sum(stats::dnorm(logy, log(mu), sigma_ml, log = TRUE)) - sum(logy)
  n_par <- length(curve_params(fit$model)) + 1  # + sigma
  default_ic(loglik = loglik, n_par = n_par,
             aic = -2 * loglik + 2 * n_par,
             bic = -2 * loglik + log(n) * n_par)
}

#' Bootstrap the least-squares fit and return duration draws.
#'
#' Resample participants, not observations: participants are the independent
#' unit, and resampling within a participant's own trajectory would understate
#' the uncertainty.
#'
#' @param n_boot number of bootstrap replicates.
#' @return a [fit_result()] with `method = "ls"`, whose `$tstar_draws` holds
#'   one row per bootstrap replicate. Non-crossing replicates are `Inf` with
#'   `crossed = FALSE`.
fit_ls_bootstrap <- function(data, model = "biphasic", cfg, n_boot = 500L,
                             start = NULL) {
  started <- Sys.time()
  model <- match.arg(model, CANDIDATE_MODELS)
  n_boot <- as.integer(n_boot)
  point <- fit_ls(data, model, cfg, start)

  obs <- point$obs
  ids <- unique(obs$participant_id)
  by_id <- split(seq_len(nrow(obs)), obs$participant_id)

  # The seed comes from the design, so a cached sweep and a fresh one agree.
  seed <- derive_seed(data$cell$cell_id, data$cell$replicate)
  reps <- .with_seed(seed, lapply(seq_len(n_boot), function(b) {
    take <- sample(ids, length(ids), replace = TRUE)
    rows <- unlist(by_id[as.character(take)], use.names = FALSE)
    d <- obs[rows, , drop = FALSE]
    core <- tryCatch(
      .ls_fit_core(d$time_days, log(d$y_obs), model, point$par),
      error = function(e) NULL
    )
    if (is.null(core) || core$convergence != 0) return(NULL)
    .ls_from_working(core$theta, model)
  }))

  ok <- Filter(Negate(is.null), reps)
  n_failed <- n_boot - length(ok)
  if (length(ok) == 0L) {
    stop("fit_ls_bootstrap(): every bootstrap replicate failed to converge",
         call. = FALSE)
  }

  nms <- curve_params(model)
  params <- tibble::tibble(
    draw_id = rep(seq_along(ok), each = length(nms)),
    param = rep(nms, times = length(ok)),
    value = as.numeric(unlist(lapply(ok, function(p) unlist(p[nms])),
                              use.names = FALSE))
  )

  # `tstar()` returns Inf for a replicate whose curve never reaches the
  # threshold. That is passed through untouched: it is the outcome the project
  # reports as prob_no_cross, not a number to be tidied away.
  ts <- vapply(ok, function(p) tstar(model, p, point$c_thr), numeric(1))
  tstar_draws <- tibble::tibble(
    draw_id = seq_along(ok),
    estimand = "population",
    participant_id = NA_integer_,
    tstar_days = as.numeric(ts),
    crossed = is.finite(ts)
  )

  notes <- character(0)
  if (point$n_dropped > 0) {
    notes <- c(notes, sprintf("%d censored observation(s) dropped",
                              point$n_dropped))
  }
  if (n_failed > 0) {
    notes <- c(notes, sprintf("%d of %d bootstrap replicates failed",
                              n_failed, n_boot))
  }
  if (!is.na(point$optim$message)) notes <- c(notes, point$optim$message)

  out <- fit_result(
    cell_id = data$cell$cell_id,
    replicate = data$cell$replicate,
    method = "ls",
    model = model,
    params = params,
    tstar_draws = tstar_draws,
    diagnostics = default_diagnostics(
      converged = point$optim$convergence == 0L,
      n_draws = nrow(tstar_draws),
      runtime_s = as.numeric(difftime(Sys.time(), started, units = "secs")),
      message = if (length(notes) == 0L) {
        NA_character_
      } else {
        paste(notes, collapse = "; ")
      }
    ),
    ic = .ls_ic(point),
    provenance = default_provenance(
      engine = "stats::optim (Nelder-Mead then BFGS)",
      engine_version = paste(R.version$major, R.version$minor, sep = "."),
      seed = seed
    )
  )
  validate_fit_result(out)
}

#' Delta-method interval for the duration, for comparison with the bootstrap.
#'
#' Kept separate on purpose. If the two disagree materially, that disagreement
#' is a finding about the design, not a nuisance to be resolved by picking one.
#'
#' The delta method is applied to `log T*` rather than to `T*`, so the
#' interval cannot run below zero and is asymmetric in the direction the
#' sampling distribution actually is. The columns match [summarise_tstar()] so
#' that the two intervals can be stacked and plotted on one axis.
#'
#' @param fit the list returned by [fit_ls()].
#' @return a one-row tibble; `basis` distinguishes it from a bootstrap summary.
tstar_delta_interval <- function(fit, cfg, level = 0.90) {
  point <- tstar(fit$model, fit$par, fit$c_thr)
  out <- tibble::tibble(
    estimand = "population",
    tstar_med = as.numeric(point),
    tstar_lo = NA_real_,
    tstar_hi = NA_real_,
    interval_level = level,
    prob_no_cross = NA_real_,
    n_draws = NA_integer_,
    basis = "delta",
    se_log = NA_real_
  )

  # A point estimate that never crosses has no curvature-based interval: the
  # quantity being linearised is not finite. Say so rather than return a
  # number.
  if (!is.finite(point) || point <= 0 || is.null(fit$vcov)) {
    out$tstar_lo <- if (is.finite(point)) NA_real_ else Inf
    out$tstar_hi <- Inf
    return(out)
  }

  g <- .ls_num_grad(
    function(th) log(tstar(fit$model, .ls_from_working(th, fit$model),
                           fit$c_thr)),
    fit$theta
  )
  var_log <- drop(t(g) %*% fit$vcov %*% g)
  if (!is.finite(var_log) || var_log < 0) return(out)

  se <- sqrt(var_log)
  z <- stats::qnorm(1 - (1 - level) / 2)
  out$se_log <- se
  out$tstar_lo <- point * exp(-z * se)
  out$tstar_hi <- point * exp(z * se)
  out
}

# --- interval constructions, for comparison --------------------------------

#' The same draws, summarised several ways.
#'
#' A diagnostic, not a replacement. `summarise_tstar()` remains the single
#' place the project turns draws into the interval it scores; this exists so
#' that the choice it encodes can be examined rather than assumed, and so that
#' an adequacy or stopping rule written on interval width can be tried against
#' more than one definition of width.
#'
#' The constructions differ in what they assume about the shape of the
#' sampling distribution, which matters here because that distribution is
#' strongly right-skewed:
#'
#' * `percentile` takes the quantiles of the draws directly. Assumes nothing
#'   about shape, which is why it is the project default.
#' * `basic` reflects the draws about the point estimate. Assumes the bias is
#'   the same at both ends, which a skewed distribution violates.
#' * `bc` is bias-corrected: it shifts the quantiles by how far the point
#'   estimate sits from the median of the draws. Cheaper than BCa, which would
#'   need a jackknife, and it addresses the part of the skew that matters most
#'   here.
#' * `delta` is [tstar_delta_interval()], carried along for the comparison.
#'
#' **A non-crossing draw defeats two of these.** `basic` reflects `Inf` to
#' `-Inf` and `bc` cannot place a quantile past it, so both return `NA` rather
#' than a number when any draw is `Inf`. That is not a defect to be patched:
#' it says those constructions have no answer when the fit says the threshold
#' may never be reached, and a rule built on them would be silently undefined
#' in exactly the cells the project cares about.
#'
#' @param fit the list from [fit_ls()].
#' @param boot the [fit_result()] from [fit_ls_bootstrap()].
#' @param level nominal interval level.
#' @return one row per construction, with the columns of [summarise_tstar()]
#'   plus `basis`, so the rows can be stacked and plotted on one axis.
tstar_interval_variants <- function(fit, boot, cfg, level = 0.90) {
  d <- boot$tstar_draws[boot$tstar_draws$estimand == "population", ]
  draws <- d$tstar_days
  point <- tstar(fit$model, fit$par, fit$c_thr)
  a <- (1 - level) / 2
  any_inf <- any(!is.finite(draws))

  row <- function(basis, lo, hi) {
    tibble::tibble(
      estimand = "population", tstar_med = as.numeric(point),
      tstar_lo = as.numeric(lo), tstar_hi = as.numeric(hi),
      interval_level = level, prob_no_cross = mean(!d$crossed),
      n_draws = nrow(d), basis = basis
    )
  }

  q <- stats::quantile(draws, c(a, 1 - a), names = FALSE)
  out <- row("percentile", q[1], q[2])

  if (any_inf) {
    out <- rbind(out, row("basic", NA_real_, NA_real_),
                 row("bc", NA_real_, NA_real_))
  } else {
    hi_lo <- stats::quantile(draws, c(1 - a, a), names = FALSE)
    out <- rbind(out, row("basic", 2 * point - hi_lo[1], 2 * point - hi_lo[2]))

    # Bias correction: z0 measures how far the point estimate sits from the
    # median of the draws, on the normal scale. A symmetric bootstrap gives
    # z0 = 0 and the interval reduces to the percentile one.
    p_below <- mean(draws < point)
    if (p_below <= 0 || p_below >= 1) {
      out <- rbind(out, row("bc", NA_real_, NA_real_))
    } else {
      z0 <- stats::qnorm(p_below)
      z <- stats::qnorm(c(a, 1 - a))
      adj <- stats::pnorm(2 * z0 + z)
      qb <- stats::quantile(draws, adj, names = FALSE)
      out <- rbind(out, row("bc", qb[1], qb[2]))
    }
  }

  dl <- tstar_delta_interval(fit, cfg, level)
  rbind(out, row("delta", dl$tstar_lo, dl$tstar_hi))
}
