# ---------------------------------------------------------------------------
# Candidate trajectory families and the duration estimand.
#
# FROZEN: owned by the coordinator (workstream a). Every workstream must use
# these curves so that a duration produced by least squares, MCMC and NLME
# means the same thing. Transcribed from notes/handout.tex, Section 4.
# ---------------------------------------------------------------------------

# --- curves ----------------------------------------------------------------

#' Single exponential decay.
#'
#' \eqn{c(t) = c_0 e^{-kt}} (handout Section 4, "Single exponential").
curve_exponential <- function(t, c0, k) {
  c0 * exp(-k * t)
}

#' Biphasic exponential decay, the suggested reference family.
#'
#' Following Hogan et al. (2023), as written in the handout:
#'
#' \deqn{c(t) = c_0 \frac{e^{-\log 2 (t/h_1 + t_s/h_2)} +
#'                        e^{-\log 2 (t/h_2 + t_s/h_1)}}
#'                       {e^{-\log 2\, t_s/h_1} + e^{-\log 2\, t_s/h_2}}}
#'
#' The parameterisation guarantees \eqn{c(0) = c_0} and equal contributions
#' from the two components at the switching time \eqn{t_s}. Evaluated through
#' a log-sum-exp so that long horizons do not underflow.
#'
#' @param h1 fast half-life in days, `h1 < h2`.
#' @param h2 slow half-life in days; this is what governs the long-term tail.
#' @param ts switching time in days.
curve_biphasic <- function(t, c0, h1, h2, ts) {
  if (any(h1 >= h2)) {
    stop("curve_biphasic(): the fast half-life h1 must be less than h2",
         call. = FALSE)
  }
  l2 <- log(2)
  log_num <- .logaddexp(-l2 * (t / h1 + ts / h2), -l2 * (t / h2 + ts / h1))
  log_den <- .logaddexp(-l2 * ts / h1, -l2 * ts / h2)
  c0 * exp(log_num - log_den)
}

#' Power-law decay.
#'
#' \eqn{c(t) = c_0 (1 + \alpha t)^{-\beta}}. The rate of decline slows
#' continuously, so a short window can look exponential while the model
#' predicts far longer persistence.
curve_powerlaw <- function(t, c0, alpha, beta) {
  c0 * (1 + alpha * t)^(-beta)
}

#' Plateau (set-point) decay.
#'
#' \eqn{c(t) = A_\infty + (c_0 - A_\infty)e^{-kt}}. Crosses the threshold only
#' when \eqn{A_\infty < c_{thr}}, so this family is the main source of
#' non-crossing draws.
curve_plateau <- function(t, c0, A_inf, k) {
  A_inf + (c0 - A_inf) * exp(-k * t)
}

.logaddexp <- function(a, b) {
  m <- pmax(a, b)
  m + log1p(exp(-abs(a - b)))
}

# --- dispatch --------------------------------------------------------------

#' The parameter names of a candidate family, in a fixed order.
curve_params <- function(model) {
  switch(
    match.arg(model, CANDIDATE_MODELS),
    exponential = c("c0", "k"),
    biphasic    = c("c0", "h1", "h2", "ts"),
    powerlaw    = c("c0", "alpha", "beta"),
    plateau     = c("c0", "A_inf", "k")
  )
}

#' The curve function for a candidate family.
curve_fun <- function(model) {
  switch(
    match.arg(model, CANDIDATE_MODELS),
    exponential = curve_exponential,
    biphasic    = curve_biphasic,
    powerlaw    = curve_powerlaw,
    plateau     = curve_plateau
  )
}

#' Evaluate a candidate family at times `t` from a named parameter list.
#'
#' Keeps downstream code generic: nothing outside this file should need to
#' know how many parameters a family has.
#'
#' @param params named list or vector holding exactly `curve_params(model)`.
curve_eval <- function(model, t, params) {
  model <- match.arg(model, CANDIDATE_MODELS)
  nms <- curve_params(model)
  params <- as.list(params)
  missing_params <- setdiff(nms, names(params))
  if (length(missing_params) > 0) {
    stop(sprintf("curve_eval(): model '%s' needs parameter(s) %s",
                 model, paste(missing_params, collapse = ", ")), call. = FALSE)
  }
  do.call(curve_fun(model), c(list(t = t), params[nms]))
}

# --- the duration estimand -------------------------------------------------

# Times beyond this are treated as "never crosses". Roughly 2700 years: far
# outside anything the project can speak to, so reporting Inf is honest.
.TSTAR_SEARCH_CAP_DAYS <- 1e6

#' Time at which a trajectory first falls to or below the threshold.
#'
#' This is \eqn{T^* = \inf\{t \ge 0 : c(t) \le c_{thr}\}} from the handout,
#' Equation (1). Closed form where one exists, `uniroot()` otherwise.
#'
#' Returns `Inf` when the trajectory settles above the threshold, or when it
#' has not crossed by `horizon_days`. **Never substitute a large finite number
#' for `Inf`.** A non-crossing draw is a distinct outcome that downstream
#' scoring reports as `prob_no_cross`; hiding it inside a big number silently
#' biases every interval that contains it.
#'
#' @param params named list; each element may be a scalar or a vector of
#'   common length `n`, which is how per-participant durations are computed
#'   under heterogeneity.
#' @param horizon_days largest time considered; `Inf` uses the internal cap.
#' @return numeric vector of length `n`, possibly containing `Inf`.
tstar <- function(model, params, c_thr, horizon_days = Inf) {
  model <- match.arg(model, CANDIDATE_MODELS)
  if (!is.numeric(c_thr) || length(c_thr) != 1L || is.na(c_thr) || c_thr <= 0) {
    stop("tstar(): c_thr must be a single positive number", call. = FALSE)
  }
  nms <- curve_params(model)
  params <- as.list(params)
  missing_params <- setdiff(nms, names(params))
  if (length(missing_params) > 0) {
    stop(sprintf("tstar(): model '%s' needs parameter(s) %s",
                 model, paste(missing_params, collapse = ", ")), call. = FALSE)
  }
  params <- params[nms]
  n <- max(vapply(params, length, integer(1)))
  params <- lapply(params, function(x) rep_len(x, n))
  cap <- if (is.finite(horizon_days)) horizon_days else .TSTAR_SEARCH_CAP_DAYS

  vapply(seq_len(n), function(i) {
    p <- lapply(params, `[[`, i)
    .tstar_one(model, p, c_thr, cap)
  }, numeric(1))
}

.tstar_one <- function(model, p, c_thr, cap) {
  if (any(vapply(p, function(x) is.na(x) || is.nan(x), logical(1)))) {
    return(NA_real_)
  }
  if (curve_eval(model, 0, p) <= c_thr) return(0)

  analytic <- switch(
    model,
    exponential = if (p$k > 0) log(p$c0 / c_thr) / p$k else Inf,
    powerlaw = if (p$alpha > 0 && p$beta > 0) {
      ((p$c0 / c_thr)^(1 / p$beta) - 1) / p$alpha
    } else {
      Inf
    },
    plateau = if (p$A_inf >= c_thr || p$k <= 0) {
      Inf
    } else {
      -log((c_thr - p$A_inf) / (p$c0 - p$A_inf)) / p$k
    },
    NULL
  )
  if (!is.null(analytic)) {
    return(if (analytic > cap) Inf else analytic)
  }

  # Numeric families (biphasic, and anything added later): the curves are
  # monotone decreasing, so bracket by doubling and then root-find on the log
  # scale, which is far better conditioned once the level is small.
  if (curve_eval(model, cap, p) > c_thr) return(Inf)
  upper <- 1
  while (upper < cap && curve_eval(model, upper, p) > c_thr) upper <- upper * 2
  upper <- min(upper, cap)
  f <- function(t) log(curve_eval(model, t, p)) - log(c_thr)
  uniroot(f, lower = 0, upper = upper, tol = 1e-8)$root
}
