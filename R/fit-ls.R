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
# ---------------------------------------------------------------------------

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
fit_ls <- function(data, model = "biphasic", cfg, start = NULL) {
  .not_implemented("fit_ls", "(b) simple analysis")
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
  .not_implemented("fit_ls_bootstrap", "(b) simple analysis")
}

#' Delta-method interval for the duration, for comparison with the bootstrap.
#'
#' Kept separate on purpose. If the two disagree materially, that disagreement
#' is a finding about the design, not a nuisance to be resolved by picking one.
tstar_delta_interval <- function(fit, cfg, level = 0.90) {
  .not_implemented("tstar_delta_interval", "(b) simple analysis")
}
