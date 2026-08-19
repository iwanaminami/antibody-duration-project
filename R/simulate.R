# ---------------------------------------------------------------------------
# Simulation: truth models, observation process, and reduction to a review.
#
# OWNER: workstream (a), coordinator. Part of the shared core -- everyone
# downstream consumes what this produces, so changes to the returned shape go
# through group review. Contract in INTERFACES.md.
#
# The central idea (handout Section 3): simulate one complete antibody history
# per participant, then recreate what a trial review would have seen by hiding
# everything after the review date. Keeping `simulate_full()` independent of
# the follow-up length is what makes the follow-up comparison clean -- the same
# underlying history is truncated at 3, 6, 12 months, so differences between
# reviews are information, not noise.
# ---------------------------------------------------------------------------

#' Simulate complete antibody histories for one design cell.
#'
#' Deterministic given `cell$seed`. Must not depend on `cell$followup_days` or
#' `cell$visits_per_year`: those belong to [reduce_to_review()].
#'
#' @param cell a [design_cell()].
#' @param cfg configuration from [read_config()].
#' @param horizon_days length of the simulated history, long enough that every
#'   participant's true duration is resolved.
#' @param dense_visits_per_year sampling density of the stored history.
#' @return a [sim_dataset()] over the full horizon, passing
#'   [validate_sim_dataset()]. `$truth$tstar_days` comes from [tstar()] and is
#'   `Inf` where a participant never crosses; `$meta$population_tstar_days`
#'   holds the population target.
simulate_full <- function(cell, cfg, horizon_days = 1200,
                          dense_visits_per_year = 52) {
  .not_implemented("simulate_full", "(a) coordinator")
}

#' Reduce a complete history to the data available at a trial review.
#'
#' Truncate at `cell$followup_days`, thin to `cell$visits_per_year`, and apply
#' the assay limits in `cell$lloq` / `cell$uloq` as censoring.
#'
#' @param full the output of [simulate_full()].
#' @param cell the [design_cell()] describing the review.
#' @return a [sim_dataset()] carrying `cell`, passing [validate_sim_dataset()].
#'   `$truth` is unchanged: the truth does not depend on what was observed.
reduce_to_review <- function(full, cell) {
  .not_implemented("reduce_to_review", "(a) coordinator")
}

#' Simulate and reduce in one step.
#'
#' The convenience path most callers want.
make_dataset <- function(cell, cfg, ...) {
  reduce_to_review(simulate_full(cell, cfg, ...), cell)
}

#' Apply the log-normal observation model of handout Equation (5).
#'
#' `log y = log c(t) + e`, `e ~ N(0, sigma_log^2)`, then censoring at the
#' quantification limits. Censored rows carry `NA` in `y_obs`; see
#' [sim_dataset()] for why the limit is not substituted for the value.
observe <- function(c_true, sigma_log, lloq = NA_real_, uloq = NA_real_) {
  .not_implemented("observe", "(a) coordinator")
}

#' Draw correlated per-participant random effects on `c0` and `h2`.
#'
#' Handout Equation (6): log-normal effects with log-SDs `omega_0`, `omega_h`
#' and correlation `rho_0h`, all read from `cfg$calibration$heterogeneity`.
#'
#' @return a data frame of `participant_id`, `eta_0`, `eta_h`.
draw_random_effects <- function(n, cfg) {
  .not_implemented("draw_random_effects", "(a) coordinator")
}

#' Simulate from the mechanistic antibody-production model.
#'
#' The three-compartment ODE of handout Section 4 (short- and long-lived
#' antibody-secreting cells plus antibody clearance), used to generate test
#' data whose shape the fitted families do not exactly contain.
simulate_mechanistic <- function(cell, cfg, horizon_days = 1200, ...) {
  .not_implemented("simulate_mechanistic", "(a) coordinator")
}
