# ---------------------------------------------------------------------------
# Alternate models, model selection, and Bayesian inference.
#
# OWNER: workstream (c). Contract in INTERFACES.md.
#
# Two jobs. First, fit the full set of candidate families from handout
# Section 4 -- exponential, biphasic, power law, plateau -- without assuming
# which is correct, and quantify how much of the uncertainty in the duration
# comes from the choice of family rather than from within any one of them.
# Second, provide the Bayesian fits that give honest intervals where the
# curvature-based ones cannot.
#
# THE ENGINE IS THIS WORKSTREAM'S CHOICE. Stan through cmdstanr, JAGS through
# rjags, brms, or a hand-written sampler are all acceptable; all of them are
# in Suggests rather than Imports so that nobody else has to install a
# toolchain. What is fixed is the output: posterior draws in
# `fit_result$tstar_draws`, and whatever diagnostics the engine provides
# recorded in `$diagnostics`. Keep priors fixed across design cells, so that
# differences between cells reflect the data and not per-cell tuning.
#
# The plateau family needs a decision before it is included in comparisons:
# its duration is undefined whenever the set-point sits above the threshold,
# so a naive model average over families is polluted by infinities. Record
# what you decide and why.
# ---------------------------------------------------------------------------

#' Fit one candidate family by MCMC.
#'
#' @param data a [sim_dataset()].
#' @param model one of [CANDIDATE_MODELS].
#' @param cfg configuration from [read_config()].
#' @param engine identifier recorded in `$provenance$engine`.
#' @return a [fit_result()] with `method = "mcmc"`. Populate `$diagnostics`
#'   with `rhat_max`, `ess_bulk_min` and `divergences`, and set `converged`
#'   from an explicit rule agreed with the group -- filtering after the fact
#'   on an undeclared rule changes what the aggregate means.
fit_mcmc <- function(data, model = "biphasic", cfg, engine = "stan", ...) {
  .not_implemented("fit_mcmc", "(c) alternate models and MCMC")
}

#' Fit every candidate family to the same dataset.
#'
#' @return a named list of [fit_result()], one per family.
fit_all_models <- function(data, cfg, models = CANDIDATE_MODELS, ...) {
  .not_implemented("fit_all_models", "(c) alternate models and MCMC")
}

#' Compare candidate families on one dataset.
#'
#' Return the information criteria per family and the selected family. Whether
#' selection uses AIC, BIC, WAIC or LOO is this workstream's call; record it in
#' the returned table so a figure can state it.
compare_models <- function(fits, cfg) {
  .not_implemented("compare_models", "(c) alternate models and MCMC")
}

#' Pool duration draws across families by their weights.
#'
#' Model averaging is not automatically an improvement: it can be worse than
#' the best single family, and it is sensitive to how non-crossing draws are
#' treated. Report it beside the individual families rather than in place of
#' them.
#'
#' @return a [fit_result()] whose `$tstar_draws` are the pooled draws.
model_average_tstar <- function(fits, cfg, weights = "aic") {
  .not_implemented("model_average_tstar", "(c) alternate models and MCMC")
}
