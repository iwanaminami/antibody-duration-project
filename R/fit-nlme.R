# ---------------------------------------------------------------------------
# Nonlinear mixed-effects models: individual and population durations.
#
# OWNER: workstream (d). Contract in INTERFACES.md.
#
# This is where the two estimands come apart. Fitting each participant
# separately wastes the information that they share a population; fitting the
# mean curve answers a different question from the one about individuals. A
# mixed-effects model estimates both, and the project needs both: the
# distribution of individual durations, and the population target defined in
# handout Section 2.
#
# The practical question this workstream owns is whether recruiting more
# participants substitutes for measuring each participant more often. It does
# not follow that it does -- more participants sharpen a population quantity
# without necessarily sharpening an individual one -- so measure it rather
# than assuming either way.
#
# Engine is this workstream's choice: nlme::nlme, saemix, or nlmixr2, all in
# Suggests. Fixed output: draws for both estimands in `fit_result$tstar_draws`,
# with `estimand = "individual"` rows carrying a `participant_id` and
# `estimand = "population"` rows carrying NA.
# ---------------------------------------------------------------------------

#' Fit a nonlinear mixed-effects model to a simulated dataset.
#'
#' Random effects on the peak level `c0` and the slow half-life `h2`, matching
#' the generating structure of handout Equation (6). Report the estimated
#' variance components, not only the fixed effects: `omega_h` is what governs
#' how much individual durations differ.
#'
#' @param data a [sim_dataset()].
#' @param model one of [CANDIDATE_MODELS].
#' @param cfg configuration from [read_config()].
#' @param engine identifier recorded in `$provenance$engine`.
#' @return a [fit_result()] with `method = "nlme"`, carrying draws for both
#'   estimands. Convergence of these fits is not a given -- record failures in
#'   `$diagnostics` rather than dropping them silently, because which cells
#'   failed is itself a result about the design.
fit_nlme <- function(data, model = "biphasic", cfg, engine = "nlme", ...) {
  .not_implemented("fit_nlme", "(d) NLME")
}

#' Duration draws for individual participants from a fitted NLME model.
#'
#' Empirical Bayes estimates of each participant's own trajectory, propagated
#' through [tstar()]. Whether the individual uncertainty includes the
#' shrinkage uncertainty is a choice to make explicitly and document.
nlme_individual_tstar <- function(fit, cfg, n_draws = 500L) {
  .not_implemented("nlme_individual_tstar", "(d) NLME")
}

#' The population duration target from a fitted NLME model.
#'
#' Not the crossing time of the mean curve. The handout suggests the time at
#' which the proportion of participants still above the threshold falls below
#' a chosen value; whichever definition the workshop settles on, use the same
#' one in the simulator's `$meta$population_tstar_days` so that the score
#' compares like with like.
nlme_population_tstar <- function(fit, cfg, n_draws = 500L) {
  .not_implemented("nlme_population_tstar", "(d) NLME")
}

#' More participants against more visits per participant, at fixed cost.
#'
#' Returns the comparison for both estimands. The interesting result is
#' whether they point the same way.
participants_versus_visits <- function(agg, cfg) {
  .not_implemented("participants_versus_visits", "(d) NLME")
}
