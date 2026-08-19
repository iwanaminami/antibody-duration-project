# ---------------------------------------------------------------------------
# Scoring: metrics, adequacy rules, aggregation.
#
# OWNER: workstream (a), coordinator, with workstream (e) on the interpretation
# of the threshold. Contract in INTERFACES.md.
#
# Scoring reads `fit_result$tstar_draws` and knows nothing about how the draws
# were produced. That is deliberate: it is what allows least squares, MCMC and
# NLME to be compared on one axis.
#
# The awkward case is the non-crossing draw. An interval that runs to Inf does
# contain the truth, so coverage alone will call it a success. It is not one.
# Report `prob_no_cross` alongside coverage and never let a mostly
# non-crossing fit pass the adequacy rule on coverage alone.
# ---------------------------------------------------------------------------

#' Score one fit against the truth that generated it.
#'
#' @param fit a [fit_result()].
#' @param data the [sim_dataset()] it was fitted to, for the true durations.
#' @param cfg configuration; supplies the interval level and adequacy rules.
#' @param estimand one of [ESTIMANDS].
#' @return one row of [score_rows()].
score_fit <- function(fit, data, cfg, estimand = "population") {
  .not_implemented("score_fit", "(a) coordinator")
}

#' Aggregate replicate-level scores into design-level summaries.
#'
#' Group by the design fields, the method, the model and the estimand; return
#' coverage, median bias, RMSE, median relative width, the non-crossing
#' fraction and the adequacy flag. Report the number of replicates behind each
#' summary: a coverage of 1.00 from three replicates is not a result.
aggregate_scores <- function(scores, cfg) {
  .not_implemented("aggregate_scores", "(a) coordinator")
}

#' Apply the adequacy rule from the configuration.
#'
#' Provisionally adequate when empirical coverage is at least
#' `scoring$min_coverage`, the median relative width is at most
#' `scoring$max_rel_width`, and `prob_no_cross` is at most
#' `scoring$max_prob_no_cross`. Over-coverage on its own is not a failure.
mark_adequate <- function(agg, cfg) {
  .not_implemented("mark_adequate", "(a) coordinator")
}

#' The smallest adequate design in each family.
#'
#' The project's central deliverable: for each fitted model, the shortest
#' follow-up, lowest visit frequency and smallest sample size that still meet
#' the adequacy rule.
minimum_viable_design <- function(agg, cfg) {
  .not_implemented("minimum_viable_design", "(a) coordinator")
}
