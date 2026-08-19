# ---------------------------------------------------------------------------
# Figures.
#
# OWNER: workstream (a), coordinator. Contract in INTERFACES.md.
#
# Every figure takes an aggregated score table and returns a ggplot object;
# saving is the caller's job, so figures can be composed and re-used. Captions
# must state the estimand, the truth model, the fitted family and the design
# -- a duration figure that does not say which estimand it shows is not
# interpretable.
#
# Label axes in months and keep the data in days: use days_to_months().
# ---------------------------------------------------------------------------

#' Bias, precision and coverage against length of follow-up.
#'
#' The core Phase 1 display: how much the estimate improves per extra month of
#' follow-up, which is the question a trial review actually faces.
plot_data_requirement <- function(agg, cfg) {
  .not_implemented("plot_data_requirement", "(a) coordinator")
}

#' Adequacy over follow-up by visit frequency and sample size.
#'
#' The practical read-off: which designs clear the adequacy rule.
plot_minimum_viable_design <- function(agg, cfg) {
  .not_implemented("plot_minimum_viable_design", "(a) coordinator")
}

#' Extrapolation bias against follow-up, by fitted family.
#'
#' Shows the direction as well as the size of the error: a family that
#' extends the early rate of decline indefinitely and one that flattens too
#' soon are wrong in opposite directions, and the average of the two hides it.
plot_extrapolation_bias <- function(agg, cfg) {
  .not_implemented("plot_extrapolation_bias", "(a) coordinator")
}

#' How often each candidate family is selected, by design.
plot_model_selection <- function(selection, cfg) {
  .not_implemented("plot_model_selection", "(a) coordinator")
}

#' One dataset with its fitted curve and its extrapolated tail.
#'
#' The explanatory figure: observed points, the fit over the observed window,
#' the extrapolation beyond it with an interval, the threshold, and the true
#' crossing time. Makes the size of the extrapolation visible.
plot_example_fit <- function(data, fits, cfg) {
  .not_implemented("plot_example_fit", "(a) coordinator")
}

#' Individual against population duration under heterogeneity.
#'
#' Whether recruiting more participants helps depends on which estimand is
#' being asked about, so the comparison needs to be explicit.
plot_estimand_comparison <- function(agg, cfg) {
  .not_implemented("plot_estimand_comparison", "(a) coordinator")
}
