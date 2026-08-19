# ---------------------------------------------------------------------------
# Calibration: threshold, assay limits, and the link to protection.
#
# OWNER: workstream (e). Contract in INTERFACES.md.
#
# Every duration in this project is a statement about a threshold. Change the
# threshold and every number changes with it, which makes calibration a
# first-class part of the analysis rather than a preliminary. The same goes
# for the assay quantification limits: censoring in the tail removes exactly
# the measurements that identify the slow half-life.
#
# This workstream owns three things: confirming that the configuration's
# calibration is defensible and internally consistent; producing the
# sensitivity runs that show how much the conclusions depend on it; and
# building the optional link from an antibody trajectory to a protection
# curve, which makes explicit that antibody persistence and protection are
# related but not the same thing (handout Section 2).
# ---------------------------------------------------------------------------

#' Check that a configuration's calibration hangs together.
#'
#' The starting values, the threshold and the assay scale come from different
#' sources, and they have to be on the same scale to mean anything. Verify
#' that they are: that the marker is expressed relative to the mean
#' convalescent level, that the threshold sits below the early level, and that
#' the implied duration is a plausible extrapolation rather than an artefact.
#'
#' @return a data frame of checks with pass/fail and a short reason, so the
#'   result can go straight into the results memo.
check_calibration <- function(cfg) {
  .not_implemented("check_calibration", "(e) calibration")
}

#' Configurations spanning the calibration uncertainty.
#'
#' The handout gives ranges as well as point values: the slow half-life h2 is
#' the one that matters most (581 days, with 250-900 worth considering), and
#' the threshold differs by an order of magnitude between the symptomatic and
#' severe endpoints. Return one configuration per scenario, labelled, ready to
#' run through the same sweep.
calibration_scenarios <- function(cfg,
                                  h2_days = c(250, 581, 900),
                                  thresholds = c("symptomatic", "severe")) {
  .not_implemented("calibration_scenarios", "(e) calibration")
}

#' Protection against a clinical endpoint, given a marker level.
#'
#' The logistic relationship of Khoury et al. (2021), turning a titre into a
#' probability of protection. This is the alternative to a hard threshold: it
#' replaces "protected until T*" with a protection curve that declines.
#'
#' @param endpoint "symptomatic" or "severe".
protection_from_titre <- function(c, cfg, endpoint = "symptomatic") {
  .not_implemented("protection_from_titre", "(e) calibration")
}

#' Time at which protection falls below a chosen level.
#'
#' The protection-curve analogue of [tstar()]. Returns `Inf` when protection
#' never falls that far, on the same convention as everything else.
time_to_protection_level <- function(fit, cfg, level = 0.5,
                                     endpoint = "symptomatic") {
  .not_implemented("time_to_protection_level", "(e) calibration")
}

#' Sensitivity of the assay quantification limits.
#'
#' Censoring in the tail removes the measurements that identify the slow
#' half-life, so an LLOQ can matter as much as a shorter follow-up. Quantify
#' the trade.
lloq_sensitivity <- function(agg, cfg) {
  .not_implemented("lloq_sensitivity", "(e) calibration")
}
