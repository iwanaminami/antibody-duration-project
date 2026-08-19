# ---------------------------------------------------------------------------
# Schema: simulated dataset.
#
# FROZEN: owned by the coordinator (workstream a). See INTERFACES.md.
#
# What a trial review would actually have in hand, plus the truth that only a
# simulation study gets to know. Analysis code must read `$obs` and nothing
# else; `$truth` and `$meta` exist for scoring.
# ---------------------------------------------------------------------------

OBS_SPEC <- list(
  participant_id = list(type = "integer", min = 1),
  time_days      = list(type = "double", min = 0),
  y_obs          = list(type = "double", min = 0, na_ok = TRUE),
  censor         = list(type = "character", values = c("none", "left", "right"))
)

#' Construct a simulated dataset.
#'
#' @param cell the [design_cell()] that generated it.
#' @param obs observations, one row per measurement:
#'   `participant_id`, `time_days`, `y_obs`, `censor`. A censored measurement
#'   carries `NA` in `y_obs` and records which limit it fell outside in
#'   `censor`; the limit itself is recoverable from `cell$lloq` / `cell$uloq`.
#'   Storing the limit in `y_obs` would let a naive fit treat it as an
#'   observation, which is exactly the mistake censoring is meant to avoid.
#' @param truth one row per participant: `participant_id`, the generating
#'   parameters (one column each), and `tstar_days` (`Inf` permitted).
#' @param population_tstar_days the population-level duration target for this
#'   dataset, as distinct from a summary of the individual `tstar_days`.
#' @return an object of class `sim_dataset`.
sim_dataset <- function(cell, obs, truth, population_tstar_days,
                        extra_meta = list()) {
  x <- list(
    cell = cell,
    obs = tibble::as_tibble(obs),
    truth = tibble::as_tibble(truth),
    meta = utils::modifyList(
      list(
        population_tstar_days = as.numeric(population_tstar_days),
        truth_model = cell$truth_model,
        seed = cell$seed,
        schema_version = SCHEMA_VERSION
      ),
      extra_meta
    )
  )
  structure(x, class = "sim_dataset")
}

#' Validate a simulated dataset, or stop with a message naming the problem.
validate_sim_dataset <- function(x) {
  what <- "sim_dataset"
  if (!inherits(x, "sim_dataset")) {
    .stop_schema(what, "object does not have class 'sim_dataset'")
  }
  .check_names(x, c("cell", "obs", "truth", "meta"), what)
  validate_design_cell(x$cell)
  .check_names(x$meta,
               c("population_tstar_days", "truth_model", "seed",
                 "schema_version"),
               "sim_dataset$meta")

  check_cols(x$obs, OBS_SPEC, "sim_dataset$obs")
  check_days_cols(x$obs, "sim_dataset$obs")
  if (nrow(x$obs) == 0L) {
    .stop_schema(what, "'obs' has no rows")
  }

  # A censored row carries no value; an uncensored row must have one.
  censored <- x$obs$censor != "none"
  if (any(censored & !is.na(x$obs$y_obs))) {
    .stop_schema("sim_dataset$obs",
                 "censored rows must have y_obs = NA; the limit lives in ",
                 "cell$lloq / cell$uloq")
  }
  if (any(!censored & is.na(x$obs$y_obs))) {
    .stop_schema("sim_dataset$obs",
                 "uncensored rows must have a non-missing y_obs")
  }
  if (any(x$obs$time_days > x$cell$followup_days + 1e-8)) {
    .stop_schema("sim_dataset$obs",
                 "observation times must not exceed cell$followup_days (",
                 x$cell$followup_days, ")")
  }

  check_cols(x$truth,
             list(participant_id = list(type = "integer", min = 1),
                  tstar_days = list(type = "double", min = 0, inf_ok = TRUE)),
             "sim_dataset$truth")
  check_days_cols(x$truth, "sim_dataset$truth")

  ids_obs <- sort(unique(x$obs$participant_id))
  ids_truth <- sort(unique(x$truth$participant_id))
  if (!identical(as.integer(ids_obs), as.integer(ids_truth))) {
    .stop_schema(what, "the participants in 'obs' and 'truth' differ")
  }
  if (length(ids_truth) != x$cell$n_participants) {
    .stop_schema(what, "'truth' has ", length(ids_truth),
                 " participants but the cell specifies ",
                 x$cell$n_participants)
  }
  if (anyDuplicated(x$truth$participant_id) > 0) {
    .stop_schema("sim_dataset$truth", "'participant_id' must be unique")
  }
  .check_scalar_duration(x$meta$population_tstar_days, "sim_dataset$meta",
                         "population_tstar_days")
  invisible(x)
}

print.sim_dataset <- function(x, ...) {
  n_cens <- sum(x$obs$censor != "none")
  cat(sprintf(
    "<sim_dataset> %s  rep %d\n  %d observations from %d participants (%d censored)\n  times %.0f-%.0f days | population T* = %s days\n",
    x$cell$cell_id, x$cell$replicate, nrow(x$obs),
    length(unique(x$obs$participant_id)), n_cens,
    min(x$obs$time_days), max(x$obs$time_days),
    format(x$meta$population_tstar_days)
  ))
  invisible(x)
}
