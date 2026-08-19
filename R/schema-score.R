# ---------------------------------------------------------------------------
# Schema: score rows.
#
# FROZEN: owned by the coordinator (workstream a). See INTERFACES.md.
#
# One row per (cell_id, replicate, method, model, estimand). Design fields are
# repeated on every row on purpose: a score table is the thing that gets
# aggregated, filtered and plotted, and it should not need a join to be
# readable.
# ---------------------------------------------------------------------------

SCORE_SPEC <- list(
  cell_id         = list(type = "character"),
  replicate       = list(type = "integer", min = 1),
  method          = list(type = "character", values = FIT_METHODS),
  model           = list(type = "character", values = CANDIDATE_MODELS),
  estimand        = list(type = "character", values = ESTIMANDS),

  truth_model     = list(type = "character", values = TRUTH_MODELS),
  followup_days   = list(type = "double", min = 0),
  visits_per_year = list(type = "double", min = 0),
  n_participants  = list(type = "integer", min = 1),
  sigma_log       = list(type = "double", min = 0),
  heterogeneity   = list(type = "logical"),
  c_thr           = list(type = "double", min = 0),

  tstar_true_days = list(type = "double", min = 0, inf_ok = TRUE),
  tstar_med       = list(type = "double", min = 0, inf_ok = TRUE, na_ok = TRUE),
  tstar_lo        = list(type = "double", min = 0, inf_ok = TRUE, na_ok = TRUE),
  tstar_hi        = list(type = "double", min = 0, inf_ok = TRUE, na_ok = TRUE),
  interval_level  = list(type = "double", min = 0, max = 1),

  bias            = list(type = "double", inf_ok = TRUE, na_ok = TRUE),
  abs_err         = list(type = "double", min = 0, inf_ok = TRUE, na_ok = TRUE),
  sq_err          = list(type = "double", min = 0, inf_ok = TRUE, na_ok = TRUE),
  covered         = list(type = "logical", na_ok = TRUE),
  width           = list(type = "double", min = 0, inf_ok = TRUE, na_ok = TRUE),
  rel_width       = list(type = "double", min = 0, inf_ok = TRUE, na_ok = TRUE),
  prob_no_cross   = list(type = "double", min = 0, max = 1),
  adequate        = list(type = "logical", na_ok = TRUE)
)

SCORE_COLUMNS <- names(SCORE_SPEC)

#' Construct and validate a table of score rows.
#'
#' Extra columns are welcome -- a workstream may carry its own diagnostics
#' alongside -- but the columns in [SCORE_COLUMNS] must all be present.
#'
#' `NA` is the right value when a quantity does not apply (an interval width
#' when both ends are `Inf`, coverage when the truth is `Inf`). `NaN` is never
#' acceptable and the validator rejects it: it means an arithmetic mistake,
#' most often `Inf - Inf`.
#'
#' @param df a data frame with at least [SCORE_COLUMNS].
#' @return a tibble of class `score_rows`.
score_rows <- function(df) {
  df <- tibble::as_tibble(df)
  validate_score_rows(structure(
    df,
    class = unique(c("score_rows", class(df)))
  ))
}

#' Validate score rows, or stop with a message naming the problem.
validate_score_rows <- function(x) {
  what <- "score_rows"
  if (!inherits(x, "score_rows")) {
    .stop_schema(what, "object does not have class 'score_rows'")
  }
  check_cols(x, SCORE_SPEC, what)
  check_days_cols(x, what)

  ok <- !is.na(x$tstar_lo) & !is.na(x$tstar_hi)
  if (any(x$tstar_hi[ok] < x$tstar_lo[ok])) {
    .stop_schema(what, "'tstar_hi' must be >= 'tstar_lo'")
  }
  key <- paste(x$cell_id, x$replicate, x$method, x$model, x$estimand,
               sep = "|")
  if (anyDuplicated(key) > 0) {
    dup <- key[duplicated(key)][1]
    .stop_schema(what, "rows must be unique on ",
                 "(cell_id, replicate, method, model, estimand); ",
                 "duplicated: ", dup)
  }
  invisible(x)
}

#' An empty, schema-valid score table.
#'
#' Useful as the accumulator a sweep binds onto, and as a shape to test
#' against before any real scores exist.
empty_score_rows <- function() {
  proto <- lapply(SCORE_SPEC, function(s) {
    switch(s$type,
           integer = integer(0),
           double = numeric(0),
           character = character(0),
           logical = logical(0))
  })
  score_rows(tibble::as_tibble(proto))
}
