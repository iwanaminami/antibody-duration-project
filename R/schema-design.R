# ---------------------------------------------------------------------------
# Schema: design cell.
#
# FROZEN: owned by the coordinator (workstream a). See INTERFACES.md.
#
# One design cell is one row of the sweep: a trial design, a truth model and
# a replicate. Everything downstream is keyed on `cell_id` and `replicate`.
# ---------------------------------------------------------------------------

DESIGN_CELL_FIELDS <- c(
  "cell_id", "truth_model", "followup_days", "visits_per_year",
  "n_participants", "sigma_log", "heterogeneity", "c_thr", "lloq", "uloq",
  "replicate", "seed", "schema_version"
)

#' Construct a design cell.
#'
#' `cell_id` identifies the *design* and deliberately excludes `replicate`,
#' so replicates of the same design group together in caches and figures.
#' `seed` is derived from both, so a result is reproducible from the design
#' alone and a resumed sweep renumbers nothing.
#'
#' @param truth_model one of [TRUTH_MODELS].
#' @param followup_days length of follow-up, in days. Use
#'   [months_to_days()] when working from a design stated in months.
#' @param visits_per_year visit frequency (12 = monthly, 4 = quarterly).
#' @param n_participants number of participants recruited.
#' @param sigma_log SD of the log-normal assay error, handout Equation (5).
#' @param heterogeneity whether between-participant random effects are on.
#' @param c_thr protection threshold, on the same scale as the marker.
#' @param lloq,uloq assay quantification limits, or `NA` for none. These are
#'   first-class fields rather than defaults hidden inside a function, because
#'   censoring materially changes what the tail of the data can support.
#' @return an object of class `design_cell`.
design_cell <- function(truth_model = "biphasic",
                        followup_days,
                        visits_per_year,
                        n_participants,
                        sigma_log,
                        c_thr,
                        heterogeneity = FALSE,
                        lloq = NA_real_,
                        uloq = NA_real_,
                        replicate = 1L,
                        seed = NULL,
                        base_seed = 20260901L) {
  truth_model <- match.arg(truth_model, TRUTH_MODELS)
  cell_id <- sprintf(
    "%s-T%03d-f%g-N%d-s%g-het%d-%s",
    truth_model, round(followup_days), visits_per_year, n_participants,
    sigma_log, as.integer(isTRUE(heterogeneity)),
    .hash8(truth_model, followup_days, visits_per_year, n_participants,
           sigma_log, heterogeneity, c_thr, lloq, uloq)
  )
  x <- list(
    cell_id = cell_id,
    truth_model = truth_model,
    followup_days = as.numeric(followup_days),
    visits_per_year = as.numeric(visits_per_year),
    n_participants = as.integer(n_participants),
    sigma_log = as.numeric(sigma_log),
    heterogeneity = isTRUE(heterogeneity),
    c_thr = as.numeric(c_thr),
    lloq = as.numeric(lloq),
    uloq = as.numeric(uloq),
    replicate = as.integer(replicate),
    seed = as.integer(
      if (is.null(seed)) derive_seed(cell_id, replicate, base_seed) else seed
    ),
    schema_version = SCHEMA_VERSION
  )
  structure(x, class = "design_cell")
}

#' Validate a design cell, or stop with a message naming the problem.
validate_design_cell <- function(x) {
  what <- "design_cell"
  if (!inherits(x, "design_cell")) {
    .stop_schema(what, "object does not have class 'design_cell'")
  }
  .check_names(x, DESIGN_CELL_FIELDS, what)

  if (!is.character(x$cell_id) || length(x$cell_id) != 1L || !nzchar(x$cell_id)) {
    .stop_schema(what, "'cell_id' must be a single non-empty string")
  }
  if (!x$truth_model %in% TRUTH_MODELS) {
    .stop_schema(what, "'truth_model' must be one of ",
                 paste(TRUTH_MODELS, collapse = ", "))
  }
  .check_scalar_duration(x$followup_days, what, "followup_days")
  if (!is.finite(x$followup_days) || x$followup_days <= 0) {
    .stop_schema(what, "'followup_days' must be a positive finite number")
  }
  for (nm in c("visits_per_year", "sigma_log", "c_thr")) {
    v <- x[[nm]]
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v <= 0) {
      .stop_schema(what, "'", nm, "' must be a single positive finite number")
    }
  }
  if (!is.integer(x$n_participants) || length(x$n_participants) != 1L ||
        is.na(x$n_participants) || x$n_participants < 1L) {
    .stop_schema(what, "'n_participants' must be a single integer >= 1")
  }
  if (!is.logical(x$heterogeneity) || length(x$heterogeneity) != 1L ||
        is.na(x$heterogeneity)) {
    .stop_schema(what, "'heterogeneity' must be TRUE or FALSE")
  }
  for (nm in c("lloq", "uloq")) {
    v <- x[[nm]]
    if (!is.numeric(v) || length(v) != 1L || is.nan(v)) {
      .stop_schema(what, "'", nm, "' must be a single number or NA")
    }
    if (!is.na(v) && v <= 0) {
      .stop_schema(what, "'", nm, "' must be positive when present")
    }
  }
  if (!is.na(x$lloq) && !is.na(x$uloq) && x$lloq >= x$uloq) {
    .stop_schema(what, "'lloq' must be less than 'uloq'")
  }
  if (!is.integer(x$replicate) || length(x$replicate) != 1L ||
        is.na(x$replicate) || x$replicate < 1L) {
    .stop_schema(what, "'replicate' must be a single integer >= 1")
  }
  if (!is.integer(x$seed) || length(x$seed) != 1L || is.na(x$seed)) {
    .stop_schema(what, "'seed' must be a single integer")
  }
  invisible(x)
}

#' Expand a design grid into a list of validated design cells.
#'
#' Every combination of the vectors supplied, crossed with `1:n_replicates`.
#' Cells that differ only by replicate share a `cell_id`.
design_grid <- function(truth_model = "biphasic",
                        followup_days,
                        visits_per_year,
                        n_participants,
                        sigma_log,
                        c_thr,
                        heterogeneity = FALSE,
                        lloq = NA_real_,
                        uloq = NA_real_,
                        n_replicates = 1L,
                        base_seed = 20260901L) {
  grid <- expand.grid(
    truth_model = truth_model,
    followup_days = followup_days,
    visits_per_year = visits_per_year,
    n_participants = n_participants,
    sigma_log = sigma_log,
    heterogeneity = heterogeneity,
    c_thr = c_thr,
    lloq = lloq,
    uloq = uloq,
    replicate = seq_len(n_replicates),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  lapply(seq_len(nrow(grid)), function(i) {
    do.call(design_cell, c(as.list(grid[i, ]), list(base_seed = base_seed)))
  })
}

print.design_cell <- function(x, ...) {
  cat(sprintf(
    "<design_cell> %s  rep %d  seed %d\n  %s truth | %.0f days (%.1f months) | %g visits/yr | N = %d\n  sigma_log = %g | c_thr = %g | heterogeneity = %s | LLOQ/ULOQ = %s/%s\n",
    x$cell_id, x$replicate, x$seed, x$truth_model, x$followup_days,
    days_to_months(x$followup_days), x$visits_per_year, x$n_participants,
    x$sigma_log, x$c_thr, x$heterogeneity,
    format(x$lloq), format(x$uloq)
  ))
  invisible(x)
}
