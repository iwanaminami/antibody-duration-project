# ---------------------------------------------------------------------------
# Configuration.
#
# FROZEN: owned by the coordinator (workstream a). See INTERFACES.md.
#
# Every number that a result depends on lives in the configuration file, not
# in a function default. That includes the protection threshold and the assay
# quantification limits: they change what the tail of the data can support, so
# they must be visible, versioned and easy to vary in a sensitivity run.
# ---------------------------------------------------------------------------

#' Path to the packaged default configuration.
default_config_path <- function() {
  p <- system.file("config", "default.yaml", package = "antibodyduration")
  if (!nzchar(p)) {
    stop("default_config_path(): packaged configuration not found; ",
         "load the package with pkgload::load_all() from the repository root",
         call. = FALSE)
  }
  p
}

#' Read a configuration file, optionally overriding individual entries.
#'
#' Derived quantities are added on read so that no caller has to repeat a
#' conversion: `design$followup_days` from `design$followup_months`, and
#' `calibration$c_thr` resolved from the named thresholds.
#'
#' @param path YAML file; defaults to the packaged configuration.
#' @param overrides a nested list merged over the file contents, e.g.
#'   `list(design = list(n_replicates = 2))`.
read_config <- function(path = default_config_path(), overrides = list()) {
  cfg <- read_yaml(path)
  if (length(overrides) > 0) cfg <- .merge_config(cfg, overrides)
  cfg$.path <- path
  .derive_config(cfg)
}

.merge_config <- function(base, over) {
  for (nm in names(over)) {
    if (is.list(over[[nm]]) && is.list(base[[nm]]) &&
          !is.null(names(over[[nm]]))) {
      base[[nm]] <- .merge_config(base[[nm]], over[[nm]])
    } else {
      base[[nm]] <- over[[nm]]
    }
  }
  base
}

.derive_config <- function(cfg) {
  thr_name <- cfg$calibration$default_threshold
  thr <- cfg$calibration$thresholds[[thr_name]]
  if (is.null(thr)) {
    stop(sprintf(
      "read_config(): default_threshold '%s' is not in calibration$thresholds (%s)",
      thr_name, paste(names(cfg$calibration$thresholds), collapse = ", ")
    ), call. = FALSE)
  }
  cfg$calibration$c_thr <- thr

  if (!is.null(cfg$design$followup_months)) {
    cfg$design$followup_days <- round(months_to_days(
      unlist(cfg$design$followup_months)
    ))
  }
  cfg
}

#' Build the design grid described by a configuration.
#'
#' A thin wrapper over [design_grid()] so that scripts never re-read the same
#' fields by hand.
config_design_grid <- function(cfg) {
  d <- cfg$design
  design_grid(
    truth_model = unlist(d$truth_models),
    followup_days = d$followup_days,
    visits_per_year = unlist(d$visits_per_year),
    n_participants = unlist(d$n_participants),
    sigma_log = unlist(d$sigma_log),
    c_thr = cfg$calibration$c_thr,
    heterogeneity = unlist(d$heterogeneity),
    lloq = if (is.null(cfg$calibration$observation$lloq)) {
      NA_real_
    } else {
      cfg$calibration$observation$lloq
    },
    uloq = if (is.null(cfg$calibration$observation$uloq)) {
      NA_real_
    } else {
      cfg$calibration$observation$uloq
    },
    n_replicates = d$n_replicates,
    base_seed = cfg$run$seed
  )
}
