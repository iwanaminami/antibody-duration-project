# ---------------------------------------------------------------------------
# Sweep driver: run design cells, cache results, resume cleanly.
#
# OWNER: workstream (a), coordinator. Contract in INTERFACES.md.
#
# A sweep is long and will be interrupted. Three properties matter more than
# speed: a cache key derived from the design alone, so a resumed run reuses
# what it already has; one completed result appended at a time, so an
# interruption costs one cell and not the run; and atomic writes, so a
# half-written file is never mistaken for a finished one.
# ---------------------------------------------------------------------------

#' Cache path for one fit.
#'
#' Keyed on the design, the replicate, the method and the model -- never on
#' anything that varies between runs of the same specification.
cache_path <- function(cell, method, model, cache_dir = "cache") {
  file.path(cache_dir,
            sprintf("%s_rep%03d_%s_%s.rds", cell$cell_id, cell$replicate,
                    method, model))
}

#' Write an object to disk atomically.
#'
#' Write to a temporary file in the same directory, then rename. Rename is
#' atomic on every platform the project targets, so a reader either sees the
#' previous file or the complete new one.
write_atomic <- function(object, path) {
  .not_implemented("write_atomic", "(a) coordinator")
}

#' Run one design cell end to end.
#'
#' simulate -> reduce -> fit -> summarise -> score, returning the score rows
#' for that cell. Reuses a cached fit when one exists and `refit` is `FALSE`.
run_cell <- function(cell, cfg, methods = "ls", models = NULL, refit = FALSE) {
  .not_implemented("run_cell", "(a) coordinator")
}

#' Run a list of design cells, appending scores as they complete.
#'
#' @param cells from [design_grid()] or [config_design_grid()].
#' @return [score_rows()] for every cell that completed.
run_sweep <- function(cells, cfg, methods = "ls", models = NULL,
                      refit = FALSE) {
  .not_implemented("run_sweep", "(a) coordinator")
}
