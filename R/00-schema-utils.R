# ---------------------------------------------------------------------------
# Shared constants and schema helpers.
#
# Sourced first: package files load in filename order, and the schema specs
# in R/schema-*.R reference the constants defined here at load time. That is
# what the "00-" prefix is for.
#
# FROZEN: owned by the coordinator (workstream a). Propose changes through a
# short group review rather than editing directly -- every workstream builds
# against these guarantees. See INTERFACES.md.
# ---------------------------------------------------------------------------

#' Version stamped onto every artefact this package produces.
#'
#' Bump the minor component when a column is added, and the major component
#' when an existing column changes meaning or type.
SCHEMA_VERSION <- "1.0.0"

#' Nominal days per month.
#'
#' Time is always measured in days inside this package. Months exist only as
#' labels for design cells and figure axes; use [months_to_days()] and
#' [days_to_months()] at the boundary rather than hard-coding a conversion.
MONTH_DAYS <- 365.25 / 12

#' Trajectory families that may be fitted to observed data.
CANDIDATE_MODELS <- c("exponential", "biphasic", "powerlaw", "plateau")

#' Trajectory families that may generate simulated truth.
TRUTH_MODELS <- c("biphasic", "mechanistic", "semimechanistic")

#' Inference approaches, one per analysis workstream (plus "mock").
FIT_METHODS <- c("ls", "mcmc", "nlme", "mock")

#' The two estimands the project distinguishes.
#'
#' "individual" is the crossing time for one participant's own trajectory;
#' "population" is the crossing time for the population-level target defined
#' in the handout. Anything that summarises durations must say which it means.
ESTIMANDS <- c("individual", "population")

#' Convert a duration in nominal months to days.
months_to_days <- function(months) months * MONTH_DAYS

#' Convert a duration in days to nominal months.
days_to_months <- function(days) days / MONTH_DAYS

# --- validation machinery --------------------------------------------------

.stop_schema <- function(what, ...) {
  stop(sprintf("%s: %s", what, paste0(..., collapse = "")), call. = FALSE)
}

.is_whole <- function(x) {
  x <- x[!is.na(x)]
  length(x) == 0 || (is.numeric(x) && all(abs(x - round(x)) < 1e-8))
}

#' Check a data frame against a column specification.
#'
#' `spec` is a named list; each element describes one required column:
#'
#' * `type`     one of "integer", "double", "character", "logical"
#' * `na_ok`    are `NA` values permitted (default `FALSE`)
#' * `inf_ok`   are infinite values permitted (default `FALSE`)
#' * `min`      lower bound, checked on non-missing values
#' * `max`      upper bound, checked on non-missing values
#' * `values`   the complete set of permitted values
#'
#' `NaN` is never permitted: it means a calculation went wrong, whereas `NA`
#' means "not applicable" and `Inf` means "never crosses the threshold".
#' Columns beyond those in `spec` are allowed, so a workstream can carry extra
#' diagnostics without breaking anyone downstream.
#'
#' Returns `df` invisibly, or stops with a message naming the offending column.
check_cols <- function(df, spec, what = "table") {
  if (!is.data.frame(df)) {
    .stop_schema(what, "expected a data frame, got ", class(df)[1])
  }
  missing_cols <- setdiff(names(spec), names(df))
  if (length(missing_cols) > 0) {
    .stop_schema(what, "missing required column(s): ",
                 paste(missing_cols, collapse = ", "))
  }
  for (nm in names(spec)) {
    s <- spec[[nm]]
    x <- df[[nm]]
    na_ok <- isTRUE(s$na_ok)
    inf_ok <- isTRUE(s$inf_ok)

    type_ok <- switch(
      s$type,
      integer = is.numeric(x) && .is_whole(x),
      double = is.numeric(x),
      character = is.character(x),
      logical = is.logical(x),
      .stop_schema(what, "unknown type '", s$type, "' in specification")
    )
    if (!type_ok) {
      .stop_schema(what, "column '", nm, "' must be ", s$type,
                   ", got ", class(x)[1])
    }
    if (is.numeric(x) && any(is.nan(x))) {
      .stop_schema(what, "column '", nm, "' contains NaN; use NA for ",
                   "'not applicable' and Inf for 'never crosses'")
    }
    if (!na_ok && any(is.na(x) & !is.nan(x))) {
      .stop_schema(what, "column '", nm, "' must not contain NA")
    }
    if (is.numeric(x) && !inf_ok && any(is.infinite(x))) {
      .stop_schema(what, "column '", nm, "' must be finite")
    }
    ok <- !is.na(x)
    if (!is.null(s$min) && any(x[ok] < s$min)) {
      .stop_schema(what, "column '", nm, "' must be >= ", s$min)
    }
    if (!is.null(s$max) && any(x[ok] > s$max)) {
      .stop_schema(what, "column '", nm, "' must be <= ", s$max)
    }
    if (!is.null(s$values)) {
      bad <- setdiff(unique(x[ok]), s$values)
      if (length(bad) > 0) {
        .stop_schema(what, "column '", nm, "' has unexpected value(s): ",
                     paste(utils::head(bad, 5), collapse = ", "),
                     "; permitted: ", paste(s$values, collapse = ", "))
      }
    }
  }
  invisible(df)
}

#' Enforce the units convention on every duration column.
#'
#' Any column whose name ends in `_days` must be numeric and non-negative.
#' `Inf` is permitted throughout: it is how the project records a trajectory
#' that never falls below the threshold.
check_days_cols <- function(df, what = "table") {
  for (nm in grep("_days$", names(df), value = TRUE)) {
    x <- df[[nm]]
    if (!is.numeric(x)) {
      .stop_schema(what, "duration column '", nm, "' must be numeric")
    }
    if (any(is.nan(x))) {
      .stop_schema(what, "duration column '", nm, "' contains NaN")
    }
    if (any(x[!is.na(x)] < 0)) {
      .stop_schema(what, "duration column '", nm, "' must be >= 0")
    }
  }
  invisible(df)
}

.check_names <- function(x, required, what) {
  if (!is.list(x)) .stop_schema(what, "expected a list, got ", class(x)[1])
  missing_names <- setdiff(required, names(x))
  if (length(missing_names) > 0) {
    .stop_schema(what, "missing required element(s): ",
                 paste(missing_names, collapse = ", "))
  }
  invisible(x)
}

.check_scalar_duration <- function(x, what, nm) {
  if (!is.numeric(x) || length(x) != 1L || is.nan(x)) {
    .stop_schema(what, "'", nm, "' must be a single non-NaN number")
  }
  if (!is.na(x) && x < 0) .stop_schema(what, "'", nm, "' must be >= 0")
  invisible(x)
}

# --- determinism -----------------------------------------------------------

#' A deterministic 32-bit FNV-1a hash of a string.
#'
#' Base R only, so it gives the same answer on every machine and R version --
#' which is the point, because cell identifiers and seeds are derived from it.
#'
#' Arithmetic is done on 16-bit halves throughout. R's `bitwXor()` refuses
#' anything above `2^31 - 1`, and a direct 32-bit multiply would exceed the
#' 53 bits a double can hold exactly, so either shortcut would silently
#' produce NA or a rounded answer.
.hash_string <- function(s) {
  bytes <- as.integer(charToRaw(paste0(s, collapse = "\x1f")))
  h <- 2166136261            # FNV-1a 32-bit offset basis
  for (b in bytes) {
    h <- .xor32(h, b)
    h <- .mul32(h, 16777619) # FNV-1a 32-bit prime
  }
  h
}

.xor32 <- function(a, b) {
  bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
}

.mul32 <- function(a, p) {
  (((a %/% 65536 * p) %% 65536) * 65536 + a %% 65536 * p) %% 4294967296
}

#' Eight hex characters identifying a set of design fields.
.hash8 <- function(...) {
  h <- .hash_string(paste0(..., collapse = "|"))
  sprintf("%04x%04x", as.integer(h %/% 65536), as.integer(h %% 65536))
}

#' Derive a replicate seed from the design cell, the replicate and a base seed.
#'
#' Seeds must be reproducible from the design alone, so that a cached result
#' and a fresh run agree and a sweep can resume without renumbering anything.
derive_seed <- function(cell_id, replicate, base_seed = 20260901L) {
  h <- .hash_string(paste(cell_id, replicate, base_seed, sep = "|"))
  as.integer(h %% 2147483647)
}

#' Evaluate an expression under a fixed seed, restoring the caller's RNG state.
.with_seed <- function(seed, expr) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_seed) {
    old <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }
  set.seed(seed)
  expr
}

# --- stubs -----------------------------------------------------------------

#' Signal that a workstream has not implemented this function yet.
#'
#' Stubs deliberately fail loudly rather than returning something plausible:
#' a silent placeholder would propagate into results.
.not_implemented <- function(fn, owner) {
  stop(sprintf(
    paste0("`%s()` is not implemented yet (workstream %s).\n",
           "  Its contract is fixed -- see INTERFACES.md.\n",
           "  Build against mock_sim_dataset() / mock_fit_result() until it lands."),
    fn, owner
  ), call. = FALSE)
}
