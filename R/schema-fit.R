# ---------------------------------------------------------------------------
# Schema: fit result.
#
# FROZEN: owned by the coordinator (workstream a). See INTERFACES.md.
#
# THE CONTRACT THAT MAKES PARALLEL WORK POSSIBLE.
#
# `$tstar_draws` is the common currency of the project. Least squares supplies
# bootstrap or delta-method draws; MCMC supplies posterior draws; NLME supplies
# draws for the population and for individuals. Scoring and plotting never
# branch on `$method`, so workstreams (b), (c) and (d) can be built in
# parallel and compared on one axis.
#
# Two rules everything depends on:
#   * a trajectory that never reaches the threshold has tstar_days = Inf and
#     crossed = FALSE -- never a large finite stand-in;
#   * `estimand` says whether a row is about one participant or the
#     population, because the two answers differ under heterogeneity.
# ---------------------------------------------------------------------------

PARAMS_SPEC <- list(
  draw_id = list(type = "integer", min = 1),
  param   = list(type = "character"),
  value   = list(type = "double")
)

TSTAR_DRAWS_SPEC <- list(
  draw_id        = list(type = "integer", min = 1),
  estimand       = list(type = "character", values = ESTIMANDS),
  participant_id = list(type = "integer", min = 1, na_ok = TRUE),
  tstar_days     = list(type = "double", min = 0, inf_ok = TRUE),
  crossed        = list(type = "logical")
)

DIAGNOSTICS_FIELDS <- c("converged", "rhat_max", "ess_bulk_min", "divergences",
                        "n_draws", "runtime_s", "message")
IC_FIELDS <- c("loglik", "n_par", "aic", "bic", "loo", "waic")
PROVENANCE_FIELDS <- c("schema_version", "engine", "engine_version", "seed",
                       "timestamp", "git_sha")

#' Default diagnostics, all missing.
#'
#' Point estimates have no `rhat_max`; that is what `NA` is for. `converged`
#' is the one field every method must set honestly, because scoring filters
#' on it.
default_diagnostics <- function(...) {
  utils::modifyList(
    list(converged = NA, rhat_max = NA_real_, ess_bulk_min = NA_real_,
         divergences = NA_integer_, n_draws = NA_integer_,
         runtime_s = NA_real_, message = NA_character_),
    list(...)
  )
}

#' Default information criteria, all missing.
default_ic <- function(...) {
  utils::modifyList(
    list(loglik = NA_real_, n_par = NA_real_, aic = NA_real_, bic = NA_real_,
         loo = NA_real_, waic = NA_real_),
    list(...)
  )
}

#' Default provenance for a fit produced now, by this package.
default_provenance <- function(engine = NA_character_,
                               engine_version = NA_character_,
                               seed = NA_integer_) {
  list(
    schema_version = SCHEMA_VERSION,
    engine = engine,
    engine_version = engine_version,
    seed = seed,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    git_sha = .git_sha()
  )
}

.git_sha <- function() {
  sha <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) NA_character_
  )
  if (length(sha) != 1L || !nzchar(sha)) NA_character_ else sha
}

#' Construct a fit result.
#'
#' @param method one of [FIT_METHODS]; `"mock"` is for development only.
#' @param model one of [CANDIDATE_MODELS].
#' @param params tibble of `draw_id`, `param`, `value` (long format, so that
#'   families with different parameter counts share one schema).
#' @param tstar_draws tibble of `draw_id`, `estimand`, `participant_id`,
#'   `tstar_days`, `crossed`. Population rows have `participant_id = NA`.
#' @param diagnostics,ic,provenance see [default_diagnostics()],
#'   [default_ic()], [default_provenance()].
#' @return an object of class `fit_result`.
fit_result <- function(cell_id, replicate, method, model, params, tstar_draws,
                       diagnostics = default_diagnostics(),
                       ic = default_ic(),
                       provenance = default_provenance()) {
  x <- list(
    cell_id = as.character(cell_id),
    replicate = as.integer(replicate),
    method = match.arg(method, FIT_METHODS),
    model = match.arg(model, CANDIDATE_MODELS),
    params = tibble::as_tibble(params),
    tstar_draws = tibble::as_tibble(tstar_draws),
    diagnostics = utils::modifyList(default_diagnostics(), diagnostics),
    ic = utils::modifyList(default_ic(), ic),
    provenance = utils::modifyList(default_provenance(), provenance)
  )
  structure(x, class = "fit_result")
}

#' Validate a fit result, or stop with a message naming the problem.
validate_fit_result <- function(x) {
  what <- "fit_result"
  if (!inherits(x, "fit_result")) {
    .stop_schema(what, "object does not have class 'fit_result'")
  }
  .check_names(x,
               c("cell_id", "replicate", "method", "model", "params",
                 "tstar_draws", "diagnostics", "ic", "provenance"),
               what)
  if (!is.character(x$cell_id) || length(x$cell_id) != 1L || !nzchar(x$cell_id)) {
    .stop_schema(what, "'cell_id' must be a single non-empty string")
  }
  if (!is.integer(x$replicate) || length(x$replicate) != 1L ||
        is.na(x$replicate) || x$replicate < 1L) {
    .stop_schema(what, "'replicate' must be a single integer >= 1")
  }
  if (!x$method %in% FIT_METHODS) {
    .stop_schema(what, "'method' must be one of ",
                 paste(FIT_METHODS, collapse = ", "))
  }
  if (!x$model %in% CANDIDATE_MODELS) {
    .stop_schema(what, "'model' must be one of ",
                 paste(CANDIDATE_MODELS, collapse = ", "))
  }

  check_cols(x$params, PARAMS_SPEC, "fit_result$params")
  expected <- curve_params(x$model)
  unexpected <- setdiff(unique(x$params$param), expected)
  if (length(unexpected) > 0) {
    .stop_schema("fit_result$params", "model '", x$model,
                 "' has parameters ", paste(expected, collapse = ", "),
                 "; found unexpected: ", paste(unexpected, collapse = ", "))
  }

  d <- x$tstar_draws
  check_cols(d, TSTAR_DRAWS_SPEC, "fit_result$tstar_draws")
  check_days_cols(d, "fit_result$tstar_draws")
  if (nrow(d) == 0L) {
    .stop_schema("fit_result$tstar_draws", "no draws; a fit that failed ",
                 "should still report its failure through $diagnostics")
  }
  if (!identical(d$crossed, is.finite(d$tstar_days))) {
    .stop_schema("fit_result$tstar_draws",
                 "'crossed' must equal is.finite(tstar_days): a non-crossing ",
                 "draw is Inf, never a large finite number")
  }
  is_pop <- d$estimand == "population"
  if (any(is_pop & !is.na(d$participant_id))) {
    .stop_schema("fit_result$tstar_draws",
                 "population rows must have participant_id = NA")
  }
  if (any(!is_pop & is.na(d$participant_id))) {
    .stop_schema("fit_result$tstar_draws",
                 "individual rows must have a participant_id")
  }

  .check_names(x$diagnostics, DIAGNOSTICS_FIELDS, "fit_result$diagnostics")
  .check_names(x$ic, IC_FIELDS, "fit_result$ic")
  .check_names(x$provenance, PROVENANCE_FIELDS, "fit_result$provenance")
  if (!is.logical(x$diagnostics$converged) ||
        length(x$diagnostics$converged) != 1L) {
    .stop_schema("fit_result$diagnostics",
                 "'converged' must be TRUE, FALSE or NA")
  }
  invisible(x)
}

#' Summarise `$tstar_draws` into a median and an equal-tailed interval.
#'
#' The single place the project turns draws into a point estimate and an
#' interval, so that every method is summarised identically. Draws at `Inf`
#' participate in the quantiles: an interval whose upper end is `Inf` is a
#' true statement about the fit, and `prob_no_cross` records how often it
#' happened.
#'
#' @param estimand one of [ESTIMANDS].
#' @param level nominal interval level; the project default is 0.90.
#' @return a one-row tibble of `tstar_med`, `tstar_lo`, `tstar_hi`,
#'   `interval_level`, `prob_no_cross`, `n_draws`.
summarise_tstar <- function(x, estimand = "individual", level = 0.90) {
  estimand <- match.arg(estimand, ESTIMANDS)
  d <- x$tstar_draws[x$tstar_draws$estimand == estimand, , drop = FALSE]
  if (nrow(d) == 0L) {
    stop(sprintf("summarise_tstar(): no '%s' draws in this fit_result",
                 estimand), call. = FALSE)
  }
  a <- (1 - level) / 2
  q <- quantile(d$tstar_days, probs = c(a, 0.5, 1 - a), names = FALSE,
                type = 7, na.rm = FALSE)
  tibble::tibble(
    estimand = estimand,
    tstar_med = q[2],
    tstar_lo = q[1],
    tstar_hi = q[3],
    interval_level = level,
    prob_no_cross = mean(!d$crossed),
    n_draws = nrow(d)
  )
}

print.fit_result <- function(x, ...) {
  cat(sprintf(
    "<fit_result> %s  rep %d\n  method = %s | model = %s | converged = %s\n  %d draws (%s) | AIC = %s\n",
    x$cell_id, x$replicate, x$method, x$model, x$diagnostics$converged,
    nrow(x$tstar_draws),
    paste(sort(unique(x$tstar_draws$estimand)), collapse = " + "),
    format(x$ic[["aic"]])
  ))
  invisible(x)
}
