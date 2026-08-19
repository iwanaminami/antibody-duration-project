# ---------------------------------------------------------------------------
# Mock artefacts.
#
# FROZEN: owned by the coordinator (workstream a). See INTERFACES.md.
#
# These exist so that no workstream waits for another. Scoring and plotting
# can be written against `mock_fit_result()` before any real fit exists;
# fitting can be written against `mock_sim_dataset()` before `simulate.R`
# lands. Every mock passes its own validator -- that is what makes building
# against them safe, and tests/testthat/test-mocks.R keeps it true.
#
# The values are plausible but arbitrary. Nothing scientific should be
# concluded from them, and no mock should reach a results table.
# ---------------------------------------------------------------------------

#' A small design cell, useful as a default argument.
mock_design_cell <- function(n_participants = 5L,
                             followup_days = 180,
                             replicate = 1L,
                             heterogeneity = FALSE,
                             ...) {
  design_cell(
    truth_model = "biphasic",
    followup_days = followup_days,
    visits_per_year = 12,
    n_participants = n_participants,
    sigma_log = 0.20,
    c_thr = 0.202,
    heterogeneity = heterogeneity,
    replicate = replicate,
    ...
  )
}

#' A schema-valid simulated dataset with the handout's biphasic truth.
#'
#' Deterministic: the same call always yields the same data, and the caller's
#' RNG state is left untouched.
#'
#' @param censor_frac fraction of observations marked left-censored, so that
#'   downstream code is exercised against censoring from the start.
mock_sim_dataset <- function(cell = mock_design_cell(), censor_frac = 0) {
  n <- cell$n_participants
  times <- seq(0, cell$followup_days,
               by = round(365.25 / cell$visits_per_year))
  truth_par <- list(c0 = 0.92, h1 = 35, h2 = 581, ts = 75)

  .with_seed(cell$seed, {
    if (cell$heterogeneity) {
      eta0 <- rnorm(n, 0, 0.30)
      etah <- rnorm(n, 0, 0.25)
    } else {
      eta0 <- etah <- rep(0, n)
    }
    c0_i <- truth_par$c0 * exp(eta0)
    h2_i <- truth_par$h2 * exp(etah)

    obs <- do.call(rbind, lapply(seq_len(n), function(i) {
      mu <- curve_biphasic(times, c0_i[i], truth_par$h1, h2_i[i], truth_par$ts)
      data.frame(
        participant_id = as.integer(i),
        time_days = as.numeric(times),
        y_obs = as.numeric(mu * exp(rnorm(length(times), 0, cell$sigma_log))),
        censor = "none",
        stringsAsFactors = FALSE
      )
    }))

    if (censor_frac > 0) {
      k <- max(1L, floor(censor_frac * nrow(obs)))
      idx <- sample.int(nrow(obs), k)
      obs$censor[idx] <- "left"
      obs$y_obs[idx] <- NA_real_
    }

    truth <- tibble::tibble(
      participant_id = seq_len(n),
      c0 = c0_i,
      h1 = truth_par$h1,
      h2 = h2_i,
      ts = truth_par$ts,
      tstar_days = tstar("biphasic",
                         list(c0 = c0_i, h1 = truth_par$h1, h2 = h2_i,
                              ts = truth_par$ts),
                         cell$c_thr)
    )
    # Population target from the mean trajectory. The real definition is a
    # decision for the workshop (handout Section 2) -- workstream (e) owns it.
    pop <- tstar("biphasic", truth_par, cell$c_thr)

    sim_dataset(cell, obs, truth, population_tstar_days = pop,
                extra_meta = list(mock = TRUE))
  })
}

#' A schema-valid fit result with draws for both estimands.
#'
#' @param n_draws number of draws per estimand.
#' @param no_cross_frac fraction of draws that never reach the threshold, so
#'   that scoring and plotting meet `Inf` durations immediately rather than
#'   on the day a real plateau fit produces them.
#' @param participant_ids individuals to emit draws for; `NULL` for none.
mock_fit_result <- function(cell = mock_design_cell(),
                            method = "mock",
                            model = "biphasic",
                            n_draws = 200L,
                            no_cross_frac = 0.05,
                            participant_ids = 1:2) {
  .with_seed(cell$seed + 1L, {
    par_names <- curve_params(model)
    truth_vals <- list(c0 = 0.92, h1 = 35, h2 = 581, ts = 75,
                       k = log(2) / 300, alpha = 0.01, beta = 1.2,
                       A_inf = 0.05)
    params <- tibble::tibble(
      draw_id = rep(seq_len(n_draws), each = length(par_names)),
      param = rep(par_names, times = n_draws),
      value = as.numeric(unlist(truth_vals[par_names])) *
        exp(rnorm(n_draws * length(par_names), 0, 0.05))
    )

    draw_tstar <- function(centre) {
      v <- centre * exp(rnorm(n_draws, 0, 0.25))
      v[seq_len(round(no_cross_frac * n_draws))] <- Inf
      v
    }
    pop <- tibble::tibble(
      draw_id = seq_len(n_draws),
      estimand = "population",
      participant_id = NA_integer_,
      tstar_days = draw_tstar(900)
    )
    ind <- if (length(participant_ids) > 0) {
      do.call(rbind, lapply(participant_ids, function(i) {
        tibble::tibble(
          draw_id = seq_len(n_draws),
          estimand = "individual",
          participant_id = as.integer(i),
          tstar_days = draw_tstar(900 * (0.8 + 0.2 * i))
        )
      }))
    } else {
      NULL
    }
    draws <- rbind(pop, ind)
    draws$crossed <- is.finite(draws$tstar_days)

    fit_result(
      cell_id = cell$cell_id,
      replicate = cell$replicate,
      method = method,
      model = model,
      params = params,
      tstar_draws = draws,
      diagnostics = default_diagnostics(
        converged = TRUE, n_draws = n_draws, runtime_s = 0,
        message = "mock fit; not a real inference"
      ),
      ic = default_ic(loglik = -120, n_par = length(par_names),
                      aic = 2 * length(par_names) + 240),
      provenance = default_provenance(engine = "mock", seed = cell$seed)
    )
  })
}

#' A schema-valid score table built from mock fits.
#'
#' Scores one mock fit per candidate model so that aggregation and plotting
#' code meets more than one row from the outset.
mock_score_rows <- function(cell = mock_design_cell(),
                            models = c("biphasic", "exponential")) {
  data <- mock_sim_dataset(cell)
  rows <- lapply(models, function(m) {
    fit <- mock_fit_result(cell, model = m)
    s <- summarise_tstar(fit, estimand = "population",
                         level = 0.90)
    true <- data$meta$population_tstar_days
    width <- s$tstar_hi - s$tstar_lo
    tibble::tibble(
      cell_id = cell$cell_id,
      replicate = cell$replicate,
      method = fit$method,
      model = m,
      estimand = "population",
      truth_model = cell$truth_model,
      followup_days = cell$followup_days,
      visits_per_year = cell$visits_per_year,
      n_participants = cell$n_participants,
      sigma_log = cell$sigma_log,
      heterogeneity = cell$heterogeneity,
      c_thr = cell$c_thr,
      tstar_true_days = true,
      tstar_med = s$tstar_med,
      tstar_lo = s$tstar_lo,
      tstar_hi = s$tstar_hi,
      interval_level = s$interval_level,
      bias = s$tstar_med - true,
      abs_err = abs(s$tstar_med - true),
      sq_err = (s$tstar_med - true)^2,
      covered = true >= s$tstar_lo & true <= s$tstar_hi,
      width = width,
      rel_width = width / true,
      prob_no_cross = s$prob_no_cross,
      adequate = NA
    )
  })
  score_rows(do.call(rbind, rows))
}
