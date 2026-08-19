#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Integration gate.
#
# OWNER: workstream (a), coordinator.
#
# The rule this script enforces: no sweep starts until one small cell runs the
# whole way through and produces schema-valid output. It is cheap to run and
# it is the thing that catches an interface drifting apart.
#
#   Rscript scripts/00-integration-gate.R
#
# Right now it checks the frozen core -- the layer every workstream is
# entitled to rely on. The pipeline steps below are commented out and become
# live as each workstream lands its function; uncomment a step only once it
# passes, so the gate never reports green on something that is not there.
# ---------------------------------------------------------------------------

suppressMessages(pkgload::load_all(quiet = TRUE))

ok <- function(fmt, ...) cat(sprintf(paste0("  ok    ", fmt, "\n"), ...))
step <- function(x) cat(sprintf("\n%s\n", x))

cat("Integration gate | antibodyduration", as.character(packageVersion("antibodyduration")), "\n")
cat("schema version   |", SCHEMA_VERSION, "\n")

step("Configuration")
cfg <- read_config()
ok("loaded %s", basename(cfg$.path))
ok("threshold c_thr = %g (%s)", cfg$calibration$c_thr,
   cfg$calibration$default_threshold)

step("Candidate families")
b <- cfg$calibration$biphasic
stopifnot(isTRUE(all.equal(curve_biphasic(0, b$c0, b$h1_days, b$h2_days, b$ts_days), b$c0)))
ok("biphasic c(0) = c0 = %g", b$c0)
tt <- tstar("biphasic",
            list(c0 = b$c0, h1 = b$h1_days, h2 = b$h2_days, ts = b$ts_days),
            cfg$calibration$c_thr)
ok("T* under the handout calibration = %.0f days (%.1f months)",
   tt, days_to_months(tt))
ok("a plateau above the threshold gives T* = %s",
   format(tstar("plateau", list(c0 = b$c0, A_inf = 0.5, k = 0.01),
                cfg$calibration$c_thr)))

step("Design grid")
cells <- config_design_grid(read_config(
  overrides = list(design = list(n_replicates = 2))
))
invisible(lapply(cells, validate_design_cell))
ok("%d cells, %d distinct designs, all valid", length(cells),
   length(unique(vapply(cells, `[[`, character(1), "cell_id"))))

step("Contracts (mock artefacts)")
gate_cell <- mock_design_cell(n_participants = 20L, followup_days = 180)
data <- validate_sim_dataset(mock_sim_dataset(gate_cell))
ok("sim_dataset  %d observations from %d participants",
   nrow(data$obs), length(unique(data$obs$participant_id)))
fit <- validate_fit_result(mock_fit_result(gate_cell))
ok("fit_result   %d draws, %d parameters",
   nrow(fit$tstar_draws), length(unique(fit$params$param)))
s <- summarise_tstar(fit, "population", cfg$scoring$interval_level)
ok("summary      T* = %.0f [%.0f, %s] days, P(no cross) = %.2f",
   s$tstar_med, s$tstar_lo, format(round(s$tstar_hi)), s$prob_no_cross)
scores <- validate_score_rows(mock_score_rows(gate_cell))
ok("score_rows   %d rows, %d columns", nrow(scores), ncol(scores))

# --- the real pipeline, one line per workstream ----------------------------
# Uncomment each as it lands. The gate is green only for what actually runs.
#
# step("Pipeline")
# full  <- simulate_full(gate_cell, cfg)                      # (a)
# data  <- reduce_to_review(full, gate_cell)                  # (a)
# fit   <- fit_ls_bootstrap(data, "biphasic", cfg)            # (b)
# score <- score_fit(fit, data, cfg)                          # (a)
# p     <- plot_example_fit(data, list(fit), cfg)             # (a)

cat("\nINTEGRATION GATE PASSED (frozen core only; pipeline steps still stubbed)\n")
