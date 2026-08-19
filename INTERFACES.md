# Interface contract

This is what each workstream may rely on, and what it owes everyone else. It
is deliberately short: freeze the interfaces, not the implementations.

The methodological source of truth is [notes/handout.tex](notes/handout.tex).
Where this document gives a formula or a starting value, it is transcribing
the handout. If the workshop changes the methodology, change the handout
first and then follow it here.

## Ownership

Each workstream owns its own files, so parallel work does not collide.

| Workstream | Files | Owns |
| --- | --- | --- |
| **shared core** | `R/schema-*.R`, `R/models.R`, `R/config.R`, `R/mocks.R`, `inst/config/default.yaml` | The contract below. Changes go through a short group review. |
| **(a)** coordinator + visualisation | `R/simulate.R`, `R/driver.R`, `R/score.R`, `R/plots.R`, `scripts/` | Simulation, the sweep, metrics, figures, integration |
| **(b)** simple analysis | `R/fit-ls.R` | Biphasic least squares, bootstrap intervals |
| **(c)** alternate models + MCMC | `R/fit-mcmc.R` | All candidate families, model selection, Bayesian fits |
| **(d)** NLME | `R/fit-nlme.R` | Mixed-effects fits, individual against population |
| **(e)** calibration | `R/calibrate.R` | Threshold, assay limits, protection link, sensitivities |

Rules:

1. Do not edit another workstream's file. Propose the change to its owner.
2. Do not edit the shared core alone. Propose it to the group; a schema change
   that lands without warning breaks work that is already in flight.
3. Every artefact you emit must pass its validator. This is not a formality:
   it is the only thing standing between a parallel split and an integration
   day spent reconciling column names.
4. Build against `mock_*()` until the function you depend on exists. Waiting
   is never necessary.

## Units and conventions

- **Time is in days.** Months appear only in labels and in the design grid;
  convert with `months_to_days()` and `days_to_months()`. Any column holding a
  duration is named `*_days` and the validators enforce it.
- **Antibody levels are relative to the mean convalescent level**, which is 1
  by construction. The threshold is on the same scale.
- **A trajectory that never reaches the threshold has `tstar_days = Inf` and
  `crossed = FALSE`.** Never substitute a large finite number. This is a real
  outcome, reported downstream as `prob_no_cross`; hiding it inside a big
  number biases every interval that contains it.
- **`NA` means "not applicable", `Inf` means "never crosses", `NaN` means a
  bug.** The validators reject `NaN` everywhere.
- **Seeds come from the design**, via `derive_seed(cell_id, replicate)`, so a
  cached result and a fresh run agree and a resumed sweep renumbers nothing.

## The four schemas

Defined in `R/schema-*.R`, each with a constructor and a `validate_*()` that
stops with a message naming the problem. Extra columns are always permitted;
the ones listed are required.

### `design_cell` — one row of the sweep

`cell_id`, `truth_model`, `followup_days`, `visits_per_year`,
`n_participants`, `sigma_log`, `heterogeneity`, `c_thr`, `lloq`, `uloq`,
`replicate`, `seed`, `schema_version`.

`cell_id` identifies the *design* and excludes `replicate`, so replicates
group together. Build grids with `design_grid()` or `config_design_grid(cfg)`.

### `sim_dataset` — what a trial review would have seen

- `$cell` — the `design_cell`
- `$obs` — `participant_id`, `time_days`, `y_obs`, `censor` ∈ {`none`, `left`,
  `right`}. **A censored row carries `NA` in `y_obs`**; the limit is in
  `cell$lloq` / `cell$uloq`. Putting the limit in `y_obs` would let a fit
  treat it as a measurement, which is the mistake censoring exists to avoid.
- `$truth` — `participant_id`, the generating parameters, `tstar_days`
- `$meta` — `population_tstar_days`, `truth_model`, `seed`, `schema_version`

Analysis code reads `$obs` and nothing else.

### `fit_result` — the common currency

`$cell_id`, `$replicate`, `$method` ∈ {`ls`, `mcmc`, `nlme`, `mock`},
`$model` ∈ {`exponential`, `biphasic`, `powerlaw`, `plateau`}, plus:

- `$params` — `draw_id`, `param`, `value` (long, so families with different
  parameter counts share one schema)
- `$tstar_draws` — `draw_id`, `estimand` ∈ {`individual`, `population`},
  `participant_id` (`NA` for population), `tstar_days`, `crossed`
- `$diagnostics` — `converged`, `rhat_max`, `ess_bulk_min`, `divergences`,
  `n_draws`, `runtime_s`, `message`
- `$ic` — `loglik`, `n_par`, `aic`, `bic`, `loo`, `waic`
- `$provenance` — `schema_version`, `engine`, `engine_version`, `seed`,
  `timestamp`, `git_sha`

**`$tstar_draws` is why the split works.** Least squares supplies bootstrap
draws, MCMC supplies posterior draws, NLME supplies draws for both estimands.
Scoring and plotting never branch on `$method`, so (b), (c) and (d) are built
in parallel and compared on one axis. Summarise with `summarise_tstar()` — the
single place draws become a point estimate and an interval, so that every
method is summarised identically.

Fields that do not apply are `NA`, not omitted. A point estimate has no
`rhat_max`; a fit that failed still returns draws and records the failure in
`$diagnostics`.

### `score_rows` — one row per (cell, replicate, method, model, estimand)

Design fields repeated on every row, then `tstar_true_days`, `tstar_med`,
`tstar_lo`, `tstar_hi`, `interval_level`, `bias`, `abs_err`, `sq_err`,
`covered`, `width`, `rel_width`, `prob_no_cross`, `adequate`. See
`SCORE_COLUMNS`, and `empty_score_rows()` for the shape.

## Signatures

Every stub in `R/` carries its full signature and its pre- and postconditions
in a header comment, and fails with a message naming its owner if called. Read
the file rather than duplicating the list here — a signature written down in
two places drifts.

## Adding a dependency

`renv::install()` it, declare it in `DESCRIPTION` (`Imports` to run,
`Suggests` for one workstream's engine), write the code, then
`renv::snapshot()` and commit `DESCRIPTION` and `renv.lock` together.

The lockfile records what the code actually references, so a package declared
in `Suggests` and not yet called stays unpinned — which is why nobody needs a
Stan toolchain to run the tests, and why the engine you choose enters the
lockfile the moment you commit code that calls it. Never hand-edit
`renv.lock`. See [AGENTS.md](AGENTS.md) for the full note.
