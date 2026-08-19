# Working in this repository

Guidance for coding agents and for anyone new to the project. Read
[INTERFACES.md](INTERFACES.md) before writing code that produces or consumes a
shared artefact.

## What this project is

A simulation study for the Nagoya-Oxford Workshop 2026. A vaccine trial
measures an antibody marker repeatedly after vaccination; the marker falls
quickly and then more slowly. At each planned trial review we want to estimate
when it will fall below a protection-relevant threshold, often beyond the
period observed so far, and to say how much would be gained from further
follow-up. The study simulates complete antibody histories, hides everything
after a review date, fits candidate trajectory models, and asks how reliably
the duration is recovered.

Five workstreams run in parallel over three days. See the ownership table in
[INTERFACES.md](INTERFACES.md).

## The handout is the source of truth

[notes/handout.tex](notes/handout.tex) states the question, the estimand, the
candidate models and the starting values. Code follows it, not the other way
round.

**A methodological change goes into the handout first, then into the code.**
If you find yourself writing a formula or a parameter value that is not in the
handout, that is a signal to stop and raise it, not to write it down twice.
`inst/config/default.yaml` transcribes the handout's tables; when the handout
changes, that file changes with it.

## Environment

R only. There is no Python in this project.

```r
renv::restore()        # first time, and whenever renv.lock changes
pkgload::load_all()    # load the package
testthat::test_local() # run the tests
```

```sh
Rscript scripts/00-integration-gate.R   # the cheap end-to-end check
```

### Adding a dependency

1. `renv::install("thepackage")`
2. Declare it in `DESCRIPTION`: `Imports` if the package needs it to run,
   `Suggests` if only one workstream's engine does.
3. Write the code that uses it.
4. `renv::snapshot()`, then commit `DESCRIPTION` and `renv.lock` together.

The lockfile is snapshotted implicitly, meaning it records what the project's
code actually references. A package declared in `Suggests` and never called
stays out of the lockfile, so nobody has to install a Stan toolchain to run
the tests — and it enters the lockfile automatically once the workstream that
wanted it commits code that calls it. Never hand-edit `renv.lock`.

The MCMC and NLME engines are the case this is designed for. `brms`,
`cmdstanr`, `nlmixr2`, `rjags` and `saemix` are declared in `Suggests` and
deliberately unpinned; workstreams (c) and (d) choose, install and snapshot
whichever they use. `cmdstanr` is not on CRAN, which is what
`Additional_repositories` in `DESCRIPTION` is for.

### Two things about the package layout

`NAMESPACE` is hand-written and exports by pattern, so there is no roxygen
step: add a function to a file in `R/` and it is exported. The consequence is
that `R CMD check` reports one warning about undocumented objects. That is
expected and is not worth fixing — the project is not going to CRAN, and
maintaining 80 `man/` pages during a three-day workshop would cost more than
it returns. Everything else in the check should stay clean.

Files in `R/` are sourced in filename order. `R/00-schema-utils.R` carries the
shared constants and is named to sort first, because the schema definitions
reference those constants while they are being sourced. Ordinary files need no
prefix.

## Conventions that matter

These are not style preferences. Each one exists because getting it wrong
produces a plausible-looking wrong answer rather than an error.

- **Time is in days.** Months are labels only. Duration columns are named
  `*_days` and the validators check it.
- **A trajectory that never reaches the threshold is `Inf`, with
  `crossed = FALSE`.** Never substitute a large finite number, and never drop
  those draws. They are reported as `prob_no_cross`. An interval that runs to
  `Inf` does contain the truth, so coverage alone will score it a success —
  which is exactly why the adequacy rule also requires a bounded width and a
  low non-crossing fraction.
- **`NA` is "not applicable", `Inf` is "never crosses", `NaN` is a bug.** The
  validators reject `NaN` everywhere.
- **Validate every artefact you emit.** `validate_design_cell()`,
  `validate_sim_dataset()`, `validate_fit_result()`, `validate_score_rows()`.
- **Seeds derive from the design**, through `derive_seed(cell_id, replicate)`.
  Do not call `set.seed()` with an ad-hoc number in analysis code.
- **State the estimand.** Individual and population durations differ under
  heterogeneity, and a figure or a number that does not say which it means
  cannot be interpreted.
- **Report failures.** A fit that did not converge, a cell that was skipped, a
  replicate that errored: record it. Which cells failed is itself a result
  about the design.
- **Do not implement a stub outside your workstream.** If you need something
  that is not there yet, build against `mock_sim_dataset()`,
  `mock_fit_result()` or `mock_score_rows()`.

## Style

- Match the surrounding code. Two-space indent, `<-` for assignment, snake
  case, lines under 80 characters.
- Comments explain why, not what. The frozen files carry longer headers on
  purpose, because they encode decisions other people depend on.
- British English in prose and comments. No emoji.
- Tests go in `tests/testthat/`, named after the file they cover, and assert
  the property rather than a value nobody can check.

## Git

Fork the [main repository](https://github.com/will-s-hart/antibody-duration-project),
branch in your fork, and open a pull request against `main`. Commit subjects
are imperative and sentence case, matching the existing log: "Refine antibody
duration handout", "Source the biphasic initial level".

Do not commit generated artefacts. `results/` and `cache/` are ignored, as are
LaTeX build products; `notes/handout.pdf` is the deliberate exception.

`_will/`, `_old/` and `slides/` are private working directories, ignored in
full. Do not read from them into tracked files, and do not cite them.
