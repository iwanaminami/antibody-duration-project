# Duration of antibody protection project

This repository supports the Duration of Antibody Protection project at the
Nagoya-Oxford Workshop 2026.

A vaccine trial measures an antibody marker repeatedly after vaccination, and
the marker falls quickly at first and then more slowly. At each planned trial
review we want to estimate when it will fall below a level associated with
protection — often beyond the period observed so far — and to say how much
would be gained from further follow-up. This repository holds the simulation
study that answers that question.

[notes/handout.pdf](notes/handout.pdf) is the participant handout and the
methodological source of truth: the question, the estimand, the candidate
models and their starting values.

## Getting started

Requires R 4.3 or later. If you do not have R, [rig](https://github.com/r-lib/rig)
is the least painful way to install and switch between versions:

```sh
brew install --cask rig && rig add release   # macOS; see rig's README for Linux and Windows
```

Then, from the repository root:

```r
renv::restore()        # install the pinned package versions (first time only)
pkgload::load_all()    # load the project
testthat::test_local() # run the tests
```

`renv` bootstraps itself from `.Rprofile` when you open the project, so
`renv::restore()` works in a fresh clone with nothing else installed.

The cheap end-to-end check, which is also what to run before opening a pull
request:

```sh
Rscript scripts/00-integration-gate.R
```

## Layout

| Path | Contents |
| --- | --- |
| [notes/](notes/) | Participant handout and bibliography |
| [R/](R/) | Package source: shared schemas, models, one file per workstream |
| [inst/config/default.yaml](inst/config/default.yaml) | Calibration, design grid and adequacy rules |
| [scripts/](scripts/) | Entry points, starting with the integration gate |
| [tests/testthat/](tests/testthat/) | Test suite |
| [INTERFACES.md](INTERFACES.md) | The shared contract — read this before writing code |
| [AGENTS.md](AGENTS.md) | Conventions, for coding agents and for people |

## Workstreams

Work is split five ways so that it can proceed in parallel. Each workstream
owns its own file and builds against the shared schemas; nobody waits for
anybody, because `mock_sim_dataset()` and `mock_fit_result()` stand in for
work that has not landed yet.

| | Workstream | File |
| --- | --- | --- |
| (a) | Coordination, simulation and visualisation | `R/simulate.R`, `R/driver.R`, `R/score.R`, `R/plots.R` |
| (b) | Simple analysis: biphasic model by least squares | `R/fit-ls.R` |
| (c) | Alternate models, model selection and MCMC | `R/fit-mcmc.R` |
| (d) | Nonlinear mixed-effects models | `R/fit-nlme.R` |
| (e) | Calibration, thresholds and protection mapping | `R/calibrate.R` |

The contract between them — the four schemas, the units convention and the
rule that a trajectory which never crosses the threshold is `Inf` — is in
[INTERFACES.md](INTERFACES.md).

## Contributing

Fork the [main repository](https://github.com/will-s-hart/antibody-duration-project),
create a branch in your fork, and open a pull request from that branch against
`main` in the main repository.

Please run `testthat::test_local()` and the integration gate before opening a
pull request, and see [AGENTS.md](AGENTS.md) for the conventions the project
relies on.
