# Repository Guidelines

## Project Structure & Module Organization
- `R/` holds core functions and S3 methods (for example `model_fit`, `compare_model_fit`, and `print.*`) across multiple model classes.
- `man/` is generated Rd documentation; `man/figures/` stores README assets like the hex logo.
- `inst/templates/` contains APA-styled Word templates used by `save_table()`.
- `README.Rmd` is the source of `README.md`; `NEWS.md` tracks user-facing changes; `DESCRIPTION` and `NAMESPACE` define package metadata.

## Build, Test, and Development Commands
- `R CMD build .` — build a source tarball.
- `R CMD check .` — run full package checks.
- `R -q -e "roxygen2::roxygenise()"` — regenerate `man/` and `NAMESPACE` after doc changes.
- `R -q -e "rmarkdown::render('README.Rmd')"` — refresh `README.md` from the Rmd source.

## Coding Style & Naming Conventions
- Use two-space indentation and `snake_case` for functions and variables.
- Document exported functions with roxygen2 (`@param`, `@return`, `@export`).
- Do not hand-edit generated files (`NAMESPACE`, `man/*.Rd`).

## Testing Guidelines
- There is no `tests/` suite yet; rely on `R CMD check` and runnable examples.
- The `v0.1.5` roadmap targets a `testthat` suite and fixes for `R CMD check` warnings/notes; see `ROADMAP.md` for scope.
- If adding tests, follow `testthat` conventions (`tests/testthat/test-*.R`) and add `testthat` to `Suggests`.
- For tests/examples involving fit indices, avoid a 1-factor/3-item model (df = 0 gives identical indices). Use at least 1 factor with 4 items or 2 factors with 3 items each.

## Dependencies & Optional Features
- Keep mandatory dependencies minimal; most integrations live in `Suggests` (`lavaan`, `ggplot2`, `flextable`, `officer`).
- For optional model classes, guard usage with `rlang::check_installed()` and provide clear messages instead of hard dependencies.
- Assume users already have the package that produced their model objects; avoid forcing installs for unrelated workflows.

## Roadmap & Planning
- `ROADMAP.md` is the source of truth for milestones; GitHub issues linked there contain the detailed specs.
- Near-term priorities include SEM support for `lavaan`, new estimate helpers, invariance tooling, and expanding model-class support (`mirt`, `psych`).

## Architecture: Tables & Methods
- Table pipeline: `prepare_table()` (internal) builds a `data.frame` with `insight::format_table()`; `format_results()` (public) returns `text`/`markdown`/`html`/`auto`; `print.*` is console-only and accepts `...` for S3 consistency.
- File structure (rule of thumb): `model_fit-<backend>.R` for `model_fit.<external_class>()` and backend helpers; `model_fit-methods.R` for `model_fit` result methods; `compare_model_fit.R` for the main function; `compare_model_fit-methods.R` for `compare_model_fit` result methods; `format_results.R` and `prepare_table.R` for generics and shared helpers; `helpers-formatting.R` for internal formatting helpers.
- Backend checklist: 1) create `R/model_fit-<backend>.R`; 2) implement `model_fit.<class>()`; 3) ensure the return is compatible with result methods (print/format_results/prepare_table).

## Commit & Pull Request Guidelines
- Commit history uses short, imperative messages and common prefixes like `feat:`, `fix:`, `docs:`, `chore:`.
- PRs should include a brief summary, tests run (or note if not run), and updates to `NEWS.md` for user-visible changes.
- When asked to verify recent changes, use `git log --oneline --left-right --graph --decorate origin/master...gitbutler/workspace` and other non-destructive history/diff commands as needed.
- This agent does not create commits or PRs (GitButler workflow). When requested or during release prep, assist with entries in `NEWS.md`.

# ExecPlans
 
When writing complex features or significant refactors, use an ExecPlan (as described in .agent/PLANS.md) from design to implementation.
