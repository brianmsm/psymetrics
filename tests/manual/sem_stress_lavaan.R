## Manual stress script for local QA (not run by testthat/CRAN checks).
## Run from the project root with:
##   Rscript tests/manual/sem_stress_lavaan.R
suppressPackageStartupMessages({
  library(devtools)
})

if (!requireNamespace("lavaan", quietly = TRUE)) {
  cat("Skipping SEM stress run: package 'lavaan' is not installed.\n")
  quit(save = "no", status = 0)
}

devtools::load_all(".", quiet = TRUE)

has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

results <- data.frame(
  case = character(),
  action = character(),
  status = character(),
  warning_count = integer(),
  message_count = integer(),
  note = character(),
  stringsAsFactors = FALSE
)

record_result <- function(case_name, action_name, status_name,
                          warnings = character(0),
                          messages = character(0),
                          note = "") {
  results <<- rbind(
    results,
    data.frame(
      case = case_name,
      action = action_name,
      status = status_name,
      warning_count = length(warnings),
      message_count = length(messages),
      note = substr(note, 1, 260),
      stringsAsFactors = FALSE
    )
  )
}

run_action <- function(case_name, action_name, expr,
                       expect_error = FALSE,
                       expect_silent = TRUE) {
  warnings <- character(0)
  messages <- character(0)
  err <- NULL

  withCallingHandlers(
    {
      tryCatch(
        force(expr),
        error = function(e) {
          err <<- e
          NULL
        }
      )
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  if (!is.null(err)) {
    if (isTRUE(expect_error)) {
      record_result(case_name, action_name, "expected_error", warnings, messages, conditionMessage(err))
    } else {
      record_result(case_name, action_name, "error", warnings, messages, conditionMessage(err))
    }
    return(invisible(NULL))
  }

  if (isTRUE(expect_error)) {
    record_result(
      case_name,
      action_name,
      "missing_expected_error",
      warnings,
      messages,
      "Expected an error, but call succeeded."
    )
    return(invisible(NULL))
  }

  if (isTRUE(expect_silent) && (length(warnings) > 0L || length(messages) > 0L)) {
    noisy_note <- paste(unique(c(warnings, messages)), collapse = " | ")
    record_result(case_name, action_name, "noisy", warnings, messages, noisy_note)
    return(invisible(NULL))
  }

  note <- ""
  if (length(warnings) > 0L || length(messages) > 0L) {
    note <- paste(unique(c(warnings, messages)), collapse = " | ")
  }
  record_result(case_name, action_name, "ok", warnings, messages, note)
  invisible(NULL)
}

safe_fit <- function(expr) {
  tryCatch(
    suppressWarnings(force(expr)),
    error = function(e) NULL
  )
}

sem_model <- "
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
textual ~ visual
speed ~ visual + textual
"

growth_model <- "
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
"

twolevel_model <- "
level: 1
  fw =~ y1 + y2 + y3
  fw ~ x1 + x2 + x3
level: 2
  fb =~ y1 + y2 + y3
  fb ~ w1 + w2
"

hs <- lavaan::HolzingerSwineford1939
hs_ord <- hs
ordered_vars <- paste0("x", 1:9)
hs_ord[ordered_vars] <- lapply(hs_ord[ordered_vars], function(x) {
  cut(x, breaks = 3, include.lowest = TRUE, ordered_result = TRUE)
})

hs_missing <- hs
set.seed(42)
hs_missing[sample(seq_len(nrow(hs_missing)), 60), "x1"] <- NA
hs_missing[sample(seq_len(nrow(hs_missing)), 50), "x5"] <- NA

fits <- list(
  sem_ml = safe_fit(lavaan::sem(sem_model, data = hs, estimator = "ML")),
  sem_mlr = safe_fit(lavaan::sem(sem_model, data = hs, estimator = "MLR")),
  sem_mlm = safe_fit(lavaan::sem(sem_model, data = hs, estimator = "MLM")),
  sem_mlmv = safe_fit(lavaan::sem(sem_model, data = hs, estimator = "MLMV")),
  sem_mlmvs = safe_fit(lavaan::sem(sem_model, data = hs, estimator = "MLMVS")),
  sem_dwls = safe_fit(lavaan::sem(sem_model, data = hs, estimator = "DWLS")),
  sem_uls = safe_fit(lavaan::sem(sem_model, data = hs, estimator = "ULS")),
  sem_wlsmv = safe_fit(
    lavaan::sem(
      sem_model,
      data = hs_ord,
      estimator = "WLSMV",
      ordered = ordered_vars
    )
  ),
  sem_test_none = safe_fit(lavaan::sem(sem_model, data = hs, test = "none")),
  sem_bollen_stine = safe_fit(
    lavaan::sem(
      sem_model,
      data = hs,
      estimator = "ML",
      test = "bollen.stine",
      bootstrap = 20
    )
  ),
  sem_fiml = safe_fit(
    lavaan::sem(
      sem_model,
      data = hs_missing,
      estimator = "MLR",
      missing = "fiml"
    )
  ),
  sem_multigroup = safe_fit(
    lavaan::sem(
      sem_model,
      data = hs,
      group = "school"
    )
  ),
  sem_twolevel = safe_fit(
    lavaan::sem(
      twolevel_model,
      data = lavaan::Demo.twolevel,
      cluster = "cluster"
    )
  ),
  sem_growth = safe_fit(
    lavaan::growth(
      growth_model,
      data = lavaan::Demo.growth
    )
  ),
  sem_nonconverged = safe_fit(
    lavaan::sem(
      sem_model,
      data = hs[1:20, ],
      estimator = "MLR",
      control = list(iter.max = 1)
    )
  ),
  sem_path_only = safe_fit(
    lavaan::sem(
      "y ~ x1 + x2\nx1 ~~ x2",
      data = transform(mtcars, x1 = wt, x2 = hp, y = mpg)
    )
  )
)

cat("SEM stress matrix:\n")
for (fit_name in names(fits)) {
  if (is.null(fits[[fit_name]])) {
    cat(sprintf("- %s: unavailable\n", fit_name))
  } else {
    cat(sprintf("- %s: ready\n", fit_name))
  }
}

fit_reference <- fits$sem_ml
if (is.null(fit_reference)) {
  first_ready <- names(fits)[!vapply(fits, is.null, logical(1))]
  if (length(first_ready) > 0L) {
    fit_reference <- fits[[first_ready[[1]]]]
  }
}

for (fit_name in names(fits)) {
  fit <- fits[[fit_name]]
  if (is.null(fit)) {
    record_result(
      fit_name,
      "fit_init",
      "skipped_unavailable",
      note = "Fit could not be estimated in this lavaan setup."
    )
    next
  }

  run_action(
    fit_name,
    "model_fit_default",
    psymetrics::model_fit(fit, verbose = FALSE),
    expect_silent = TRUE
  )
  run_action(
    fit_name,
    "model_fit_scaled",
    psymetrics::model_fit(fit, type = "scaled", verbose = FALSE),
    expect_silent = TRUE
  )
  run_action(
    fit_name,
    "model_fit_robust",
    psymetrics::model_fit(fit, type = "robust", verbose = FALSE),
    expect_silent = TRUE
  )
  run_action(
    fit_name,
    "model_fit_details",
    psymetrics::model_fit(
      fit,
      standard_test = TRUE,
      test_details = TRUE,
      verbose = FALSE
    ),
    expect_silent = TRUE
  )

  if (!is.null(fit_reference) && !identical(fit, fit_reference)) {
    run_action(
      fit_name,
      "compare_model_fit_vs_reference",
      psymetrics::compare_model_fit(
        reference = fit_reference,
        candidate = fit,
        type = "robust",
        test_details = TRUE,
        verbose = FALSE
      ),
      expect_silent = TRUE
    )
  }

  if (!isTRUE(has_ggplot2)) {
    record_result(fit_name, "plot_factor_loadings", "skipped_no_ggplot2", note = "ggplot2 not installed.")
    next
  }

  if (identical(fit_name, "sem_path_only")) {
    run_action(
      fit_name,
      "plot_factor_loadings",
      psymetrics::plot_factor_loadings(fit, verbose = FALSE),
      expect_error = TRUE,
      expect_silent = TRUE
    )
  } else if (identical(fit_name, "sem_multigroup")) {
    run_action(
      fit_name,
      "plot_factor_loadings_group",
      psymetrics::plot_factor_loadings(fit, facet_by = "group", verbose = FALSE),
      expect_silent = TRUE
    )
  } else if (identical(fit_name, "sem_twolevel")) {
    run_action(
      fit_name,
      "plot_factor_loadings_level",
      psymetrics::plot_factor_loadings(fit, facet_by = "level", verbose = FALSE),
      expect_silent = TRUE
    )
  } else {
    run_action(
      fit_name,
      "plot_factor_loadings",
      psymetrics::plot_factor_loadings(fit, verbose = FALSE),
      expect_silent = TRUE
    )
  }
}

cat("\nStatus counts:\n")
print(as.data.frame(table(results$status), stringsAsFactors = FALSE), row.names = FALSE)

problem_rows <- results[results$status %in% c("error", "missing_expected_error", "noisy"), , drop = FALSE]
if (nrow(problem_rows) > 0L) {
  cat("\nProblem rows:\n")
  print(problem_rows, row.names = FALSE)
}

if (nrow(problem_rows) > 0L) {
  quit(save = "no", status = 1)
}

quit(save = "no", status = 0)
