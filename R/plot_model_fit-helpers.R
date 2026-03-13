plot_model_fit_supported_metrics <- function() {
  c("CFI", "TLI", "RMSEA", "SRMR")
}

plot_model_fit_metric_spec <- function(metrics = NULL) {
  spec <- data.frame(
    Metric = plot_model_fit_supported_metrics(),
    Direction = c("higher", "higher", "lower", "lower"),
    Good = c(0.90, 0.90, 0.08, 0.08),
    Ideal = c(0.95, 0.95, 0.05, 0.06),
    Poor = c(0.80, 0.80, 0.12, 0.12),
    Primary = c(0.95, 0.90, 0.08, 0.05),
    AxisMin = c(0.80, 0.80, 0.00, 0.00),
    AxisMax = c(1.00, 1.00, 0.14, 0.14),
    Panel = c(
      "Incremental fit (CFI & TLI)",
      "Incremental fit (CFI & TLI)",
      "Approximation error (RMSEA & SRMR)",
      "Approximation error (RMSEA & SRMR)"
    ),
    ShowInterval = c(FALSE, FALSE, TRUE, FALSE),
    IntervalLowCol = c(NA_character_, NA_character_, "RMSEA_CI_low", NA_character_),
    IntervalHighCol = c(NA_character_, NA_character_, "RMSEA_CI_high", NA_character_),
    stringsAsFactors = FALSE
  )

  if (is.null(metrics)) {
    return(spec)
  }

  spec[match(metrics, spec$Metric), , drop = FALSE]
}

plot_model_fit_model_palette <- function(n_models) {
  base_palette <- c("#2aa198", "#d1a013", "#7b6db6", "#d95f5f", "#4f8ad9", "#7f9f3f")
  if (n_models <= length(base_palette)) {
    return(base_palette[seq_len(n_models)])
  }
  grDevices::hcl.colors(n_models, palette = "Dynamic")
}

plot_model_fit_variant_shapes <- function(n_variants) {
  base_shapes <- c(16, 17, 15, 18, 8, 3, 7, 9)
  if (n_variants <= length(base_shapes)) {
    return(base_shapes[seq_len(n_variants)])
  }
  rep(base_shapes, length.out = n_variants)
}

plot_model_fit_validate_input <- function(x) {
  if (inherits(x, "compare_model_fit")) {
    if (!"MODEL" %in% names(x)) {
      cli::cli_abort("`compare_model_fit` objects must contain a `MODEL` column.")
    }
    return("compare_model_fit")
  }

  if (inherits(x, "model_fit")) {
    return("model_fit")
  }

  fit_class <- class(x)
  if (is.null(fit_class) || length(fit_class) == 0L) {
    fit_class <- typeof(x)
  }
  fit_class <- paste(fit_class, collapse = ", ")

  cli::cli_abort(c(
    sprintf("Objects of class '%s' are not currently supported by `plot_model_fit()`.", fit_class),
    "`plot_model_fit()` requires a `model_fit` or `compare_model_fit` object, not a raw fitted model.",
    "For one fitted model, call `model_fit(fit)` first.",
    "For multiple fitted models, call `compare_model_fit(...)` first."
  ))
}

plot_model_fit_resolve_test_mode <- function(test_mode) {
  supported <- c("all", "non_standard", "standard_only", "primary")

  if (!is.character(test_mode) || length(test_mode) != 1L || is.na(test_mode)) {
    cli::cli_abort("`test_mode` must be a single character string.")
  }

  test_mode <- trimws(test_mode)
  if (!test_mode %in% supported) {
    cli::cli_abort(c(
      sprintf("Unsupported `test_mode = '%s'`.", test_mode),
      sprintf("Supported values: %s.", paste(supported, collapse = ", "))
    ))
  }

  test_mode
}

plot_model_fit_resolve_type <- function(type, input_class, n_rows) {
  supported <- c("default", "bullet", "dots", "bars", "heatmap")

  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    cli::cli_abort("`type` must be a single character string.")
  }

  type <- trimws(type)

  if (!type %in% supported) {
    cli::cli_abort(c(
      sprintf("Unsupported `type = '%s'` for `%s` objects.", type, input_class),
      sprintf("Supported values: %s.", paste(supported, collapse = ", "))
    ))
  }

  if (type == "default") {
    if (identical(input_class, "model_fit") && n_rows == 1L) {
      return("bullet")
    }
    return("dots")
  }

  type
}

plot_model_fit_resolve_metrics <- function(x, metrics = NULL, verbose = TRUE) {
  supported <- plot_model_fit_supported_metrics()

  if (is.null(metrics)) {
    selected <- supported[supported %in% names(x)]
    if (length(selected) == 0L) {
      cli::cli_abort("No supported fit indices are available to plot.")
    }
    return(selected)
  }

  if (!is.character(metrics) || length(metrics) == 0L) {
    cli::cli_abort("`metrics` must be NULL or a non-empty character vector.")
  }

  metrics <- toupper(trimws(metrics))
  metrics <- metrics[metrics != ""]
  metrics <- unique(metrics)
  unknown <- setdiff(metrics, supported)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      sprintf(
        "Unsupported metric name%s: %s.",
        if (length(unknown) > 1L) "s" else "",
        paste(unknown, collapse = ", ")
      ),
      sprintf("Supported metrics: %s.", paste(supported, collapse = ", "))
    ))
  }

  available <- metrics[metrics %in% names(x)]
  dropped <- setdiff(metrics, available)
  if (length(dropped) > 0L && isTRUE(verbose)) {
    cli::cli_inform(
      "Requested metrics not present in the object were dropped: {dropped}."
    )
  }
  if (length(available) == 0L) {
    cli::cli_abort("None of the requested metrics are available in the object.")
  }

  available
}

plot_model_fit_prepare_single_df <- function(x, metrics) {
  keep <- unique(c(metrics, "RMSEA_CI_low", "RMSEA_CI_high", "TEST", "ESTIMATOR"))
  keep <- keep[keep %in% names(x)]
  data.frame(
    MODEL_BASE = "Model",
    x[, keep, drop = FALSE],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

plot_model_fit_prepare_compare_df <- function(x, metrics) {
  keep <- unique(c("MODEL", metrics, "RMSEA_CI_low", "RMSEA_CI_high", "TEST", "ESTIMATOR"))
  keep <- keep[keep %in% names(x)]
  out <- x[, keep, drop = FALSE]
  out$MODEL_BASE <- as.character(out$MODEL)
  out$MODEL <- NULL
  out
}

plot_model_fit_is_standard_row <- function(df) {
  out <- rep(FALSE, nrow(df))
  if (nrow(df) == 0L) {
    return(out)
  }

  rows_by_model <- split(seq_len(nrow(df)), df$MODEL_BASE)

  for (idx in rows_by_model) {
    model_df <- df[idx, , drop = FALSE]

    if ("TEST" %in% names(model_df)) {
      test_label <- trimws(as.character(model_df$TEST))
      test_label[test_label == ""] <- NA_character_
      if (any(!is.na(test_label))) {
        out[idx] <- test_label %in% c("standard", "default", "none")
        next
      }
    }

    if ("ESTIMATOR" %in% names(model_df) && nrow(model_df) > 1L) {
      estimator_label <- trimws(as.character(model_df$ESTIMATOR))
      estimator_label[estimator_label == ""] <- NA_character_
      if (length(unique(stats::na.omit(estimator_label))) > 1L &&
          !is.na(estimator_label[1]) &&
          !estimator_label[1] %in% estimator_label[-1]) {
        out[idx[1]] <- TRUE
      }
    }
  }

  out
}

plot_model_fit_variant_labels <- function(df) {
  n <- nrow(df)
  if (n == 0L) {
    return(character(0))
  }

  variant <- rep(NA_character_, n)
  if ("TEST" %in% names(df)) {
    variant <- trimws(as.character(df$TEST))
    variant[variant == ""] <- NA_character_
  }

  estimator_label <- rep(NA_character_, n)
  if ("ESTIMATOR" %in% names(df)) {
    estimator_label <- trimws(as.character(df$ESTIMATOR))
    estimator_label[estimator_label == ""] <- NA_character_
  }

  need_estimator <- is.na(variant)
  variant[need_estimator] <- estimator_label[need_estimator]

  row_fallback <- paste0("Row ", seq_len(n))
  variant[is.na(variant)] <- row_fallback[is.na(variant)]

  duplicated_variant <- duplicated(variant) | duplicated(variant, fromLast = TRUE)
  if (any(duplicated_variant)) {
    dup_index <- ave(seq_len(n), variant, FUN = seq_along)
    variant[duplicated_variant] <- paste0(variant[duplicated_variant], " (Row ", dup_index[duplicated_variant], ")")
  }

  variant
}

plot_model_fit_apply_test_mode <- function(df, test_mode) {
  if (nrow(df) == 0L) {
    return(df)
  }

  rows_by_model <- split(seq_len(nrow(df)), df$MODEL_BASE)
  keep_idx <- integer(0)

  for (idx in rows_by_model) {
    model_df <- df[idx, , drop = FALSE]
    standard_idx <- which(model_df$IS_STANDARD)
    non_standard_idx <- setdiff(seq_len(nrow(model_df)), standard_idx)

    selected_idx <- switch(
      test_mode,
      all = seq_len(nrow(model_df)),
      non_standard = if (length(non_standard_idx) > 0L) non_standard_idx else seq_len(nrow(model_df)),
      standard_only = standard_idx,
      primary = if (length(non_standard_idx) > 0L) non_standard_idx[1] else 1L
    )

    if (length(selected_idx) > 0L) {
      keep_idx <- c(keep_idx, idx[selected_idx])
    }
  }

  df[keep_idx, , drop = FALSE]
}

plot_model_fit_finalize_plot_rows <- function(df) {
  if (nrow(df) == 0L) {
    return(df)
  }

  rows_by_model <- split(seq_len(nrow(df)), df$MODEL_BASE)
  variant <- character(nrow(df))
  plot_id <- character(nrow(df))

  for (idx in rows_by_model) {
    model_df <- df[idx, , drop = FALSE]
    model_variant <- plot_model_fit_variant_labels(model_df)
    variant[idx] <- model_variant
    if (length(idx) == 1L) {
      plot_id[idx] <- model_df$MODEL_BASE[1]
    } else {
      plot_id[idx] <- paste0(model_df$MODEL_BASE, " • ", model_variant)
    }
  }

  df$VARIANT <- variant
  df$PLOT_ID <- plot_id
  df$PLOT_ORDER <- seq_len(nrow(df))
  df
}

plot_model_fit_prepare_data <- function(x, metrics, input_class, test_mode) {
  out <- if (identical(input_class, "model_fit")) {
    plot_model_fit_prepare_single_df(x, metrics)
  } else {
    plot_model_fit_prepare_compare_df(x, metrics)
  }

  out$MODEL_BASE <- as.character(out$MODEL_BASE)
  out$IS_STANDARD <- plot_model_fit_is_standard_row(out)
  out$ROW_IN_MODEL <- ave(seq_len(nrow(out)), out$MODEL_BASE, FUN = seq_along)
  out <- plot_model_fit_apply_test_mode(out, test_mode)

  if (nrow(out) == 0L) {
    cli::cli_abort(c(
      sprintf("`test_mode = '%s'` left no rows available to plot.", test_mode),
      "Choose a different `test_mode` or create a fit table with compatible test rows."
    ))
  }

  plot_model_fit_finalize_plot_rows(out)
}

plot_model_fit_extract_interval_df <- function(fit_df, metric_spec) {
  interval_metrics <- metric_spec[metric_spec$ShowInterval, , drop = FALSE]
  if (nrow(interval_metrics) == 0L) {
    return(NULL)
  }

  rows <- lapply(seq_len(nrow(interval_metrics)), function(i) {
    row <- interval_metrics[i, ]
    if (!all(c(row$IntervalLowCol, row$IntervalHighCol) %in% names(fit_df))) {
      return(NULL)
    }
    ci_low <- fit_df[[row$IntervalLowCol]]
    ci_high <- fit_df[[row$IntervalHighCol]]
    if (all(is.na(ci_low)) || all(is.na(ci_high))) {
      return(NULL)
    }
    data.frame(
      MODEL_BASE = fit_df$MODEL_BASE,
      PLOT_ID = fit_df$PLOT_ID,
      VARIANT = fit_df$VARIANT,
      Metric = row$Metric,
      CI_low = ci_low,
      CI_high = ci_high,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(NULL)
  }

  do.call(rbind, rows)
}

plot_model_fit_single_band_spec <- function(metrics) {
  rows <- list()
  for (metric in metrics) {
    if (metric %in% c("CFI", "TLI")) {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        xmin = c(0.80, 0.90, 0.95),
        xmax = c(0.90, 0.95, 1.00),
        band = c("Needs work", "Acceptable", "Good"),
        stringsAsFactors = FALSE
      )
    } else if (metric == "RMSEA") {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        xmin = c(0.00, 0.05, 0.08, 0.10),
        xmax = c(0.05, 0.08, 0.10, 0.14),
        band = c("Good", "Acceptable", "Near limit", "Needs work"),
        stringsAsFactors = FALSE
      )
    } else if (metric == "SRMR") {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        xmin = c(0.00, 0.06, 0.08),
        xmax = c(0.06, 0.08, 0.14),
        band = c("Good", "Almost good", "Needs work"),
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  out$Metric <- factor(out$Metric, levels = metrics)
  out
}

plot_model_fit_cutoff_spec <- function(metrics, style = c("single", "grouped")) {
  style <- match.arg(style)
  spec <- plot_model_fit_metric_spec(metrics)
  rows <- lapply(seq_len(nrow(spec)), function(i) {
    row <- spec[i, ]
    cutoffs <- if (style == "grouped") {
      row$Primary
    } else if (row$Metric %in% c("CFI", "TLI")) {
      c(row$Good, row$Ideal)
    } else if (row$Metric == "RMSEA") {
      c(row$Ideal, row$Good)
    } else {
      c(row$Ideal, row$Good)
    }

    data.frame(
      Metric = row$Metric,
      cutoff = cutoffs,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$Metric <- factor(out$Metric, levels = metrics)
  out
}

plot_model_fit_tick_spec <- function(metrics) {
  rows <- list()
  for (metric in metrics) {
    if (metric %in% c("CFI", "TLI")) {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        x = c(0.80, 0.85, 0.90, 0.95, 1.00),
        label = c("0.80", "0.85", "0.90", "0.95", "1.00"),
        stringsAsFactors = FALSE
      )
    } else if (metric == "RMSEA") {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        x = c(0.00, 0.05, 0.08, 0.10),
        label = c("0.00", "0.05", "0.08", "0.10"),
        stringsAsFactors = FALSE
      )
    } else if (metric == "SRMR") {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        x = c(0.00, 0.06, 0.08, 0.10),
        label = c("0.00", "0.06", "0.08", "0.10"),
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  out$Metric <- factor(out$Metric, levels = metrics)
  cutoff_df <- plot_model_fit_cutoff_spec(metrics, style = "single")
  out$is_cutoff <- mapply(function(metric, x) {
    x %in% cutoff_df$cutoff[cutoff_df$Metric == metric]
  }, as.character(out$Metric), out$x)
  out
}

plot_model_fit_assign_band <- function(metric, value, band_df) {
  if (is.na(value)) {
    return(NA_character_)
  }
  segments <- band_df[band_df$Metric == metric, , drop = FALSE]
  hit <- which(value >= segments$xmin & value <= segments$xmax)
  if (length(hit) == 0L) {
    hit <- if (value < min(segments$xmin)) 1L else nrow(segments)
  }
  segments$band[hit[1]]
}

plot_model_fit_rescale <- function(values, to = c(0, 1), from = range(values, na.rm = TRUE)) {
  if (diff(from) == 0) {
    return(rep(mean(to), length(values)))
  }
  (values - from[1]) / diff(from) * diff(to) + to[1]
}

plot_model_fit_compute_band_score <- function(value, direction, poor, good, ideal) {
  if (is.na(value)) {
    return(NA_real_)
  }
  if (direction == "higher") {
    if (value < good) {
      score <- -1 + (value - poor) / (good - poor)
      return(max(min(score, -0.05), -1))
    }
    if (value < ideal) {
      score <- 0.25 + 0.50 * (value - good) / (ideal - good)
      return(min(max(score, 0.25), 0.75))
    }
    score <- 0.75 + 0.25 * (value - ideal) / (1 - ideal)
    return(min(max(score, 0.75), 1))
  }

  if (value > good) {
    score <- -1 + (poor - value) / (poor - good)
    return(max(min(score, -0.05), -1))
  }
  if (value > ideal) {
    score <- 0.25 + 0.50 * (good - value) / (good - ideal)
    return(min(max(score, 0.25), 0.75))
  }
  score <- 0.75 + 0.25 * (ideal - value) / ideal
  min(max(score, 0.75), 1)
}

plot_model_fit_choose_incremental_ymin <- function(values) {
  min_value <- suppressWarnings(min(values, na.rm = TRUE))
  if (!is.finite(min_value) || min_value >= 0.80) {
    return(0.80)
  }
  if (min_value >= 0.50) {
    return(0.50)
  }
  if (min_value >= 0.20) {
    return(0.20)
  }
  0.00
}

plot_model_fit_build_model_spec <- function(model_names) {
  n_models <- length(model_names)
  offsets <- if (n_models == 1L) 0 else seq(-0.24, 0.24, length.out = n_models)
  data.frame(
    MODEL_BASE = model_names,
    Fill = plot_model_fit_model_palette(n_models),
    Offset = offsets,
    stringsAsFactors = FALSE
  )
}
