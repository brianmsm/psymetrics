#' @export
plot_model_fit.model_fit <- function(x, type = "default", metrics = NULL, verbose = TRUE, ...) {
  rlang::check_installed("ggplot2", reason = "to create model fit plots.")
  input_class <- plot_model_fit_validate_input(x)
  plot_model_fit_resolve_type(type, input_class)
  resolved_metrics <- plot_model_fit_resolve_metrics(x, metrics = metrics, verbose = verbose)
  fit_df <- plot_model_fit_prepare_data(x, resolved_metrics, input_class)
  metric_spec <- plot_model_fit_metric_spec(resolved_metrics)

  plot_model_fit_single_bullet(fit_df, metric_spec)
}

#' @export
plot_model_fit.compare_model_fit <- function(x, type = "default", metrics = NULL, verbose = TRUE, ...) {
  rlang::check_installed("ggplot2", reason = "to create model fit plots.")
  input_class <- plot_model_fit_validate_input(x)
  resolved_type <- plot_model_fit_resolve_type(type, input_class)
  resolved_metrics <- plot_model_fit_resolve_metrics(x, metrics = metrics, verbose = verbose)
  fit_df <- plot_model_fit_prepare_data(x, resolved_metrics, input_class)
  metric_spec <- plot_model_fit_metric_spec(resolved_metrics)

  if (identical(resolved_type, "dots")) {
    return(plot_model_fit_threshold_dots(fit_df, metric_spec))
  }
  if (identical(resolved_type, "bars")) {
    return(plot_model_fit_grouped_threshold_bars(fit_df, metric_spec))
  }
  plot_model_fit_heatmap_scorecard(fit_df, metric_spec)
}

plot_model_fit_single_bullet <- function(fit_df, metric_spec) {
  metrics <- metric_spec$Metric
  band_spec <- plot_model_fit_single_band_spec(metrics)
  cutoff_spec <- plot_model_fit_cutoff_spec(metrics, style = "single")
  tick_spec <- plot_model_fit_tick_spec(metrics)
  interval_df <- plot_model_fit_extract_interval_df(fit_df, metric_spec)

  value_df <- data.frame(
    Metric = factor(metrics, levels = metrics),
    Value = as.numeric(fit_df[1, metrics, drop = TRUE]),
    AxisMin = metric_spec$AxisMin,
    AxisMax = metric_spec$AxisMax,
    stringsAsFactors = FALSE
  )
  value_df$ValueLabel <- ifelse(is.na(value_df$Value), "", sprintf("%.3f", value_df$Value))
  value_df$Status <- vapply(
    seq_len(nrow(value_df)),
    function(i) plot_model_fit_assign_band(as.character(value_df$Metric[i]), value_df$Value[i], band_spec),
    character(1)
  )

  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    idx <- match(interval_df$Metric, value_df$Metric)
    value_df$ValueLabel[idx] <- ifelse(
      is.na(value_df$Value[idx]),
      "",
      sprintf("%.3f [%.3f, %.3f]", value_df$Value[idx], interval_df$CI_low, interval_df$CI_high)
    )
  }

  layout <- list(
    point_y = 1.00,
    band_ymin = 0.80,
    band_ymax = 1.20,
    tick_y = 0.71,
    plain_label_y = 1.225,
    curve_yend = 1.16,
    proximity_threshold = 0.035,
    callout_x_offset = 0.05,
    callout_xend_offset = 0.010
  )

  value_df <- plot_model_fit_single_callout_layout(value_df, cutoff_spec, layout)
  valid_df <- value_df[is.finite(value_df$Value), , drop = FALSE]
  callout_df <- valid_df[valid_df$use_callout, , drop = FALSE]
  plain_df <- valid_df[!valid_df$use_callout, , drop = FALSE]

  axis_df <- data.frame(
    Metric = factor(metrics, levels = metrics),
    x = metric_spec$AxisMin,
    y = layout$tick_y,
    stringsAsFactors = FALSE
  )
  axis_df2 <- data.frame(
    Metric = factor(metrics, levels = metrics),
    x = metric_spec$AxisMax,
    y = layout$tick_y,
    stringsAsFactors = FALSE
  )

  band_fill <- c(
    "Good" = "#9fd39a",
    "Almost good" = "#c9d97a",
    "Acceptable" = "#dfc55b",
    "Near limit" = "#d6d6d6",
    "Needs work" = "#bebebe"
  )
  status_fill <- c(
    "Good" = "#137f5b",
    "Almost good" = "#6d9d29",
    "Acceptable" = "#b77f00",
    "Near limit" = "#8b8b8b",
    "Needs work" = "#7e7e7e"
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = band_spec,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = layout$band_ymin, ymax = layout$band_ymax, fill = .data$band),
      color = NA
    ) +
    ggplot2::geom_segment(
      data = cutoff_spec,
      ggplot2::aes(x = .data$cutoff, xend = .data$cutoff, y = layout$tick_y + 0.03, yend = layout$band_ymax),
      color = "#676767",
      linewidth = 0.8
    )

  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    p <- p + ggplot2::geom_segment(
      data = interval_df,
      ggplot2::aes(x = .data$CI_low, xend = .data$CI_high, y = layout$point_y, yend = layout$point_y),
      linewidth = 3,
      color = "#2f2f2f"
    )
  }

  p <- p +
    ggplot2::geom_curve(
      data = callout_df,
      ggplot2::aes(x = .data$Value, y = layout$point_y + 0.02, xend = .data$curve_xend, yend = layout$curve_yend),
      curvature = 0.18,
      linewidth = 0.75,
      color = "#505050",
      arrow = grid::arrow(length = grid::unit(0.11, "inches"), type = "closed")
    ) +
    ggplot2::geom_point(
      data = valid_df,
      ggplot2::aes(x = .data$Value, y = layout$point_y, fill = .data$Status),
      size = 6.6,
      shape = 21,
      color = "white",
      stroke = 1.2
    ) +
    ggplot2::geom_text(
      data = plain_df,
      ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$ValueLabel),
      color = "#1f1f1f",
      size = 4.6 / ggplot2::.pt
    ) +
    ggplot2::geom_text(
      data = callout_df,
      ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$ValueLabel, hjust = .data$label_hjust),
      color = "#1f1f1f",
      size = 4.6 / ggplot2::.pt
    ) +
    ggplot2::geom_text(
      data = tick_spec[!tick_spec$is_cutoff, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = layout$tick_y, label = .data$label),
      color = "#5a5a5a",
      size = 3.9 / ggplot2::.pt
    ) +
    ggplot2::geom_text(
      data = tick_spec[tick_spec$is_cutoff, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = layout$tick_y, label = .data$label),
      color = "#4b4b4b",
      size = 4.1 / ggplot2::.pt,
      fontface = "bold"
    ) +
    ggplot2::geom_blank(data = axis_df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_blank(data = axis_df2, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::facet_wrap(~Metric, ncol = 1, scales = "free_x", strip.position = "left") +
    ggplot2::scale_fill_manual(values = c(band_fill, status_fill), guide = "none") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "Model fit indices", subtitle = "Single-fit bullet chart", x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title.position = "plot",
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 15),
      plot.margin = ggplot2::margin(18, 24, 14, 18)
    )

  p
}

plot_model_fit_single_callout_layout <- function(value_df, cutoff_df, layout) {
  out <- value_df
  range_width <- out$AxisMax - out$AxisMin
  right_x <- pmin(out$Value + range_width * layout$callout_x_offset, out$AxisMax - range_width * 0.018)
  left_x <- pmax(out$Value - range_width * layout$callout_x_offset, out$AxisMin + range_width * 0.018)
  default_side <- ifelse(out$Value > out$AxisMin + range_width * 0.72, "left", "right")

  nearest_distance <- function(metric_name, candidate_x) {
    cutoffs <- cutoff_df$cutoff[cutoff_df$Metric == metric_name]
    if (length(cutoffs) == 0L) {
      return(Inf)
    }
    min(abs(candidate_x - cutoffs))
  }

  right_near <- mapply(nearest_distance, as.character(out$Metric), right_x) <= range_width * layout$proximity_threshold
  left_near <- mapply(nearest_distance, as.character(out$Metric), left_x) <= range_width * layout$proximity_threshold
  label_side <- ifelse(
    default_side == "right" & right_near,
    "left",
    ifelse(default_side == "left" & left_near & !right_near, "right", default_side)
  )

  out$use_callout <- label_side != default_side | mapply(function(metric, value) {
    cutoffs <- cutoff_df$cutoff[cutoff_df$Metric == metric]
    if (length(cutoffs) == 0L || is.na(value)) {
      return(FALSE)
    }
    min(abs(value - cutoffs)) <= (out$AxisMax[out$Metric == metric][1] - out$AxisMin[out$Metric == metric][1]) * layout$proximity_threshold
  }, as.character(out$Metric), out$Value)
  out$label_x <- ifelse(label_side == "right", right_x, left_x)
  out$label_y <- layout$plain_label_y
  out$curve_xend <- ifelse(label_side == "right", out$label_x - range_width * layout$callout_xend_offset, out$label_x + range_width * layout$callout_xend_offset)
  out$curve_yend <- layout$curve_yend
  out$label_hjust <- ifelse(label_side == "right", 0, 1)
  out
}

plot_model_fit_threshold_dots <- function(fit_df, metric_spec) {
  metrics <- metric_spec$Metric
  model_levels <- fit_df$MODEL
  n_models <- length(model_levels)
  model_y_values <- rev(seq_len(n_models))
  names(model_y_values) <- model_levels

  long_df <- do.call(rbind, lapply(seq_len(nrow(metric_spec)), function(i) {
    row <- metric_spec[i, ]
    data.frame(
      MODEL = fit_df$MODEL,
      Metric = row$Metric,
      Value = fit_df[[row$Metric]],
      AxisMin = row$AxisMin,
      AxisMax = row$AxisMax,
      stringsAsFactors = FALSE
    )
  }))
  long_df$Metric <- factor(long_df$Metric, levels = metrics)
  long_df$MODEL_y <- unname(model_y_values[long_df$MODEL])
  long_df$ValueLabel <- ifelse(is.na(long_df$Value), "", sprintf("%.3f", long_df$Value))

  interval_df <- plot_model_fit_extract_interval_df(fit_df, metric_spec)
  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    interval_df$MODEL_y <- unname(model_y_values[interval_df$MODEL])
    interval_df$Metric <- factor(interval_df$Metric, levels = metrics)
  }

  band_spec <- plot_model_fit_single_band_spec(metrics)
  active_band_spec <- band_spec[band_spec$band != "Needs work", , drop = FALSE]
  cutoff_spec <- plot_model_fit_cutoff_spec(metrics, style = "single")
  tick_spec <- plot_model_fit_tick_spec(metrics)

  layout <- list(
    data_ymin = 0.72,
    data_ymax = n_models + 0.38,
    tick_y = 0.34,
    y_limits = c(0.20, n_models + 0.70),
    plain_label_offset = 0.18,
    callout_label_offset = 0.24,
    callout_curve_offset = 0.17,
    callout_point_offset = 0.02,
    proximity_threshold = 0.035,
    callout_x_offset = 0.05,
    callout_xend_offset = 0.010
  )

  long_df <- plot_model_fit_multi_callout_layout(long_df, cutoff_spec, layout)
  plain_df <- long_df[!long_df$use_callout & is.finite(long_df$Value), , drop = FALSE]
  callout_df <- long_df[long_df$use_callout & is.finite(long_df$Value), , drop = FALSE]
  point_df <- long_df[is.finite(long_df$Value), , drop = FALSE]

  axis_min_df <- data.frame(Metric = factor(metrics, levels = metrics), x = metric_spec$AxisMin, y = layout$tick_y)
  axis_max_df <- data.frame(Metric = factor(metrics, levels = metrics), x = metric_spec$AxisMax, y = layout$tick_y)

  point_palette <- stats::setNames(plot_model_fit_model_palette(length(model_levels)), model_levels)
  band_fill <- c(
    "Good" = "#dff0dc",
    "Almost good" = "#e7efbf",
    "Acceptable" = "#f4e7a6",
    "Near limit" = "#f3f3f3"
  )

  p <- ggplot2::ggplot(point_df, ggplot2::aes(x = .data$Value, y = .data$MODEL_y, color = .data$MODEL)) +
    ggplot2::geom_rect(
      data = active_band_spec,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = layout$data_ymin, ymax = layout$data_ymax, fill = .data$band),
      inherit.aes = FALSE,
      alpha = 0.95
    ) +
    ggplot2::geom_segment(
      data = cutoff_spec,
      ggplot2::aes(x = .data$cutoff, xend = .data$cutoff, y = layout$data_ymin, yend = layout$data_ymax),
      inherit.aes = FALSE,
      color = "#5f5f5f",
      linewidth = 0.75,
      linetype = "dashed"
    )

  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    p <- p + ggplot2::geom_segment(
      data = interval_df,
      ggplot2::aes(x = .data$CI_low, xend = .data$CI_high, y = .data$MODEL_y, yend = .data$MODEL_y, color = .data$MODEL),
      inherit.aes = FALSE,
      linewidth = 1.1,
      show.legend = FALSE
    )
  }

  p <- p +
    ggplot2::geom_curve(
      data = callout_df,
      ggplot2::aes(x = .data$Value, y = .data$MODEL_y + layout$callout_point_offset, xend = .data$curve_xend, yend = .data$curve_yend, color = .data$MODEL),
      inherit.aes = FALSE,
      curvature = 0.18,
      linewidth = 0.82,
      arrow = grid::arrow(length = grid::unit(0.045, "inches"), type = "closed"),
      show.legend = FALSE
    ) +
    ggplot2::geom_point(size = 5.1, color = "white", show.legend = FALSE) +
    ggplot2::geom_point(size = 3.8) +
    ggplot2::geom_text(
      data = plain_df,
      ggplot2::aes(x = .data$Value, y = .data$label_y, label = .data$ValueLabel, color = .data$MODEL),
      inherit.aes = FALSE,
      size = 3.2 / ggplot2::.pt,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = callout_df,
      ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$ValueLabel, hjust = .data$label_hjust, color = .data$MODEL),
      inherit.aes = FALSE,
      size = 3.2 / ggplot2::.pt,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = tick_spec[!tick_spec$is_cutoff, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = layout$tick_y, label = .data$label),
      inherit.aes = FALSE,
      color = "#636363",
      size = 3.7 / ggplot2::.pt
    ) +
    ggplot2::geom_text(
      data = tick_spec[tick_spec$is_cutoff, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = layout$tick_y, label = .data$label),
      inherit.aes = FALSE,
      color = "#4d4d4d",
      size = 3.9 / ggplot2::.pt,
      fontface = "bold"
    ) +
    ggplot2::geom_blank(data = axis_min_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE) +
    ggplot2::geom_blank(data = axis_max_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE) +
    ggplot2::scale_color_manual(values = point_palette) +
    ggplot2::scale_fill_manual(values = band_fill, guide = "none") +
    ggplot2::scale_y_continuous(
      breaks = model_y_values,
      labels = names(model_y_values),
      limits = layout$y_limits
    ) +
    ggplot2::facet_wrap(~Metric, scales = "free_x", ncol = min(2L, length(metrics))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "Model fit comparison", subtitle = "Threshold-aware dot plot", x = NULL, y = NULL, color = "Model") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot",
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  p
}

plot_model_fit_multi_callout_layout <- function(df, cutoff_df, layout) {
  out <- df
  out$use_callout <- FALSE
  out$label_x <- out$Value
  out$label_y <- out$MODEL_y + layout$plain_label_offset
  out$label_hjust <- 0.5
  out$curve_xend <- NA_real_
  out$curve_yend <- NA_real_

  for (i in seq_len(nrow(out))) {
    if (!is.finite(out$Value[i])) {
      next
    }
    metric_name <- as.character(out$Metric[i])
    cutoffs <- cutoff_df$cutoff[cutoff_df$Metric == metric_name]
    range_width <- out$AxisMax[i] - out$AxisMin[i]
    nearest_idx <- which.min(abs(out$Value[i] - cutoffs))
    nearest_cutoff <- cutoffs[nearest_idx]
    near_cutoff <- abs(out$Value[i] - nearest_cutoff) <= range_width * layout$proximity_threshold

    if (!near_cutoff) {
      next
    }

    place_left <- out$Value[i] <= nearest_cutoff
    out$use_callout[i] <- TRUE
    out$label_x[i] <- if (place_left) out$Value[i] - range_width * layout$callout_x_offset else out$Value[i] + range_width * layout$callout_x_offset
    out$label_y[i] <- out$MODEL_y[i] + layout$callout_label_offset
    out$label_hjust[i] <- if (place_left) 1 else 0
    out$curve_xend[i] <- if (place_left) out$label_x[i] + range_width * layout$callout_xend_offset else out$label_x[i] - range_width * layout$callout_xend_offset
    out$curve_yend[i] <- out$MODEL_y[i] + layout$callout_curve_offset
  }

  out
}

plot_model_fit_grouped_threshold_bars <- function(fit_df, metric_spec) {
  metrics <- metric_spec$Metric
  model_levels <- fit_df$MODEL
  model_spec <- plot_model_fit_build_model_spec(model_levels)

  metric_panel <- metric_spec[, c("Metric", "Panel", "Primary", "Direction", "ShowInterval", "IntervalLowCol", "IntervalHighCol"), drop = FALSE]
  metric_panel$metric_id <- ave(seq_len(nrow(metric_panel)), metric_panel$Panel, FUN = seq_along)
  metric_panel$ThresholdLabel <- ifelse(
    metric_panel$Direction == "higher",
    paste0(">= ", formatC(metric_panel$Primary, format = "f", digits = 2)),
    paste0("<= ", formatC(metric_panel$Primary, format = "f", digits = 2))
  )

  bar_df <- do.call(rbind, lapply(seq_len(nrow(metric_panel)), function(i) {
    row <- metric_panel[i, ]
    data.frame(
      MODEL = fit_df$MODEL,
      Metric = row$Metric,
      Panel = row$Panel,
      Threshold = row$Primary,
      ThresholdLabel = row$ThresholdLabel,
      metric_id = row$metric_id,
      ShowInterval = row$ShowInterval,
      IntervalLowCol = row$IntervalLowCol,
      IntervalHighCol = row$IntervalHighCol,
      Value = fit_df[[row$Metric]],
      stringsAsFactors = FALSE
    )
  }))
  bar_df$Panel <- factor(bar_df$Panel, levels = unique(metric_panel$Panel))
  bar_df$Metric <- factor(bar_df$Metric, levels = metrics)
  bar_df <- merge(bar_df, model_spec, by = "MODEL", all.x = TRUE, sort = FALSE)
  bar_df$MODEL <- factor(bar_df$MODEL, levels = model_levels)
  bar_df <- bar_df[order(bar_df$Panel, bar_df$metric_id, bar_df$MODEL), ]
  bar_df$x <- bar_df$metric_id + bar_df$Offset
  bar_df$xmin <- bar_df$x - 0.10
  bar_df$xmax <- bar_df$x + 0.10
  bar_df$ValueLabel <- ifelse(is.na(bar_df$Value), "", sprintf("%.3f", bar_df$Value))

  panel_levels <- levels(bar_df$Panel)
  incremental_ymin <- plot_model_fit_choose_incremental_ymin(bar_df$Value[bar_df$Panel == "Incremental fit (CFI & TLI)"])
  axis_df <- data.frame(
    Panel = factor(panel_levels, levels = panel_levels),
    ymin = ifelse(panel_levels == "Incremental fit (CFI & TLI)", incremental_ymin, 0.00),
    ymax = ifelse(panel_levels == "Incremental fit (CFI & TLI)", 1.00, 0.12),
    axis_label_y = ifelse(panel_levels == "Incremental fit (CFI & TLI)", incremental_ymin - 0.015, -0.010),
    stringsAsFactors = FALSE
  )

  bar_df <- merge(bar_df, axis_df, by = "Panel", all.x = TRUE, sort = FALSE)
  bar_df <- bar_df[order(bar_df$Panel, bar_df$metric_id, bar_df$MODEL), ]
  bar_df$label_y <- bar_df$Value + ifelse(bar_df$Panel == "Incremental fit (CFI & TLI)", 0.0080, 0.0065)

  threshold_df <- unique(bar_df[c("Panel", "Metric", "metric_id", "Threshold", "ThresholdLabel")])
  threshold_df <- threshold_df[order(threshold_df$Panel, threshold_df$metric_id), ]
  metric_counts <- stats::setNames(as.integer(tapply(metric_panel$metric_id, metric_panel$Panel, max)), unique(metric_panel$Panel))
  threshold_df$label_x <- unname(metric_counts[as.character(threshold_df$Panel)]) + 0.49
  threshold_df$label_y <- threshold_df$Threshold + ifelse(duplicated(threshold_df$Panel), -ifelse(threshold_df$Panel == "Incremental fit (CFI & TLI)", 0.0040, 0.0030), ifelse(threshold_df$Panel == "Incremental fit (CFI & TLI)", 0.0040, 0.0030))
  threshold_df$hjust <- 1
  threshold_df$vjust <- ifelse(duplicated(threshold_df$Panel), 1, 0)

  axis_label_df <- merge(metric_panel[, c("Panel", "metric_id", "Metric")], axis_df[, c("Panel", "axis_label_y")], by = "Panel", all.x = TRUE, sort = FALSE)
  names(axis_label_df)[names(axis_label_df) == "metric_id"] <- "x"
  names(axis_label_df)[names(axis_label_df) == "Metric"] <- "label"

  interval_df <- plot_model_fit_extract_interval_df(fit_df, metric_spec)
  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    interval_df <- merge(interval_df, bar_df[c("MODEL", "Metric", "Panel", "x")], by = c("MODEL", "Metric"), all.x = TRUE, sort = FALSE)
    interval_df$Panel <- factor(interval_df$Panel, levels = panel_levels)
  }

  axis_min_df <- data.frame(Panel = axis_df$Panel, x = 0.45, y = axis_df$ymin)
  axis_max_df <- data.frame(Panel = axis_df$Panel, x = unname(metric_counts[as.character(axis_df$Panel)]) + 0.52, y = axis_df$ymax)

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = bar_df[is.finite(bar_df$Value), , drop = FALSE],
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$Value, fill = .data$MODEL),
      color = "#4f4f4f",
      linewidth = 0.35
    ) +
    ggplot2::geom_hline(
      data = threshold_df,
      ggplot2::aes(yintercept = .data$Threshold),
      linewidth = 0.75,
      linetype = "dashed",
      color = "#5b5b5b"
    )

  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    p <- p + ggplot2::geom_errorbar(
      data = interval_df,
      ggplot2::aes(x = .data$x, ymin = .data$CI_low, ymax = .data$CI_high),
      inherit.aes = FALSE,
      width = 0.06,
      linewidth = 0.65,
      color = "#202020"
    )
  }

  p <- p +
    ggplot2::geom_label(
      data = bar_df[is.finite(bar_df$Value), , drop = FALSE],
      ggplot2::aes(x = .data$x, y = .data$label_y, label = .data$ValueLabel),
      color = "#202020",
      fill = "#f7f7f7",
      linewidth = 0,
      label.padding = grid::unit(0.012, "lines"),
      label.r = grid::unit(0.02, "lines"),
      size = 3.1 / ggplot2::.pt,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = threshold_df,
      ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$ThresholdLabel),
      inherit.aes = FALSE,
      color = "#4a4a4a",
      hjust = threshold_df$hjust,
      vjust = threshold_df$vjust,
      size = 3.3 / ggplot2::.pt,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      data = axis_label_df,
      ggplot2::aes(x = .data$x, y = .data$axis_label_y, label = .data$label),
      inherit.aes = FALSE,
      size = 3.5 / ggplot2::.pt
    ) +
    ggplot2::geom_blank(data = axis_min_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE) +
    ggplot2::geom_blank(data = axis_max_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE) +
    ggplot2::facet_wrap(~Panel, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(values = stats::setNames(model_spec$Fill, model_levels)) +
    ggplot2::scale_x_continuous(breaks = NULL, labels = NULL, expand = ggplot2::expansion(mult = c(0.08, 0.03))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "Model fit comparison", subtitle = "Grouped threshold bars", x = NULL, y = "Index value", fill = "Model") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "right",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  p
}

plot_model_fit_heatmap_scorecard <- function(fit_df, metric_spec) {
  metrics <- metric_spec$Metric
  model_levels <- fit_df$MODEL
  long_df <- do.call(rbind, lapply(seq_len(nrow(metric_spec)), function(i) {
    row <- metric_spec[i, ]
    values <- fit_df[[row$Metric]]
    scores <- vapply(
      values,
      plot_model_fit_compute_band_score,
      numeric(1),
      direction = row$Direction,
      poor = row$Poor,
      good = row$Good,
      ideal = row$Ideal
    )
    data.frame(
      MODEL = fit_df$MODEL,
      Metric = row$Metric,
      Score = scores,
      ValueLabel = ifelse(is.na(values), "", sprintf("%.3f", values)),
      stringsAsFactors = FALSE
    )
  }))
  long_df$MODEL <- factor(long_df$MODEL, levels = rev(model_levels))
  long_df$Metric <- factor(long_df$Metric, levels = metrics)

  plot_layout <- list(
    palette = c("#d73027", "#f0b16b", "#efe3ae", "#c9dca8", "#8fc48d"),
    palette_values = c(-1, -0.15, 0.25, 0.75, 1),
    legend_breaks = c(-0.65, 0.18, 0.97),
    legend_labels = c("Poor", "Good", "Ideal"),
    legend_title = "Fit band",
    legend_barwidth = grid::unit(6.4, "cm"),
    legend_barheight = grid::unit(0.55, "cm")
  )

  ggplot2::ggplot(long_df, ggplot2::aes(x = .data$Metric, y = .data$MODEL, fill = .data$Score)) +
    ggplot2::geom_tile(color = "#f4f4f4", linewidth = 2.2, width = 0.99, height = 0.99) +
    ggplot2::geom_text(ggplot2::aes(label = .data$ValueLabel), size = 6.2 / ggplot2::.pt, fontface = "bold", color = "#1f1f1f") +
    ggplot2::scale_fill_gradientn(
      colours = plot_layout$palette,
      values = plot_model_fit_rescale(plot_layout$palette_values),
      limits = c(-1, 1),
      breaks = plot_layout$legend_breaks,
      labels = plot_layout$legend_labels,
      name = plot_layout$legend_title,
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = plot_layout$legend_barwidth,
        barheight = plot_layout$legend_barheight,
        ticks = FALSE,
        frame.colour = "#d8d8d8"
      )
    ) +
    ggplot2::labs(title = "Model fit comparison", subtitle = "Heatmap scorecard", x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 15, face = "plain"),
      axis.text.y = ggplot2::element_text(size = 15, face = "plain"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.text = ggplot2::element_text(size = 10.5),
      legend.box.margin = ggplot2::margin(t = 6, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

