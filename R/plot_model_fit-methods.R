plot_model_fit_dispatch <- function(x, type = "default", metrics = NULL, test_mode = "all", verbose = TRUE) {
  rlang::check_installed("ggplot2", reason = "to create model fit plots.")
  input_class <- plot_model_fit_validate_input(x)
  resolved_metrics <- plot_model_fit_resolve_metrics(x, metrics = metrics, verbose = verbose)
  resolved_test_mode <- plot_model_fit_resolve_test_mode(test_mode)
  fit_df <- plot_model_fit_prepare_data(x, resolved_metrics, input_class, resolved_test_mode)
  resolved_type <- plot_model_fit_resolve_type(type, input_class, nrow(fit_df))
  metric_spec <- plot_model_fit_metric_spec(resolved_metrics)

  if (identical(resolved_type, "bullet")) {
    return(plot_model_fit_single_bullet(fit_df, metric_spec))
  }
  if (identical(resolved_type, "dots")) {
    return(plot_model_fit_threshold_dots(fit_df, metric_spec))
  }
  if (identical(resolved_type, "bars")) {
    return(plot_model_fit_grouped_threshold_bars(fit_df, metric_spec))
  }
  plot_model_fit_heatmap_scorecard(fit_df, metric_spec)
}

#' @export
plot_model_fit.model_fit <- function(x, type = "default", metrics = NULL, test_mode = "all", verbose = TRUE, ...) {
  plot_model_fit_dispatch(x, type = type, metrics = metrics, test_mode = test_mode, verbose = verbose)
}

#' @export
plot_model_fit.compare_model_fit <- function(x, type = "default", metrics = NULL, test_mode = "all", verbose = TRUE, ...) {
  plot_model_fit_dispatch(x, type = type, metrics = metrics, test_mode = test_mode, verbose = verbose)
}

plot_model_fit_single_bullet <- function(fit_df, metric_spec) {
  if (nrow(fit_df) != 1L) {
    cli::cli_abort(c(
      "`type = 'bullet'` requires a single-row fit summary after applying `test_mode`.",
      "Use `type = 'dots'` to visualize multiple tests or test variants."
    ))
  }

  metrics <- metric_spec$Metric
  size_spec <- plot_model_fit_size_spec("bullet", n_metrics = length(metrics), n_rows = 1L)
  interval_df <- plot_model_fit_extract_interval_df(fit_df, metric_spec)

  value_df <- data.frame(
    Metric = factor(metrics, levels = metrics),
    Value = as.numeric(fit_df[1, metrics, drop = TRUE]),
    AxisMin = metric_spec$AxisMin,
    AxisMax = metric_spec$AxisMax,
    stringsAsFactors = FALSE
  )
  value_df$ValueLabel <- ifelse(is.na(value_df$Value), "", sprintf("%.3f", value_df$Value))

  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    idx <- match(interval_df$Metric, value_df$Metric)
    value_df$ValueLabel[idx] <- ifelse(
      is.na(value_df$Value[idx]),
      "",
      sprintf("%.3f [%.3f, %.3f]", value_df$Value[idx], interval_df$CI_low, interval_df$CI_high)
    )
  }

  metric_spec <- plot_model_fit_single_axis_spec(metric_spec, value_df, interval_df, upper_expand = "nice")
  band_spec <- plot_model_fit_single_band_spec(metrics, axis_spec = metric_spec)
  cutoff_spec <- plot_model_fit_cutoff_spec(metrics, style = "single")
  tick_spec <- plot_model_fit_tick_spec(metrics, axis_spec = metric_spec)
  value_df$AxisMin <- metric_spec$AxisMin[match(as.character(value_df$Metric), metric_spec$Metric)]
  value_df$AxisMax <- metric_spec$AxisMax[match(as.character(value_df$Metric), metric_spec$Metric)]
  value_df$Status <- vapply(
    seq_len(nrow(value_df)),
    function(i) plot_model_fit_assign_band(as.character(value_df$Metric[i]), value_df$Value[i], band_spec),
    character(1)
  )

  layout <- list(
    point_y = 1.00,
    band_ymin = 0.80,
    band_ymax = 1.20,
    tick_y = size_spec$tick_y,
    proximity_threshold = 0.035,
    callout_x_offset = 0.05
  )

  value_df <- plot_model_fit_single_callout_layout(value_df, cutoff_spec, layout)
  valid_df <- value_df[is.finite(value_df$Value), , drop = FALSE]
  plain_df <- valid_df[!valid_df$use_callout, , drop = FALSE]
  callout_df <- valid_df[valid_df$use_callout, , drop = FALSE]
  callout_df$PointY <- rep(layout$point_y, nrow(callout_df))

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
    ggplot2::geom_point(
      data = valid_df,
      ggplot2::aes(x = .data$Value, y = layout$point_y, fill = .data$Status),
      size = 6.6,
      shape = 21,
      color = "white",
      stroke = 1.2
    ) +
    plot_model_fit_geom_plain_label(
      data = plain_df,
      ggplot2::aes(x = .data$Value, y = layout$point_y, label = .data$ValueLabel),
      inherit.aes = FALSE,
      size = plot_model_fit_pt(size_spec$value_pt),
      point_size = 6.6,
      gap_mm = 1.15,
      text_colour = "#1f1f1f",
      show.legend = FALSE
    ) +
    plot_model_fit_geom_dot_callout(
      data = callout_df,
      ggplot2::aes(x = .data$Value, y = .data$PointY, label = .data$ValueLabel, hjust = .data$label_hjust),
      inherit.aes = FALSE,
      colour = "#505050",
      text_colour = "#1f1f1f",
      size = plot_model_fit_pt(size_spec$value_pt),
      point_size = 6.6,
      label_dx_mm = size_spec$callout_label_dx_mm,
      label_dy_mm = size_spec$callout_label_dy_mm,
      anchor_rise_mm = size_spec$callout_anchor_rise_mm,
      arrow_mm = size_spec$callout_arrow_mm,
      linewidth = size_spec$callout_curve_linewidth,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = tick_spec[!tick_spec$is_cutoff, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = layout$tick_y, label = .data$label),
      color = "#5e5e5e",
      size = plot_model_fit_pt(size_spec$tick_pt)
    ) +
    ggplot2::geom_text(
      data = tick_spec[tick_spec$is_cutoff, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = layout$tick_y, label = .data$label),
      color = "#4a4a4a",
      size = plot_model_fit_pt(size_spec$cutoff_pt),
      fontface = "bold"
    ) +
    ggplot2::geom_blank(data = axis_df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_blank(data = axis_df2, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::facet_wrap(~Metric, ncol = 1, scales = "free_x", strip.position = "left") +
    ggplot2::scale_fill_manual(values = c(band_fill, status_fill), guide = "none") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "Model fit indices", subtitle = "Single-fit bullet chart", x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = size_spec$base) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title.position = "plot",
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = size_spec$strip),
      plot.title = ggplot2::element_text(size = size_spec$title, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = size_spec$subtitle),
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

  point_near_cutoff <- mapply(function(metric_name, x, width) {
    cutoffs <- cutoff_df$cutoff[cutoff_df$Metric == metric_name]
    if (length(cutoffs) == 0L || !is.finite(x) || !is.finite(width)) {
      return(FALSE)
    }
    min(abs(x - cutoffs)) <= width * layout$proximity_threshold
  }, as.character(out$Metric), out$Value, range_width)

  out$use_callout <- is.finite(out$Value) & point_near_cutoff
  out$label_hjust <- ifelse(label_side == "right", 0, 1)
  out
}

plot_model_fit_threshold_dots <- function(fit_df, metric_spec) {
  metrics <- metric_spec$Metric
  model_levels <- unique(fit_df$MODEL_BASE)
  plot_levels <- unique(fit_df$PLOT_ID)
  size_spec <- plot_model_fit_size_spec("dots", n_metrics = length(metrics), n_rows = length(plot_levels))
  row_y_values <- rev(seq_along(plot_levels))
  names(row_y_values) <- plot_levels
  show_variant_shape <- any(duplicated(fit_df$MODEL_BASE))

  long_df <- do.call(rbind, lapply(seq_len(nrow(metric_spec)), function(i) {
    row <- metric_spec[i, ]
    data.frame(
      MODEL_BASE = fit_df$MODEL_BASE,
      PLOT_ID = fit_df$PLOT_ID,
      VARIANT = fit_df$VARIANT,
      Metric = row$Metric,
      Value = fit_df[[row$Metric]],
      AxisMin = row$AxisMin,
      AxisMax = row$AxisMax,
      stringsAsFactors = FALSE
    )
  }))
  long_df$Metric <- factor(long_df$Metric, levels = metrics)
  long_df$MODEL_y <- unname(row_y_values[long_df$PLOT_ID])
  long_df$ValueLabel <- ifelse(is.na(long_df$Value), "", sprintf("%.3f", long_df$Value))

  interval_df <- plot_model_fit_extract_interval_df(fit_df, metric_spec)
  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    interval_df$MODEL_y <- unname(row_y_values[interval_df$PLOT_ID])
    interval_df$Metric <- factor(interval_df$Metric, levels = metrics)
  }

  metric_spec <- plot_model_fit_single_axis_spec(metric_spec, long_df, interval_df, upper_expand = "data")
  long_df$AxisMin <- metric_spec$AxisMin[match(as.character(long_df$Metric), metric_spec$Metric)]
  long_df$AxisMax <- metric_spec$AxisMax[match(as.character(long_df$Metric), metric_spec$Metric)]
  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    interval_df$AxisMin <- metric_spec$AxisMin[match(as.character(interval_df$Metric), metric_spec$Metric)]
    interval_df$AxisMax <- metric_spec$AxisMax[match(as.character(interval_df$Metric), metric_spec$Metric)]
  }

  band_spec <- plot_model_fit_single_band_spec(metrics, axis_spec = metric_spec)
  active_band_spec <- band_spec[band_spec$band != "Needs work", , drop = FALSE]
  cutoff_spec <- plot_model_fit_cutoff_spec(metrics, style = "single")
  tick_spec <- plot_model_fit_tick_spec(metrics, axis_spec = metric_spec)

  layout <- list(
    data_ymin = size_spec$data_ymin,
    data_ymax = length(plot_levels) + size_spec$data_ymax_pad,
    tick_y = size_spec$tick_y,
    y_limits = c(size_spec$y_lower, length(plot_levels) + size_spec$y_upper_pad),
    proximity_threshold = 0.035
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
  shape_values <- stats::setNames(plot_model_fit_variant_shapes(length(unique(point_df$VARIANT))), unique(point_df$VARIANT))

  p <- ggplot2::ggplot() +
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
      ggplot2::aes(x = .data$CI_low, xend = .data$CI_high, y = .data$MODEL_y, yend = .data$MODEL_y, color = .data$MODEL_BASE),
      inherit.aes = FALSE,
      linewidth = 1.1,
      show.legend = FALSE
    )
  }

  if (show_variant_shape) {
    p <- p +
      ggplot2::geom_point(
        data = point_df,
        ggplot2::aes(x = .data$Value, y = .data$MODEL_y, shape = .data$VARIANT),
        inherit.aes = FALSE,
        size = 5.1,
        color = "white",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = point_df,
        ggplot2::aes(x = .data$Value, y = .data$MODEL_y, color = .data$MODEL_BASE, shape = .data$VARIANT),
        inherit.aes = FALSE,
        size = 3.8
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        data = point_df,
        ggplot2::aes(x = .data$Value, y = .data$MODEL_y),
        inherit.aes = FALSE,
        size = 5.1,
        color = "white",
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = point_df,
        ggplot2::aes(x = .data$Value, y = .data$MODEL_y, color = .data$MODEL_BASE),
        inherit.aes = FALSE,
        size = 3.8
      )
  }

  p <- p +
    plot_model_fit_geom_plain_label(
      data = plain_df,
      ggplot2::aes(x = .data$Value, y = .data$MODEL_y, label = .data$ValueLabel, colour = .data$MODEL_BASE),
      inherit.aes = FALSE,
      size = plot_model_fit_pt(size_spec$value_pt),
      point_size = 5.1,
      text_colour = "#1f1f1f",
      gap_mm = 0.75,
      show.legend = FALSE
    ) +
    plot_model_fit_geom_dot_callout(
      data = callout_df,
      ggplot2::aes(x = .data$Value, y = .data$MODEL_y, label = .data$ValueLabel, hjust = .data$label_hjust, colour = .data$MODEL_BASE),
      inherit.aes = FALSE,
      size = plot_model_fit_pt(size_spec$value_pt),
      point_size = 5.1,
      text_colour = "#1f1f1f",
      label_dx_mm = size_spec$callout_label_dx_mm,
      label_dy_mm = size_spec$callout_label_dy_mm,
      anchor_rise_mm = size_spec$callout_anchor_rise_mm,
      arrow_mm = size_spec$callout_arrow_mm,
      linewidth = size_spec$callout_curve_linewidth,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = tick_spec[!tick_spec$is_cutoff, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = layout$tick_y, label = .data$label),
      inherit.aes = FALSE,
      color = "#5e5e5e",
      size = plot_model_fit_pt(size_spec$tick_pt)
    ) +
    ggplot2::geom_text(
      data = tick_spec[tick_spec$is_cutoff, , drop = FALSE],
      ggplot2::aes(x = .data$x, y = layout$tick_y, label = .data$label),
      inherit.aes = FALSE,
      color = "#4a4a4a",
      size = plot_model_fit_pt(size_spec$cutoff_pt),
      fontface = "bold"
    ) +
    ggplot2::geom_blank(data = axis_min_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE) +
    ggplot2::geom_blank(data = axis_max_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE) +
    ggplot2::scale_color_manual(values = point_palette) +
    ggplot2::scale_fill_manual(values = band_fill, guide = "none") +
    ggplot2::scale_y_continuous(
      breaks = row_y_values,
      labels = names(row_y_values),
      limits = layout$y_limits
    ) +
    ggplot2::facet_wrap(~Metric, scales = "free_x", ncol = min(2L, length(metrics))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = "Model fit comparison",
      subtitle = "Threshold-aware dot plot",
      x = NULL,
      y = NULL,
      color = "Model"
    ) +
    ggplot2::theme_minimal(base_size = size_spec$base) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = grid::unit(1.2, "mm"),
      legend.box.spacing = grid::unit(1.0, "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.title.position = "plot",
      strip.text = ggplot2::element_text(face = "bold", size = size_spec$strip),
      plot.title = ggplot2::element_text(size = size_spec$title, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = size_spec$subtitle),
      axis.text.y = ggplot2::element_text(size = size_spec$axis_y, color = "#4a4a4a"),
      legend.text = ggplot2::element_text(size = size_spec$legend_pt),
      legend.title = ggplot2::element_text(size = size_spec$legend_pt + 0.5),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      shape = "none"
    )

  if (show_variant_shape) {
    p <- p + ggplot2::scale_shape_manual(values = shape_values)
  }

  p
}

plot_model_fit_multi_callout_layout <- function(df, cutoff_df, layout) {
  out <- df
  out$use_callout <- FALSE
  out$label_hjust <- 0.5

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
    out$label_hjust[i] <- if (place_left) 1 else 0
  }

  out
}

plot_model_fit_grouped_threshold_bars <- function(fit_df, metric_spec) {
  metrics <- metric_spec$Metric
  model_levels <- unique(fit_df$MODEL_BASE)
  plot_levels <- unique(fit_df$PLOT_ID)
  size_spec <- plot_model_fit_size_spec("bars", n_metrics = length(metrics), n_rows = length(plot_levels))
  model_spec <- plot_model_fit_build_model_spec(model_levels)
  group_layout <- plot_model_fit_group_bar_layout(length(plot_levels))
  plot_spec <- data.frame(PLOT_ID = plot_levels, PlotOffset = group_layout$offsets, stringsAsFactors = FALSE)
  show_variant_shape <- any(duplicated(fit_df$MODEL_BASE))
  shape_values <- NULL
  if (show_variant_shape) {
    variant_levels <- unique(fit_df$VARIANT)
    shape_values <- stats::setNames(plot_model_fit_variant_shapes(length(variant_levels)), variant_levels)
  }

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
      MODEL_BASE = fit_df$MODEL_BASE,
      PLOT_ID = fit_df$PLOT_ID,
      VARIANT = fit_df$VARIANT,
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
  panel_levels <- c("Incremental fit (CFI & TLI)", "Approximation error (RMSEA & SRMR)")
  bar_df$Panel <- factor(bar_df$Panel, levels = panel_levels)
  bar_df$Metric <- factor(bar_df$Metric, levels = metrics)
  bar_df <- merge(bar_df, model_spec, by = "MODEL_BASE", all.x = TRUE, sort = FALSE)
  bar_df <- merge(bar_df, plot_spec, by = "PLOT_ID", all.x = TRUE, sort = FALSE)
  bar_df$MODEL_BASE <- factor(bar_df$MODEL_BASE, levels = model_levels)
  bar_df <- bar_df[order(bar_df$Panel, bar_df$metric_id, bar_df$PLOT_ID), ]
  bar_df$x <- bar_df$metric_id + bar_df$PlotOffset
  bar_df$xmin <- bar_df$x - group_layout$half_width
  bar_df$xmax <- bar_df$x + group_layout$half_width
  bar_df$ValueLabel <- ifelse(is.na(bar_df$Value), "", sprintf("%.3f", bar_df$Value))

  panel_levels <- levels(bar_df$Panel)
  incremental_ymin <- plot_model_fit_choose_incremental_ymin(bar_df$Value[bar_df$Panel == "Incremental fit (CFI & TLI)"])
  axis_df <- data.frame(
    Panel = factor(panel_levels, levels = panel_levels),
    ymin = ifelse(panel_levels == "Incremental fit (CFI & TLI)", incremental_ymin, 0.00),
    ymax = ifelse(panel_levels == "Incremental fit (CFI & TLI)", 1.00, 0.12),
    axis_label_y = ifelse(panel_levels == "Incremental fit (CFI & TLI)", incremental_ymin - (size_spec$axis_panel_offset + 0.004), -size_spec$axis_panel_offset),
    stringsAsFactors = FALSE
  )

  bar_df <- merge(bar_df, axis_df, by = "Panel", all.x = TRUE, sort = FALSE)

  interval_df <- plot_model_fit_extract_interval_df(fit_df, metric_spec)
  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    bar_df <- merge(bar_df, interval_df[, c("PLOT_ID", "Metric", "CI_low", "CI_high")], by = c("PLOT_ID", "Metric"), all.x = TRUE, sort = FALSE)
  } else {
    bar_df$CI_low <- NA_real_
    bar_df$CI_high <- NA_real_
  }

  bar_df <- bar_df[order(bar_df$Panel, bar_df$metric_id, bar_df$PLOT_ID), ]
  bar_df$label_anchor <- bar_df$Value
  bar_df$label_y <- mapply(
    plot_model_fit_bar_label_y,
    anchor = bar_df$label_anchor,
    ymin = bar_df$ymin,
    ymax = bar_df$ymax,
    threshold = bar_df$Threshold,
    min_offset = ifelse(bar_df$Panel == "Incremental fit (CFI & TLI)", size_spec$upper_label_offset, size_spec$lower_label_offset)
  )
  marker_df <- NULL
  if (isTRUE(show_variant_shape)) {
    marker_df <- bar_df[is.finite(bar_df$Value), c('PLOT_ID', 'MODEL_BASE', 'VARIANT', 'Metric', 'Panel', 'x', 'Value', 'label_y', 'ymin', 'ymax', 'CI_high'), drop = FALSE]
    marker_df$shape_code <- unname(shape_values[marker_df$VARIANT])
    marker_df$marker_x <- ifelse(marker_df$Metric == 'RMSEA' & is.finite(marker_df$CI_high), marker_df$x + group_layout$half_width * 0.38, marker_df$x)
  }


  threshold_df <- unique(bar_df[c("Panel", "Metric", "metric_id", "Threshold", "ThresholdLabel")])
  metric_counts <- stats::setNames(as.integer(tapply(metric_panel$metric_id, metric_panel$Panel, max)), unique(metric_panel$Panel))
  threshold_df$label_x <- unname(metric_counts[as.character(threshold_df$Panel)]) + 0.49
  threshold_df$label_y <- threshold_df$Threshold + ifelse(
    duplicated(threshold_df$Panel),
    -ifelse(threshold_df$Panel == "Incremental fit (CFI & TLI)", size_spec$threshold_panel_offset, size_spec$threshold_stack_offset),
    ifelse(threshold_df$Panel == "Incremental fit (CFI & TLI)", size_spec$threshold_panel_offset, size_spec$threshold_stack_offset)
  )
  threshold_df$hjust <- 1
  threshold_df$vjust <- ifelse(duplicated(threshold_df$Panel), 1, 0)

  axis_label_df <- merge(metric_panel[, c("Panel", "metric_id", "Metric")], axis_df[, c("Panel", "axis_label_y")], by = "Panel", all.x = TRUE, sort = FALSE)
  names(axis_label_df)[names(axis_label_df) == "metric_id"] <- "x"
  names(axis_label_df)[names(axis_label_df) == "Metric"] <- "label"

  interval_df <- bar_df[is.finite(bar_df$CI_low) & is.finite(bar_df$CI_high), c("PLOT_ID", "Metric", "Panel", "x", "CI_low", "CI_high"), drop = FALSE]
  if (nrow(interval_df) > 0L) {
    interval_df$Panel <- factor(interval_df$Panel, levels = panel_levels)
  }

  axis_min_df <- data.frame(Panel = axis_df$Panel, x = 0.45, y = axis_df$ymin)
  axis_max_df <- data.frame(Panel = axis_df$Panel, x = unname(metric_counts[as.character(axis_df$Panel)]) + 0.52, y = axis_df$ymax)

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = bar_df[is.finite(bar_df$Value), , drop = FALSE],
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$Value, fill = .data$MODEL_BASE),
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

  if (isTRUE(show_variant_shape) && !is.null(marker_df) && nrow(marker_df) > 0L) {
    p <- p + plot_model_fit_geom_bar_marker(
      data = marker_df,
      ggplot2::aes(x = .data$marker_x, y = .data$Value, shape_code = .data$shape_code),
      inherit.aes = FALSE,
      size = size_spec$variant_marker_size,
      stroke = size_spec$variant_marker_stroke,
      colour = '#202020',
      fill = '#202020',
      show.legend = FALSE
    ) +
      ggplot2::geom_point(
        data = marker_df,
        ggplot2::aes(x = .data$marker_x, y = .data$Value, shape = .data$VARIANT),
        inherit.aes = FALSE,
        alpha = 0,
        size = size_spec$variant_marker_size,
        stroke = size_spec$variant_marker_stroke,
        color = '#202020',
        show.legend = TRUE
      )
  }

  p <- p +
    ggplot2::geom_label(
      data = bar_df[is.finite(bar_df$Value), , drop = FALSE],
      ggplot2::aes(x = .data$x, y = .data$label_y, label = .data$ValueLabel),
      color = "#202020",
      fill = "#f7f7f7",
      linewidth = 0,
      label.padding = grid::unit(size_spec$label_padding, "lines"),
      label.r = grid::unit(size_spec$label_radius, "lines"),
      size = plot_model_fit_pt(size_spec$value_pt),
      vjust = 0,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = threshold_df,
      ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$ThresholdLabel),
      inherit.aes = FALSE,
      color = "#4a4a4a",
      hjust = threshold_df$hjust,
      vjust = threshold_df$vjust,
      size = plot_model_fit_pt(size_spec$threshold_pt),
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      data = axis_label_df,
      ggplot2::aes(x = .data$x, y = .data$axis_label_y, label = .data$label),
      inherit.aes = FALSE,
      size = plot_model_fit_pt(size_spec$metric_pt)
    ) +
    ggplot2::geom_blank(data = axis_min_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE) +
    ggplot2::geom_blank(data = axis_max_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE) +
    ggplot2::facet_wrap(~Panel, ncol = 1, scales = "free_y", as.table = FALSE) +
    ggplot2::scale_fill_manual(values = stats::setNames(model_spec$Fill, model_levels)) +
    ggplot2::scale_x_continuous(breaks = NULL, labels = NULL, expand = plot_model_fit_bar_expand(max(metric_counts))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = "Model fit comparison", subtitle = "Grouped threshold bars", x = NULL, y = "Index value", fill = "Model", shape = if (show_variant_shape) "Variant" else NULL) +
    ggplot2::theme_minimal(base_size = size_spec$base) +
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = size_spec$strip),
      plot.title = ggplot2::element_text(size = size_spec$title, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = size_spec$subtitle),
      axis.text.y = ggplot2::element_text(size = size_spec$axis_y, color = "#4a4a4a"),
      legend.text = ggplot2::element_text(size = size_spec$legend_pt),
      legend.title = ggplot2::element_text(size = size_spec$legend_pt + 0.5),
      legend.position = "right",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  if (isTRUE(show_variant_shape) && !is.null(marker_df) && nrow(marker_df) > 0L) {
    p <- p + ggplot2::scale_shape_manual(values = shape_values) +
      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(alpha = 1, size = size_spec$variant_marker_size, colour = '#202020')))
  }

  p
}

plot_model_fit_heatmap_scorecard <- function(fit_df, metric_spec) {
  metrics <- metric_spec$Metric
  plot_levels <- unique(fit_df$PLOT_ID)
  size_spec <- plot_model_fit_size_spec("heatmap", n_metrics = length(metrics), n_rows = length(plot_levels))
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
      PLOT_ID = fit_df$PLOT_ID,
      Metric = row$Metric,
      Score = scores,
      ValueLabel = ifelse(is.na(values), "", sprintf("%.3f", values)),
      stringsAsFactors = FALSE
    )
  }))
  long_df$PLOT_ID <- factor(long_df$PLOT_ID, levels = rev(plot_levels))
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

  ggplot2::ggplot(long_df, ggplot2::aes(x = .data$Metric, y = .data$PLOT_ID, fill = .data$Score)) +
    ggplot2::geom_tile(color = "#f4f4f4", linewidth = 2.2, width = 0.99, height = 0.99) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$ValueLabel),
      size = plot_model_fit_pt(size_spec$cell_pt),
      fontface = "bold",
      color = "#1f1f1f"
    ) +
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
    ggplot2::theme_minimal(base_size = size_spec$base) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(size = size_spec$title, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = size_spec$subtitle),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = size_spec$axis_pt, face = "plain", color = "#4a4a4a"),
      axis.text.y = ggplot2::element_text(size = size_spec$axis_pt, face = "plain", color = "#4a4a4a"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = size_spec$legend_title_pt, hjust = 0.5, color = "#2f2f2f"),
      legend.text = ggplot2::element_text(size = size_spec$legend_text_pt, color = "#2f2f2f"),
      legend.box.margin = ggplot2::margin(t = 6, r = 0, b = 0, l = 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0)
    )
}
















