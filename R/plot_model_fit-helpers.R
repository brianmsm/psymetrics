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
    AxisMax = c(1.00, 1.00, 0.15, 0.15),
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

plot_model_fit_density_scale <- function(n_items, reference, min_scale = 0.85, max_scale = 1.15) {
  if (!is.numeric(n_items) || length(n_items) != 1L || is.na(n_items) || n_items <= 0) {
    return(1)
  }
  scale <- reference / n_items
  min(max(scale, min_scale), max_scale)
}

plot_model_fit_pt <- function(points) {
  points / ggplot2::.pt
}

plot_model_fit_halo_text_grob <- function(label, x, y, just, col, fontsize, family = "", fontface = 1, lineheight = 1.2, halo_mm = 0.30, halo_col = "#E7E7E7", halo_alpha = 0.96) {
  offsets <- list(
    c(-1, 0), c(1, 0), c(0, -1), c(0, 1),
    c(-1, -1), c(-1, 1), c(1, -1), c(1, 1)
  )
  halo_grobs <- lapply(offsets, function(offset) {
    grid::textGrob(
      label = label,
      x = x + grid::unit(offset[1] * halo_mm, "mm"),
      y = y + grid::unit(offset[2] * halo_mm, "mm"),
      just = just,
      gp = grid::gpar(
        col = grDevices::adjustcolor(halo_col, alpha.f = halo_alpha),
        fontsize = fontsize,
        fontfamily = family,
        fontface = fontface,
        lineheight = lineheight
      )
    )
  })
  main_grob <- grid::textGrob(
    label = label,
    x = x,
    y = y,
    just = just,
    gp = grid::gpar(
      col = col,
      fontsize = fontsize,
      fontfamily = family,
      fontface = fontface,
      lineheight = lineheight
    )
  )
  do.call(grid::grobTree, c(halo_grobs, list(main_grob)))
}

GeomPlotModelFitBarMarker <- ggplot2::ggproto(
  'GeomPlotModelFitBarMarker',
  ggplot2::Geom,
  required_aes = c('x', 'y', 'shape_code'),
  default_aes = ggplot2::aes(alpha = 1, placement = "inside", inside_scale = 1),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord, size = 2.8, stroke = 0.7, colour = '#202020', fill = '#202020', gap_mm = 0.9, tall_gap_mm = 0.35) {
    if (nrow(data) == 0L) {
      return(grid::nullGrob())
    }

    coords <- coord$transform(data, panel_params)
    tall_shape <- is.finite(coords$shape_code) & coords$shape_code %in% c(17, 24, 25)
    gap_vec <- gap_mm + ifelse(tall_shape, tall_gap_mm, 0)
    placement_vec <- if ("placement" %in% names(coords)) as.character(coords$placement) else rep("inside", nrow(coords))
    inside_scale <- if ("inside_scale" %in% names(coords)) coords$inside_scale else rep(1, nrow(coords))
    y_adjust_mm <- ifelse(placement_vec == "inside", -(gap_vec + size / 2) * inside_scale, 0)

    grid::pointsGrob(
      x = grid::unit(coords$x, 'npc'),
      y = grid::unit(coords$y, 'npc') + grid::unit(y_adjust_mm, 'mm'),
      pch = coords$shape_code,
      size = grid::unit(rep(size, nrow(coords)), 'mm'),
      gp = grid::gpar(
        col = colour,
        fill = fill,
        lwd = stroke * ggplot2::.stroke
      )
    )
  }
)

plot_model_fit_geom_bar_marker <- function(mapping = NULL, data = NULL, ..., size = 2.8, stroke = 0.7, colour = '#202020', fill = '#202020', gap_mm = 0.9, tall_gap_mm = 0.35, na.rm = FALSE, show.legend = FALSE, inherit.aes = FALSE) {
  ggplot2::layer(
    geom = GeomPlotModelFitBarMarker,
    mapping = mapping,
    data = data,
    stat = 'identity',
    position = 'identity',
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size = size,
      stroke = stroke,
      colour = colour,
      fill = fill,
      gap_mm = gap_mm,
      tall_gap_mm = tall_gap_mm,
      na.rm = na.rm,
      ...
    )
  )
}

GeomPlotModelFitPlainLabel <- ggplot2::ggproto(
  'GeomPlotModelFitPlainLabel',
  ggplot2::Geom,
  required_aes = c('x', 'y', 'label'),
  default_aes = ggplot2::aes(colour = '#202020', alpha = 1),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord, size = 3.8, point_size = 5.1, gap_mm = 0.8, family = '', fontface = 1, lineheight = 1.2, text_colour = '#1f1f1f') {
    if (nrow(data) == 0L) {
      return(grid::nullGrob())
    }

    coords <- coord$transform(data, panel_params)
    alpha_vec <- if ("alpha" %in% names(coords)) coords$alpha else rep(1, nrow(coords))
    col_vec <- mapply(function(col, alpha) grDevices::adjustcolor(col, alpha.f = alpha), coords$colour, alpha_vec, USE.NAMES = FALSE)
    text_col_vec <- rep(text_colour, length.out = nrow(coords))
    text_y <- grid::unit(coords$y, "npc") + grid::unit(point_size / 2 + gap_mm, "mm")
    fontsize <- size * ggplot2::.pt
    text_grobs <- lapply(seq_len(nrow(coords)), function(i) {
      plot_model_fit_halo_text_grob(
        label = coords$label[i],
        x = grid::unit(coords$x[i], "npc"),
        y = text_y[i],
        just = c("centre", "bottom"),
        col = text_col_vec[i],
        fontsize = fontsize,
        family = family,
        fontface = fontface,
        lineheight = lineheight
      )
    })
    do.call(grid::grobTree, text_grobs)
  }
)

plot_model_fit_geom_plain_label <- function(mapping = NULL, data = NULL, ..., size = 3.8, point_size = 5.1, gap_mm = 0.8, family = '', fontface = 1, lineheight = 1.2, text_colour = '#1f1f1f', na.rm = FALSE, show.legend = FALSE, inherit.aes = FALSE) {
  ggplot2::layer(
    geom = GeomPlotModelFitPlainLabel,
    mapping = mapping,
    data = data,
    stat = 'identity',
    position = 'identity',
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size = size,
      point_size = point_size,
      gap_mm = gap_mm,
      family = family,
      fontface = fontface,
      lineheight = lineheight,
      text_colour = text_colour,
      na.rm = na.rm,
      ...
    )
  )
}

GeomPlotModelFitDotCallout <- ggplot2::ggproto(
  'GeomPlotModelFitDotCallout',
  ggplot2::Geom,
  required_aes = c('x', 'y', 'label'),
  default_aes = ggplot2::aes(colour = '#202020', alpha = 1, hjust = 0),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord, size = 3.8, point_size = 5.1, label_dx_mm = 1.95, label_dy_mm = 1.45, anchor_rise_mm = 0.75, arrow_mm = 0.90, linewidth = 0.72, family = '', fontface = 1, lineheight = 1.2, text_colour = '#1f1f1f') {
    if (nrow(data) == 0L) {
      return(grid::nullGrob())
    }

    coords <- coord$transform(data, panel_params)
    alpha_vec <- if ('alpha' %in% names(coords)) coords$alpha else rep(1, nrow(coords))
    col_vec <- mapply(function(col, alpha) grDevices::adjustcolor(col, alpha.f = alpha), coords$colour, alpha_vec, USE.NAMES = FALSE)
    text_col_vec <- rep(text_colour, length.out = nrow(coords))
    hjust_vec <- if ('hjust' %in% names(data)) data$hjust else rep(0, nrow(data))
    direction <- ifelse(hjust_vec > 0.5, -1, 1)
    text_x <- grid::unit(coords$x, 'npc') + grid::unit(direction * (point_size / 2 + label_dx_mm), 'mm')
    text_y <- grid::unit(coords$y, 'npc') + grid::unit(label_dy_mm, 'mm')
    seg_x0 <- grid::unit(coords$x, 'npc') + grid::unit(direction * (point_size / 2 - 0.1), 'mm')
    seg_y0 <- grid::unit(coords$y, 'npc') + grid::unit(anchor_rise_mm, 'mm')
    seg_x1 <- text_x - grid::unit(direction * 0.55, 'mm')
    seg_y1 <- text_y - grid::unit(0.55, 'mm')
    text_grobs <- lapply(seq_len(nrow(coords)), function(i) {
      plot_model_fit_halo_text_grob(
        label = coords$label[i],
        x = text_x[i],
        y = text_y[i],
        just = c(if (direction[i] > 0) "left" else "right", "bottom"),
        col = text_col_vec[i],
        fontsize = size * ggplot2::.pt,
        family = family,
        fontface = fontface,
        lineheight = lineheight
      )
    })

    do.call(
      grid::grobTree,
      c(
        list(
          grid::segmentsGrob(
            x0 = seg_x0,
            y0 = seg_y0,
            x1 = seg_x1,
            y1 = seg_y1,
            gp = grid::gpar(col = col_vec, lwd = linewidth * ggplot2::.stroke),
            arrow = grid::arrow(length = grid::unit(arrow_mm, 'mm'), type = 'closed')
          )
        ),
        text_grobs
      )
    )
  }
)

plot_model_fit_geom_dot_callout <- function(mapping = NULL, data = NULL, ..., size = 3.8, point_size = 5.1, label_dx_mm = 1.95, label_dy_mm = 1.45, anchor_rise_mm = 0.75, arrow_mm = 0.90, linewidth = 0.72, family = '', fontface = 1, lineheight = 1.2, text_colour = '#1f1f1f', na.rm = FALSE, show.legend = FALSE, inherit.aes = FALSE) {
  ggplot2::layer(
    geom = GeomPlotModelFitDotCallout,
    mapping = mapping,
    data = data,
    stat = 'identity',
    position = 'identity',
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size = size,
      point_size = point_size,
      label_dx_mm = label_dx_mm,
      label_dy_mm = label_dy_mm,
      anchor_rise_mm = anchor_rise_mm,
      arrow_mm = arrow_mm,
      linewidth = linewidth,
      family = family,
      fontface = fontface,
      lineheight = lineheight,
      text_colour = text_colour,
      na.rm = na.rm,
      ...
    )
  )
}
plot_model_fit_size_spec <- function(style, n_metrics = 4L, n_rows = 1L) {
  style <- match.arg(style, c("bullet", "dots", "bars", "heatmap"))
  metric_scale <- plot_model_fit_density_scale(n_metrics, reference = 4, min_scale = 0.92, max_scale = 1.10)
  row_scale <- plot_model_fit_density_scale(n_rows, reference = 3, min_scale = 0.85, max_scale = 1.10)
  compact_scale <- min(metric_scale, row_scale)

  common <- list(
    metric_scale = metric_scale,
    row_scale = row_scale,
    compact_scale = compact_scale
  )

  utils::modifyList(
    common,
    switch(
      style,
      bullet = list(
        base = 15,
        title = 20,
        subtitle = 14.5,
        strip = 17,
        value_pt = 10.0 * compact_scale,
        tick_pt = 8.6 * metric_scale,
        cutoff_pt = 9.2 * metric_scale,
        tick_y = 0.72,
        band_padding = 0.02 * metric_scale,
        callout_curve_linewidth = 0.78 * compact_scale,
        callout_arrow_mm = 1.10 * compact_scale,
        callout_label_dx_mm = 2.25 * compact_scale,
        callout_label_dy_mm = 1.90 * compact_scale,
        callout_anchor_rise_mm = 0.95 * compact_scale
      ),
      dots = list(
        base = 14.5,
        title = 19,
        subtitle = 14,
        strip = 13.8,
        axis_y = 11.4,
        value_pt = 10.2 * compact_scale,
        tick_pt = 9.4 * metric_scale,
        cutoff_pt = 9.9 * metric_scale,
        legend_pt = 11.2,
        data_ymin = 0.72,
        data_ymax_pad = 0.42 * row_scale,
        y_lower = 0.18,
        y_upper_pad = 0.72 * row_scale,
        callout_curve_linewidth = 0.72 * compact_scale,
        callout_arrow_mm = 0.90 * compact_scale,
        callout_label_dx_mm = 1.95 * compact_scale,
        callout_label_dy_mm = 1.45 * compact_scale,
        callout_anchor_rise_mm = 0.75 * compact_scale,
        tick_y = 0.34
      ),
      bars = list(
        base = 14.5,
        title = 19,
        subtitle = 14,
        strip = 13.5,
        axis_y = 11.4,
        value_pt = 10.8 * compact_scale,
        threshold_pt = 9.9 * metric_scale,
        metric_pt = 10.2,
        legend_pt = 11.2,
        upper_label_offset = 0.0018 + 0.0008 * compact_scale,
        lower_label_offset = 0.0014 + 0.0006 * compact_scale,
        label_padding = 0.0055 + 0.0015 * compact_scale,
        label_radius = 0.015,
        variant_marker_size = 2.7 + 0.25 * compact_scale,
        variant_marker_stroke = 0.7,
        threshold_panel_offset = 0.003 + 0.0015 * compact_scale,
        threshold_stack_offset = 0.002 + 0.0010 * compact_scale,
        axis_panel_offset = 0.014 + 0.004 * compact_scale
      ),
      heatmap = list(
        base = 14.5,
        title = 20,
        subtitle = 14.5,
        axis_pt = 14.5 * metric_scale,
        cell_pt = 11.5 * compact_scale,
        legend_title_pt = 12.2,
        legend_text_pt = 11.2
      )
    )
  )
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

plot_model_fit_rows_by_model <- function(model_base) {
  split(seq_along(model_base), factor(model_base, levels = unique(model_base)))
}

plot_model_fit_metadata_values <- function(df, source, getter) {
  out <- rep(NA, nrow(df))
  if (nrow(df) == 0L || is.null(source)) {
    return(out)
  }

  meta <- getter(source)
  if (length(meta) == 0L) {
    return(out)
  }

  row_keys <- model_fit_metadata_row_keys(df)
  out[] <- unname(meta[row_keys])
  out
}

plot_model_fit_is_standard_row <- function(df, source = NULL) {
  out <- rep(NA, nrow(df))
  if (nrow(df) == 0L) {
    return(out)
  }

  metadata_role <- plot_model_fit_metadata_values(
    df,
    source = source,
    getter = model_fit_get_test_role
  )
  known <- !is.na(metadata_role)
  out[known] <- metadata_role[known] == "standard"

  out
}

plot_model_fit_is_primary_row <- function(df, source = NULL) {
  out <- rep(NA, nrow(df))
  if (nrow(df) == 0L) {
    return(out)
  }

  metadata_primary <- plot_model_fit_metadata_values(
    df,
    source = source,
    getter = model_fit_get_test_primary
  )
  known <- !is.na(metadata_primary)
  out[known] <- metadata_primary[known]

  out
}

plot_model_fit_validate_test_metadata <- function(df, test_mode) {
  if (nrow(df) == 0L || identical(test_mode, "all")) {
    return(invisible(NULL))
  }

  rows_by_model <- plot_model_fit_rows_by_model(df$MODEL_BASE)

  for (idx in rows_by_model) {
    model_df <- df[idx, , drop = FALSE]
    if (test_mode %in% c("non_standard", "standard_only") && any(is.na(model_df$IS_STANDARD))) {
      cli::cli_abort(c(
        "This fit object does not contain the internal test metadata required for `test_mode` filtering.",
        "Recreate it with the current `model_fit()` or `compare_model_fit()` implementation."
      ))
    }

    if (identical(test_mode, "primary") && any(is.na(model_df$IS_PRIMARY))) {
      cli::cli_abort(c(
        "This fit object does not contain the internal primary-test metadata required for `test_mode = 'primary'`.",
        "Recreate it with the current `model_fit()` or `compare_model_fit()` implementation."
      ))
    }
  }

  invisible(NULL)
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

  row_labels <- paste0("Row ", seq_len(n))
  variant[is.na(variant)] <- row_labels[is.na(variant)]

  duplicated_variant <- duplicated(variant) | duplicated(variant, fromLast = TRUE)
  if (any(duplicated_variant)) {
    dup_index <- stats::ave(seq_len(n), variant, FUN = seq_along)
    variant[duplicated_variant] <- paste0(variant[duplicated_variant], " (Row ", dup_index[duplicated_variant], ")")
  }

  variant
}

plot_model_fit_apply_test_mode <- function(df, test_mode) {
  if (nrow(df) == 0L) {
    return(df)
  }

  rows_by_model <- plot_model_fit_rows_by_model(df$MODEL_BASE)
  keep_idx <- integer(0)

  for (idx in rows_by_model) {
    model_df <- df[idx, , drop = FALSE]
    standard_idx <- which(model_df$IS_STANDARD %in% TRUE)
    non_standard_idx <- setdiff(seq_len(nrow(model_df)), standard_idx)
    primary_idx <- which(model_df$IS_PRIMARY %in% TRUE)

    selected_idx <- switch(
      test_mode,
      all = seq_len(nrow(model_df)),
      non_standard = non_standard_idx,
      standard_only = standard_idx,
      primary = if (length(primary_idx) > 0L) primary_idx[1] else integer(0)
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

  rows_by_model <- plot_model_fit_rows_by_model(df$MODEL_BASE)
  variant <- character(nrow(df))
  plot_id <- character(nrow(df))

  for (idx in rows_by_model) {
    model_df <- df[idx, , drop = FALSE]
    model_variant <- plot_model_fit_variant_labels(model_df)
    variant[idx] <- model_variant
    if (length(idx) == 1L) {
      plot_id[idx] <- model_df$MODEL_BASE[1]
    } else {
      plot_id[idx] <- paste0(model_df$MODEL_BASE, " | ", model_variant)
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
  out$IS_STANDARD <- plot_model_fit_is_standard_row(out, source = x)
  out$IS_PRIMARY <- plot_model_fit_is_primary_row(out, source = x)
  out$ROW_IN_MODEL <- stats::ave(seq_len(nrow(out)), out$MODEL_BASE, FUN = seq_along)
  plot_model_fit_validate_test_metadata(out, test_mode)
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

plot_model_fit_floor_nice <- function(x, step) {
  if (!is.finite(x) || !is.finite(step) || step <= 0) {
    return(x)
  }
  round(step * floor((x + 1e-9) / step), 8)
}

plot_model_fit_ceiling_nice <- function(x, step) {
  if (!is.finite(x) || !is.finite(step) || step <= 0) {
    return(x)
  }
  round(step * ceiling((x - 1e-9) / step), 8)
}

plot_model_fit_single_axis_step <- function(metric) {
  0.05
}

plot_model_fit_display_axis_padding <- function(axis_min, axis_max, step) {
  if (!is.finite(axis_min) || !is.finite(axis_max) || !is.finite(step) || step <= 0) {
    return(0)
  }

  span <- axis_max - axis_min
  if (!is.finite(span) || span <= 0) {
    return(0)
  }

  max(span * 0.035, step * 0.10)
}

plot_model_fit_round_midpoint <- function(x) {
  if (!is.finite(x)) {
    return(x)
  }
  round(x, 2)
}

plot_model_fit_single_tick_near_threshold <- function(axis_min, axis_max, auto_breaks, cutoff_breaks) {
  candidate_diffs <- diff(sort(unique(c(axis_min, auto_breaks, axis_max))))
  candidate_diffs <- candidate_diffs[is.finite(candidate_diffs) & candidate_diffs > 0]
  ref_gap <- if (length(candidate_diffs) > 0L) stats::median(candidate_diffs) else (axis_max - axis_min) / 4
  min(ref_gap * 0.45, (axis_max - axis_min) * 0.12)
}

plot_model_fit_auto_breaks <- function(metric, axis_min, axis_max) {
  metric <- as.character(metric)[1]
  axis_min <- suppressWarnings(min(axis_min, na.rm = TRUE))
  axis_max <- suppressWarnings(max(axis_max, na.rm = TRUE))

  if (!is.finite(axis_min) || !is.finite(axis_max) || axis_max <= axis_min) {
    return(numeric())
  }

  span <- axis_max - axis_min
  pretty_n <- if (metric %in% c("CFI", "TLI") && span > 0.22) 3 else 4
  breaks <- pretty(c(axis_min, axis_max), n = pretty_n)
  breaks <- breaks[is.finite(breaks)]
  breaks <- breaks[breaks >= axis_min - 1e-9 & breaks <= axis_max + 1e-9]
  unique(round(breaks, 10))
}

plot_model_fit_single_tick_spec_for_metric <- function(metric, axis_min, axis_max, cutoff_breaks) {
  canonical_row <- plot_model_fit_metric_spec(metric)
  canonical_min <- canonical_row$AxisMin
  canonical_max <- canonical_row$AxisMax
  expanded_high <- is.finite(axis_max) && axis_max > canonical_max + 1e-9

  auto_breaks <- plot_model_fit_auto_breaks(metric, axis_min, axis_max)
  auto_breaks <- auto_breaks[is.finite(auto_breaks)]
  auto_breaks <- auto_breaks[auto_breaks >= axis_min - 1e-9 & auto_breaks <= axis_max + 1e-9]
  auto_breaks <- auto_breaks[!(abs(auto_breaks - axis_min) < 1e-9 | abs(auto_breaks - axis_max) < 1e-9)]

  near_threshold <- plot_model_fit_single_tick_near_threshold(axis_min, axis_max, auto_breaks, cutoff_breaks)
  between_cutoffs <- vapply(auto_breaks, function(x) {
    any(x > cutoff_breaks[-length(cutoff_breaks)] & x < cutoff_breaks[-1])
  }, logical(1))
  near_cutoff <- vapply(auto_breaks, function(x) {
    any(abs(x - cutoff_breaks) <= near_threshold)
  }, logical(1))

  kept_breaks <- auto_breaks[!(between_cutoffs | near_cutoff)]
  left_interval_breaks <- kept_breaks[kept_breaks > axis_min & kept_breaks < min(cutoff_breaks)]
  allow_left_midpoint <- !(metric %in% c("RMSEA", "SRMR") && expanded_high)
  if (allow_left_midpoint && length(left_interval_breaks) == 0L && axis_min < min(cutoff_breaks)) {
    midpoint <- plot_model_fit_round_midpoint(mean(c(axis_min, min(cutoff_breaks))))
    if (midpoint > axis_min + 1e-9 && midpoint < min(cutoff_breaks) - 1e-9) {
      kept_breaks <- c(kept_breaks, midpoint)
    }
  }

  if (identical(metric, "RMSEA") && axis_min <= 0.10 && axis_max >= 0.10) {
    kept_breaks <- c(kept_breaks, 0.10)
  }

  force_axis_min_tick <- TRUE
  force_axis_max_tick <- !expanded_high

  tick_values <- c(kept_breaks, cutoff_breaks)
  if (force_axis_min_tick) {
    tick_values <- c(tick_values, axis_min)
  }
  if (force_axis_max_tick) {
    tick_values <- c(tick_values, axis_max)
  }

  sort(unique(tick_values))
}

plot_model_fit_single_axis_spec <- function(metric_spec, value_df, interval_df = NULL, upper_expand = c("nice", "data")) {
  upper_expand <- match.arg(upper_expand)
  out <- metric_spec
  out$DisplayMin <- out$AxisMin
  out$DisplayMax <- out$AxisMax
  interval_split <- NULL
  if (!is.null(interval_df) && nrow(interval_df) > 0L) {
    interval_split <- split(interval_df, as.character(interval_df$Metric))
  }

  for (i in seq_len(nrow(out))) {
    metric <- out$Metric[i]
    axis_min <- out$AxisMin[i]
    axis_max <- out$AxisMax[i]
    step <- plot_model_fit_single_axis_step(metric)
    observed <- value_df$Value[value_df$Metric == metric]
    observed <- observed[is.finite(observed)]

    if (!is.null(interval_split) && metric %in% names(interval_split)) {
      metric_interval <- interval_split[[metric]]
      observed <- c(observed, metric_interval$CI_low, metric_interval$CI_high)
    }

    observed <- observed[is.finite(observed)]
    if (length(observed) == 0L) {
      next
    }

    lower_bound <- min(axis_min, observed)
    upper_bound <- max(axis_max, observed)
    expanded_high <- upper_bound > axis_max + 1e-9
    out$AxisMin[i] <- plot_model_fit_floor_nice(lower_bound, step)
    out$AxisMax[i] <- if (identical(upper_expand, "nice")) {
      plot_model_fit_ceiling_nice(upper_bound, step)
    } else {
      round(upper_bound, 3)
    }
    out$DisplayMin[i] <- out$AxisMin[i]
    out$DisplayMax[i] <- if (identical(upper_expand, "data") && expanded_high) {
      round(out$AxisMax[i] + plot_model_fit_display_axis_padding(out$AxisMin[i], out$AxisMax[i], step), 3)
    } else {
      out$AxisMax[i]
    }
  }

  out
}

plot_model_fit_single_band_spec <- function(metrics, axis_spec = NULL, display = FALSE) {
  if (is.null(axis_spec)) {
    axis_spec <- plot_model_fit_metric_spec(metrics)
  }
  rows <- list()
  for (metric in metrics) {
    axis_row <- axis_spec[match(metric, axis_spec$Metric), , drop = FALSE]
    axis_min <- if (isTRUE(display) && "DisplayMin" %in% names(axis_row)) axis_row$DisplayMin else axis_row$AxisMin
    axis_max <- if (isTRUE(display) && "DisplayMax" %in% names(axis_row)) axis_row$DisplayMax else axis_row$AxisMax
    if (metric %in% c("CFI", "TLI")) {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        xmin = c(axis_min, 0.90, 0.95),
        xmax = c(0.90, 0.95, axis_max),
        band = c("Needs work", "Acceptable", "Good"),
        stringsAsFactors = FALSE
      )
    } else if (metric == "RMSEA") {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        xmin = c(0.00, 0.05, 0.08, 0.10),
        xmax = c(0.05, 0.08, 0.10, axis_max),
        band = c("Good", "Acceptable", "Near limit", "Needs work"),
        stringsAsFactors = FALSE
      )
    } else if (metric == "SRMR") {
      rows[[length(rows) + 1L]] <- data.frame(
        Metric = metric,
        xmin = c(0.00, 0.06, 0.08),
        xmax = c(0.06, 0.08, axis_max),
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

plot_model_fit_tick_spec <- function(metrics, axis_spec = NULL) {
  if (is.null(axis_spec)) {
    axis_spec <- plot_model_fit_metric_spec(metrics)
  }
  cutoff_df <- plot_model_fit_cutoff_spec(metrics, style = "single")
  rows <- list()
  for (metric in metrics) {
    axis_row <- axis_spec[match(metric, axis_spec$Metric), , drop = FALSE]
    axis_min <- axis_row$AxisMin
    axis_max <- axis_row$AxisMax
    cutoff_breaks <- cutoff_df$cutoff[cutoff_df$Metric == metric]
    ticks <- plot_model_fit_single_tick_spec_for_metric(metric, axis_min, axis_max, cutoff_breaks)

    rows[[length(rows) + 1L]] <- data.frame(
      Metric = metric,
      x = ticks,
      label = sprintf("%.2f", ticks),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out$Metric <- factor(out$Metric, levels = metrics)
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
  } else if (length(hit) > 1L) {
    direction <- plot_model_fit_metric_spec(metric)$Direction[1]
    hit <- if (identical(direction, "higher")) max(hit) else min(hit)
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
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(0.80)
  }

  min_value <- min(values)
  candidates <- seq(0.80, 0.00, by = -0.05)
  candidates <- candidates[candidates <= min_value + 1e-9]
  if (length(candidates) == 0L) {
    return(0.00)
  }

  visible_fraction <- (min_value - candidates) / (1 - candidates)
  keep <- candidates[visible_fraction >= 0.08]
  if (length(keep) > 0L) {
    return(max(keep))
  }

  min(candidates)
}

plot_model_fit_choose_error_ymax <- function(values, ci_high = NULL) {
  observed <- c(values, ci_high)
  observed <- observed[is.finite(observed)]
  required_max <- if (length(observed) > 0L) {
    max(c(0.08, observed))
  } else {
    0.08
  }

  candidates <- c(0.08, 0.10, 0.12, 0.15, 0.20, 0.25)
  keep <- candidates[candidates >= required_max - 1e-9]
  if (length(keep) > 0L) {
    return(min(keep))
  }

  max(0.08, plot_model_fit_ceiling_nice(required_max, 0.05))
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




plot_model_fit_group_bar_layout <- function(n_groups) {
  if (!is.numeric(n_groups) || length(n_groups) != 1L || is.na(n_groups) || n_groups <= 1L) {
    return(list(offsets = 0, half_width = 0.09))
  }

  center_span <- if (n_groups == 2L) {
    0.24
  } else if (n_groups == 3L) {
    0.36
  } else if (n_groups == 4L) {
    0.46
  } else {
    min(0.58, 0.12 * (n_groups - 1L))
  }

  offsets <- seq(-center_span / 2, center_span / 2, length.out = n_groups)
  width <- min(0.18, max(0.11, (center_span / (n_groups - 1L)) * 0.72))

  list(
    offsets = offsets,
    half_width = width / 2
  )
}

plot_model_fit_bar_expand <- function(n_metrics) {
  if (!is.numeric(n_metrics) || length(n_metrics) != 1L || is.na(n_metrics) || n_metrics <= 2L) {
    return(ggplot2::expansion(mult = c(0.025, 0.015)))
  }
  if (n_metrics == 3L) {
    return(ggplot2::expansion(mult = c(0.04, 0.025)))
  }
  ggplot2::expansion(mult = c(0.05, 0.03))
}

plot_model_fit_group_bar_breaks <- function(limits) {
  axis_min <- suppressWarnings(min(limits, na.rm = TRUE))
  axis_max <- suppressWarnings(max(limits, na.rm = TRUE))
  if (!is.finite(axis_min) || !is.finite(axis_max) || axis_max <= axis_min) {
    return(numeric())
  }

  if (abs(axis_max - 1.00) <= 1e-9) {
    return(round(seq(axis_min, axis_max, by = 0.05), 2))
  }

  step <- if (axis_max <= 0.10 + 1e-9) 0.02 else 0.05
  base_breaks <- seq(axis_min, axis_max, by = step)
  threshold_breaks <- c(0.05, 0.08)
  threshold_breaks <- threshold_breaks[threshold_breaks >= axis_min - 1e-9 & threshold_breaks <= axis_max + 1e-9]
  unique(round(sort(c(base_breaks, threshold_breaks, axis_max)), 2))
}

plot_model_fit_group_bar_limits <- function(limits) {
  axis_min <- suppressWarnings(min(limits, na.rm = TRUE))
  axis_max <- suppressWarnings(max(limits, na.rm = TRUE))
  if (!is.finite(axis_min) || !is.finite(axis_max) || axis_max <= axis_min) {
    return(limits)
  }

  if (axis_max > 0.30) {
    lower <- min(0.80, plot_model_fit_ceiling_nice(axis_min, 0.05))
    return(c(lower, 1.00))
  }

  if (axis_max <= 0.085) {
    return(c(0.00, 0.08))
  }

  c(0.00, plot_model_fit_choose_error_ymax(axis_max))
}

plot_model_fit_group_bar_labels <- function(x) {
  formatC(x, format = "f", digits = 2)
}

plot_model_fit_bar_label_y <- function(anchor, ymin, ymax, threshold, min_offset, proximity = 0.02, threshold_bonus = 0.0012) {
  panel_span <- ymax - ymin
  if (!is.finite(anchor) || !is.finite(panel_span) || panel_span <= 0) {
    return(anchor)
  }

  base_offset <- max(min_offset, panel_span * 0.004)
  candidate_y <- anchor + base_offset
  near_threshold <- is.finite(threshold) && abs(candidate_y - threshold) <= panel_span * proximity
  total_offset <- base_offset + if (near_threshold) panel_span * threshold_bonus else 0
  anchor + total_offset
}

plot_model_fit_bar_marker_placement <- function(value, ymin, ymax, min_visible_fraction = 0.08) {
  panel_span <- ymax - ymin
  visible_height <- value - ymin
  if (!is.finite(value) || !is.finite(visible_height) || !is.finite(panel_span) || panel_span <= 0 || visible_height <= 0) {
    return("above")
  }

  if (visible_height / panel_span >= min_visible_fraction) {
    return("inside")
  }

  "above"
}

plot_model_fit_bar_marker_y <- function(value, label_y, ymin, ymax, shape_code = NA_integer_, placement = c("inside", "above")) {
  placement <- match.arg(placement)
  panel_span <- ymax - ymin
  visible_height <- value - ymin
  if (!is.finite(value) || !is.finite(visible_height) || !is.finite(panel_span) || panel_span <= 0 || visible_height <= 0) {
    return(value)
  }

  if (identical(placement, "inside")) {
    return(value)
  }

  tall_shape <- is.finite(shape_code) && shape_code %in% c(17, 24, 25)
  rise_fraction <- 0.018 + if (tall_shape) 0.004 else 0
  value + panel_span * rise_fraction
}










