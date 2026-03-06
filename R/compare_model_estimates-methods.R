#' @keywords internal
#' @noRd
prepare_table.compare_model_estimates <- function(x, digits = 3, ci_digits = digits,
                                                  p_digits = 3, select = "ci", ...) {
  if (!"Component" %in% names(x)) {
    x$Component <- "Other"
  }
  x$Component[is.na(x$Component) | x$Component == ""] <- "Other"
  x$Link <- model_estimates_compose_link(
    to = x$To,
    operator = x$Operator,
    from = x$From
  )

  select_spec <- compare_model_estimates_normalize_select(select)
  model_names <- attr(x, "model_names")
  if (is.null(model_names) || length(model_names) == 0L) {
    model_names <- compare_model_estimates_infer_model_names(x)
  }

  identity_cols <- compare_model_estimates_identity_columns(x)
  display_name_map <- compare_model_estimates_resolve_display_names(
    model_names = model_names,
    select_spec = select_spec,
    reserved = identity_cols
  )
  component_sequence <- compare_model_estimates_component_sequence(x$Component)

  blocks <- list()
  for (component_name in component_sequence) {
    block <- x[x$Component == component_name, , drop = FALSE]
    if (nrow(block) == 0L) {
      next
    }

    display_block <- compare_model_estimates_build_display_block(
      block = block,
      model_names = model_names,
      identity_cols = identity_cols,
      display_name_map = display_name_map,
      select_spec = select_spec,
      digits = digits,
      ci_digits = ci_digits,
      p_digits = p_digits,
      ...
    )
    attr(display_block, "table_caption") <- c(paste0("# ", component_name, " "), "blue")
    blocks[[component_name]] <- display_block
  }

  if (length(blocks) == 0L) {
    empty_cols <- c(identity_cols, compare_model_estimates_default_display_cols(display_name_map))
    empty_block <- as.data.frame(
      setNames(replicate(length(empty_cols), character(0), simplify = FALSE), empty_cols),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    attr(empty_block, "table_caption") <- c("# No Parameters ", "blue")
    blocks <- list("No Parameters" = empty_block)
  }

  blocks
}

#' @export
format_results.compare_model_estimates <- function(x,
                                                   output = c("auto", "text", "markdown", "md", "html"),
                                                   digits = 3,
                                                   ci_digits = digits,
                                                   p_digits = 3,
                                                   align = "firstleft",
                                                   digits_by_col = NULL,
                                                   table_args = list(),
                                                   output_args = list()) {
  if (!is.list(table_args)) {
    cli::cli_abort("`table_args` must be a list.")
  }
  if (length(table_args) > 0 &&
      (is.null(names(table_args)) || any(names(table_args) == ""))) {
    cli::cli_abort("`table_args` must use named entries.")
  }

  if (!is.list(output_args)) {
    cli::cli_abort("`output_args` must be a list.")
  }
  if (length(output_args) > 0 &&
      (is.null(names(output_args)) || any(names(output_args) == ""))) {
    cli::cli_abort("`output_args` must use named entries.")
  }

  core_args <- c("digits", "ci_digits", "p_digits")
  passed_direct <- !missing(digits) || !missing(ci_digits) || !missing(p_digits)
  if (passed_direct && any(core_args %in% names(table_args))) {
    cli::cli_abort(
      "Do not pass `digits/ci_digits/p_digits` in both places; use only direct arguments or only `table_args`."
    )
  }

  output <- match.arg(output, c("auto", "text", "markdown", "md", "html"))
  if (output == "md") {
    output <- "markdown"
  }
  output <- .resolve_output_auto(output)

  base_table_args <- list(
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits
  )
  table_call_args <- utils::modifyList(base_table_args, table_args)
  formatted_table <- do.call(prepare_table, c(list(x = x), table_call_args))

  formatted_table <- apply_digits_by_col(formatted_table, digits_by_col = digits_by_col)

  if ("format" %in% names(output_args)) {
    cli::cli_abort("`output_args$format` is not supported. Use the top-level `output` argument.")
  }
  if ("align" %in% names(output_args)) {
    if (!missing(align)) {
      cli::cli_abort(
        "Do not pass `align` in both places; use either the top-level `align` argument or `output_args$align`."
      )
    }
    align <- output_args[["align"]]
    output_args$align <- NULL
  }

  if (output == "text") {
    return(do.call(
      insight::export_table,
      c(list(formatted_table, format = "text", align = align), output_args)
    ))
  }

  if (output == "markdown") {
    return(do.call(
      insight::export_table,
      c(list(formatted_table, format = "markdown", align = align), output_args)
    ))
  }

  html_payload <- model_estimates_html_payload(formatted_table)
  html_data <- html_payload$data
  if (nrow(html_data) == 0L) {
    html_data <- model_estimates_html_empty_placeholder(html_data)
  }

  table_html <- do.call(tinytable::tt, c(list(html_data), output_args))
  if (methods::is(table_html, "tinytable")) {
    methods::slot(table_html, "output") <- "html"
  }
  if (length(html_payload$groups) > 0L) {
    table_html <- tinytable::group_tt(table_html, i = html_payload$groups)
  }
  table_html
}

#' Print Method for compare_model_estimates Objects
#'
#' @description
#' `print.compare_model_estimates()` prints a text summary of compared model
#' estimates to the console in component blocks.
#'
#' @param x An object of class `compare_model_estimates`.
#' @param digits Number of digits for rounding numeric values. Default is 3.
#' @param ci_digits Number of digits for confidence intervals. Default is
#'   `digits`.
#' @param p_digits Number of digits for p-values. Default is 3.
#' @param ... Other arguments are ignored.
#'
#' @return Returns `x` invisibly.
#' @exportS3Method base::print compare_model_estimates
#' @seealso [format_results()]
print.compare_model_estimates <- function(x, digits = 3, ci_digits = digits, p_digits = 3, ...) {
  txt <- format_results(
    x,
    output = "text",
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits
  )
  cat(txt, sep = "\n")
  invisible(x)
}


# Helpers -----------------------------------------------------------------

compare_model_estimates_build_display_block <- function(block, model_names, identity_cols,
                                                        display_name_map, select_spec, digits, ci_digits,
                                                        p_digits, ...) {
  numeric_cols <- unlist(lapply(
    model_names,
    function(model_name) {
      paste0(c("Coefficient", "SE", "CI_low", "CI_high", "z", "p"), ".", model_name)
    }
  ))
  numeric_cols <- numeric_cols[numeric_cols %in% names(block)]
  formatted_numeric <- if (length(numeric_cols) > 0L) {
    compare_model_estimates_format_numeric_columns(
      block[, numeric_cols, drop = FALSE],
      digits = digits,
      ci_digits = ci_digits,
      p_digits = p_digits,
      ...
    )
  } else {
    block[, 0, drop = FALSE]
  }

  display_block <- as.data.frame(
    lapply(block[, identity_cols, drop = FALSE], compare_model_estimates_clean_identity_column),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (model_name in model_names) {
    parts <- compare_model_estimates_render_parts(
      block = block,
      formatted_numeric = formatted_numeric,
      model_name = model_name,
      select_spec = select_spec
    )

    for (part_idx in seq_along(parts)) {
      display_name <- display_name_map[[model_name]][[part_idx]]
      display_block[[display_name]] <- parts[[part_idx]]
    }
  }

  display_block
}

compare_model_estimates_format_numeric_columns <- function(x, digits, ci_digits, p_digits, ...) {
  out <- lapply(names(x), function(col_name) {
    temp_name <- sub('\\..*$', '', col_name)
    temp_df <- x[, col_name, drop = FALSE]
    names(temp_df) <- temp_name
    formatted_col <- insight::format_table(
      temp_df,
      digits = digits,
      ci_digits = ci_digits,
      p_digits = p_digits,
      ...
    )
    as.character(formatted_col[[1]])
  })

  out <- as.data.frame(
    setNames(out, names(x)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[is.na(out)] <- ""
  out
}

compare_model_estimates_clean_identity_column <- function(values) {
  values <- as.character(values)
  values[is.na(values)] <- ""
  values
}

compare_model_estimates_identity_columns <- function(x) {
  identity_cols <- c("Group", "Level", "Link")
  identity_cols <- identity_cols[identity_cols %in% names(x)]
  identity_cols[vapply(x[identity_cols], function(col) {
    any(!is.na(col) & nzchar(trimws(as.character(col))))
  }, logical(1))]
}

compare_model_estimates_component_sequence <- function(component) {
  component_order <- c(
    "Loading", "Regression", "Correlation", "Variance",
    "Mean", "Defined", "Threshold", "Other"
  )
  component_present <- unique(as.character(component))
  component_present <- component_present[!is.na(component_present) & nzchar(component_present)]
  c(
    component_order[component_order %in% component_present],
    setdiff(component_present, component_order)
  )
}

compare_model_estimates_default_display_cols <- function(display_name_map) {
  unlist(unname(display_name_map), use.names = FALSE)
}

compare_model_estimates_infer_model_names <- function(x) {
  suffix_pattern <- "^(Coefficient|SE|CI_low|CI_high|z|p|converged)\\."
  estimate_cols <- grep(suffix_pattern, names(x), value = TRUE)
  if (length(estimate_cols) == 0L) {
    return(character(0))
  }

  unique(sub(suffix_pattern, "", estimate_cols))
}

compare_model_estimates_normalize_select <- function(select) {
  if (is.null(select)) {
    select <- "ci"
  }
  if (!is.character(select) || length(select) != 1L || !nzchar(trimws(select))) {
    cli::cli_abort(
      "`select` must be a single string preset or a template using tokens like `{estimate}` and `{ci}`."
    )
  }

  select_key <- trimws(select)
  preset_map <- c(
    "ci" = "{estimate} ({ci})",
    "se" = "{estimate} ({se})",
    "ci_p" = "{estimate}{stars} ({ci})",
    "se_p" = "{estimate}{stars} ({se})",
    "ci_p2" = "{estimate} ({ci})|{p}",
    "se_p2" = "{estimate} ({se})|{p}"
  )
  template <- if (select_key %in% names(preset_map)) {
    unname(preset_map[[select_key]])
  } else {
    select_key
  }

  if (length(gregexpr("|", template, fixed = TRUE)[[1]]) > 1L) {
    cli::cli_abort("`select` may contain at most one `|`, which creates at most two columns per model.")
  }

  token_matches <- gregexpr("\\{[^{}]+\\}", template, perl = TRUE)[[1]]
  tokens <- if (length(token_matches) == 1L && token_matches[1] == -1L) {
    character(0)
  } else {
    regmatches(template, list(token_matches))[[1]]
  }
  token_names <- unique(gsub("[{}]", "", tokens))
  allowed_tokens <- c("estimate", "se", "ci", "ci_low", "ci_high", "p", "stars")
  unknown_tokens <- setdiff(token_names, allowed_tokens)
  if (length(unknown_tokens) > 0L) {
    cli::cli_abort(c(
      "Unknown tokens in `select`: {.field {unknown_tokens}}.",
      "Allowed tokens: estimate, se, ci, ci_low, ci_high, p, stars."
    ))
  }

  list(
    template = template,
    parts = strsplit(template, "|", fixed = TRUE)[[1]]
  )
}

compare_model_estimates_render_parts <- function(block, formatted_numeric, model_name, select_spec) {
  token_values <- compare_model_estimates_token_values(
    block = block,
    formatted_numeric = formatted_numeric,
    model_name = model_name
  )
  has_any_value <- Reduce(
    `|`,
    lapply(token_values[c("estimate", "se", "ci_low", "ci_high", "p", "stars")], nzchar)
  )

  lapply(select_spec$parts, function(part) {
    out <- character(nrow(block))
    for (row_idx in seq_len(nrow(block))) {
      if (!has_any_value[[row_idx]]) {
        out[[row_idx]] <- ""
        next
      }
      rendered <- part
      for (token_name in names(token_values)) {
        rendered <- gsub(
          paste0("{", token_name, "}"),
          token_values[[token_name]][[row_idx]],
          rendered,
          fixed = TRUE
        )
      }
      out[[row_idx]] <- compare_model_estimates_cleanup_cell(rendered)
    }
    out
  })
}

compare_model_estimates_token_values <- function(block, formatted_numeric, model_name) {
  formatted_value <- function(prefix) {
    col_name <- paste0(prefix, ".", model_name)
    if (!col_name %in% names(formatted_numeric)) {
      return(rep("", nrow(block)))
    }

    values <- trimws(as.character(formatted_numeric[[col_name]]))
    values[is.na(values)] <- ""
    values
  }

  raw_p <- if (paste0("p.", model_name) %in% names(block)) {
    suppressWarnings(as.numeric(block[[paste0("p.", model_name)]]))
  } else {
    rep(NA_real_, nrow(block))
  }

  ci_low <- formatted_value("CI_low")
  ci_high <- formatted_value("CI_high")

  list(
    estimate = formatted_value("Coefficient"),
    se = formatted_value("SE"),
    ci_low = ci_low,
    ci_high = ci_high,
    ci = compare_model_estimates_compose_ci(ci_low, ci_high),
    p = formatted_value("p"),
    stars = compare_model_estimates_significance_stars(raw_p)
  )
}

compare_model_estimates_compose_ci <- function(ci_low, ci_high) {
  out <- rep("", length(ci_low))
  has_low <- nzchar(ci_low)
  has_high <- nzchar(ci_high)

  out[has_low & has_high] <- paste(ci_low[has_low & has_high], ci_high[has_low & has_high], sep = ", ")
  out[has_low & !has_high] <- ci_low[has_low & !has_high]
  out[!has_low & has_high] <- ci_high[!has_low & has_high]
  out
}

compare_model_estimates_significance_stars <- function(p_values) {
  stars <- rep("", length(p_values))
  valid <- !is.na(p_values)
  stars[valid & p_values < 0.001] <- "***"
  stars[valid & p_values >= 0.001 & p_values < 0.01] <- "**"
  stars[valid & p_values >= 0.01 & p_values < 0.05] <- "*"
  stars
}

compare_model_estimates_cleanup_cell <- function(value) {
  value <- gsub("\\(\\s*\\)", "", value)
  value <- gsub("\\s+", " ", value)
  trimws(value)
}

compare_model_estimates_display_name <- function(model_name, part, index) {
  if (index == 1L) {
    return(model_name)
  }
  if (grepl("{p}", part, fixed = TRUE)) {
    return(sprintf("p (%s)", model_name))
  }

  sprintf("detail (%s)", model_name)
}





compare_model_estimates_resolve_display_names <- function(model_names, select_spec, reserved = character()) {
  used_names <- reserved
  resolved <- vector("list", length(model_names))
  names(resolved) <- model_names

  for (model_name in model_names) {
    part_names <- character(length(select_spec$parts))
    for (part_idx in seq_along(select_spec$parts)) {
      candidate <- compare_model_estimates_display_name(
        model_name = model_name,
        part = select_spec$parts[[part_idx]],
        index = part_idx
      )
      part_names[[part_idx]] <- compare_model_estimates_unique_display_name(
        candidate = candidate,
        used_names = used_names,
        model_name = model_name,
        index = part_idx
      )
      used_names <- c(used_names, part_names[[part_idx]])
    }
    resolved[[model_name]] <- part_names
  }

  resolved
}

compare_model_estimates_unique_display_name <- function(candidate, used_names, model_name, index) {
  if (!candidate %in% used_names) {
    return(candidate)
  }

  fallback <- if (index == 1L) {
    sprintf("estimate (%s)", model_name)
  } else {
    candidate
  }
  if (!fallback %in% used_names) {
    return(fallback)
  }

  make.unique(c(used_names, fallback), sep = "_")[[length(used_names) + 1L]]
}
