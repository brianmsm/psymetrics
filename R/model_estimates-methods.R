#' @keywords internal
#' @noRd
prepare_table.model_estimates <- function(x, digits = 3, ci_digits = digits,
                                          p_digits = 3, ...) {
  x <- drop_converged_column(x)
  if (!"Component" %in% names(x)) {
    x$Component <- "Other"
  }
  x$Component[is.na(x$Component) | x$Component == ""] <- "Other"

  x$Link <- model_estimates_compose_link(
    to = x$To,
    operator = x$Operator,
    from = x$From
  )

  display_cols <- c(
    "Group", "Level", "Link", "Label",
    "Coefficient", "SE", "CI_low", "CI_high", "z", "p"
  )
  display_cols <- display_cols[display_cols %in% names(x)]

  component_order <- c(
    "Loading", "Regression", "Correlation", "Variance",
    "Mean", "Defined", "Threshold", "Other"
  )
  component_present <- unique(as.character(x$Component))
  component_present <- component_present[!is.na(component_present) & nzchar(component_present)]
  component_sequence <- c(
    component_order[component_order %in% component_present],
    setdiff(component_present, component_order)
  )

  blocks <- list()
  for (component_name in component_sequence) {
    block <- x[x$Component == component_name, display_cols, drop = FALSE]
    if (nrow(block) == 0L) {
      next
    }
    block <- insight::format_table(
      block,
      digits = digits,
      ci_digits = ci_digits,
      p_digits = p_digits,
      ...
    )
    attr(block, "table_caption") <- c(paste0("# ", component_name, " "), "blue")
    blocks[[component_name]] <- block
  }

  if (length(blocks) == 0L) {
    empty_block <- x[0, display_cols, drop = FALSE]
    if (nrow(empty_block) > 0L) {
      empty_block <- insight::format_table(
        empty_block,
        digits = digits,
        ci_digits = ci_digits,
        p_digits = p_digits,
        ...
      )
    } else {
      empty_block <- as.data.frame(
        lapply(empty_block, function(x) character(0)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    attr(empty_block, "table_caption") <- c("# No Parameters ", "blue")
    blocks <- list("No Parameters" = empty_block)
  }

  blocks
}

#' @export
format_results.model_estimates <- function(x,
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

#' Print Method for model_estimates Objects
#'
#' @description
#' `print.model_estimates()` prints a text summary of model
#' parameter estimates to the console in component blocks.
#'
#' @param x An object of class `model_estimates`.
#' @param digits Number of digits for rounding numeric values.
#'   Default is 3.
#' @param ci_digits Number of digits for confidence intervals.
#'   Default is `digits`.
#' @param p_digits Number of digits for p-values. Default is 3.
#' @param ... Other arguments are ignored.
#'
#' @return Returns `x` invisibly.
#' @exportS3Method base::print model_estimates
#' @seealso [format_results()]
print.model_estimates <- function(x, digits = 3, ci_digits = digits, p_digits = 3, ...) {
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

model_estimates_compose_link <- function(to, operator, from) {
  to <- as.character(to)
  operator <- as.character(operator)
  from <- as.character(from)
  n <- length(operator)
  links <- rep(NA_character_, n)

  if (n == 0L) {
    return(links)
  }

  to_trim <- trimws(to)
  operator_trim <- trimws(operator)
  from_trim <- trimws(from)

  all_na_triplet <- is.na(to) & is.na(operator) & is.na(from)
  valid_operator <- !is.na(operator_trim) & nzchar(operator_trim)
  usable_rows <- valid_operator & !all_na_triplet

  mean_rows <- usable_rows & operator_trim == "~1"
  if (any(mean_rows)) {
    links[mean_rows] <- paste(to_trim[mean_rows], "~1")
  }

  remaining_rows <- usable_rows & !mean_rows
  if (any(remaining_rows)) {
    from_has_value <- remaining_rows & !is.na(from_trim) & nzchar(from_trim)
    if (any(from_has_value)) {
      links[from_has_value] <- paste(
        to_trim[from_has_value],
        operator_trim[from_has_value],
        from_trim[from_has_value]
      )
    }
    without_from <- remaining_rows & !from_has_value
    if (any(without_from)) {
      links[without_from] <- paste(
        to_trim[without_from],
        operator_trim[without_from]
      )
    }
  }

  trimws(gsub("\\s+", " ", links))
}

model_estimates_html_payload <- function(formatted_table) {
  if (is.data.frame(formatted_table)) {
    return(list(data = formatted_table, groups = character(0)))
  }

  if (length(formatted_table) == 0L) {
    return(list(data = data.frame(), groups = character(0)))
  }

  blocks <- list()
  groups <- character(0)
  for (i in seq_along(formatted_table)) {
    block <- formatted_table[[i]]
    if (!is.data.frame(block)) {
      next
    }

    if (nrow(block) == 0L) {
      if (length(blocks) == 0L) {
        blocks[[length(blocks) + 1L]] <- block
      }
      next
    }

    block_name <- names(formatted_table)[[i]]
    caption <- model_estimates_block_caption(block, fallback = block_name, index = i)
    blocks[[length(blocks) + 1L]] <- block
    groups <- c(groups, rep(caption, nrow(block)))
  }

  if (length(blocks) == 0L) {
    return(list(data = data.frame(), groups = character(0)))
  }

  data <- do.call(rbind, blocks)
  rownames(data) <- NULL
  list(data = data, groups = groups)
}

model_estimates_block_caption <- function(block, fallback = NULL, index = 1L) {
  caption <- attr(block, "table_caption")
  if (!is.null(caption) && length(caption) > 0L) {
    label <- trimws(as.character(caption[[1]]))
    label <- gsub("^#+\\s*", "", label)
    if (nzchar(label)) {
      return(label)
    }
  }

  if (!is.null(fallback) && !is.na(fallback) && nzchar(fallback)) {
    return(as.character(fallback))
  }

  paste("Component", index)
}

model_estimates_html_empty_placeholder <- function(data) {
  if (!is.data.frame(data) || ncol(data) == 0L) {
    return(data.frame(
      Note = "No parameters matched the selected component(s).",
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  placeholder <- as.data.frame(
    matrix("", nrow = 1, ncol = ncol(data)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(placeholder) <- names(data)
  placeholder[[1]] <- "No parameters"
  placeholder
}
