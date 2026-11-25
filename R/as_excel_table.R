#' Convert any table-like object into a data frame ready for Excel
#'
#' `as_excel_table()` is what [ExcelBook]`$add()` calls on whatever you give
#' it. Each kind of object has its own method; extra arguments to `$add()` are
#' passed here, so they depend on the type of `x`:
#'
#' | `x`                                   | result / extra arguments                                  |
#' |---------------------------------------|-----------------------------------------------------------|
#' | data.frame / tibble                   | written as is                                             |
#' | matrix                                | row names become the first column (`rownames_col`)       |
#' | `table()` / `xtabs()`                 | counts, optionally `"n (%)"` with `percent = "row"`, `"col"`, `"total"` |
#' | `CreateTableOne()` objects            | `nonnormal`, `exact`, `sort`, `na_label`, `counts`, any `print()` argument |
#' | gtsummary tables                      | the displayed table, plain-text headers                   |
#' | `survfit`                             | [tidy_survfit()]: `stat`, `times`, `endpoint`             |
#' | `coxph`, `glm`, `lm`, ...             | coefficients via broom.helpers: `exponentiate`, `full`   |
#' | `htest` (`fisher.test()`, `chisq.test()`, ...) | one row: method, statistic, p-value, estimate, CI |
#' | anything else                         | `as.data.frame(x)`                                        |
#'
#' To support a new class, write a method that returns a data frame (wrap it
#' with [excel_table()] to control alignment or row groups):
#' ```
#' as_excel_table.myclass <- function(x, ...) excel_table(my_df(x), groups = ...)
#' ```
#'
#' @param x The object to convert.
#' @param ... Method-specific options (see table above).
#' @return A data frame, possibly with the `groups` / `left_cols` attributes
#'   set by [excel_table()].
#' @export
as_excel_table <- function(x, ...) {
      UseMethod("as_excel_table")
}

#' Mark up a data frame for Excel
#'
#' Attach layout hints to a data frame, for use in [as_excel_table()] methods.
#'
#' @param df A data frame.
#' @param groups Optional integer vector, one value per row, grouping rows
#'   that belong together (e.g. the levels of one variable). Used when the
#'   theme has `banding = "group"`.
#' @param left_cols Columns (index or name) aligned left; by default the first
#'   column.
#' @return `df` with attributes set.
#' @export
excel_table <- function(df, groups = NULL, left_cols = NULL) {
      df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
      if (is.character(left_cols)) left_cols <- match(left_cols, names(df))
      attr(df, "groups") <- groups
      attr(df, "left_cols") <- left_cols
      df
}

#' @rdname as_excel_table
#' @export
as_excel_table.default <- function(x, ...) {
      df <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE),
                     error = function(e) {
                           stop("Don't know how to turn a <", class(x)[1],
                                "> into a table. Convert it to a data frame first, ",
                                "or write an as_excel_table() method for it.",
                                call. = FALSE)
                     })
      as_excel_table(df, ...)
}

#' @rdname as_excel_table
#' @export
as_excel_table.data.frame <- function(x, ...) {
      groups <- attr(x, "groups")
      left_cols <- attr(x, "left_cols")
      df <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
      rownames(df) <- NULL
      df[] <- lapply(df, function(col) {
            # list-columns (e.g. from broom) cannot be written to Excel
            if (is.list(col)) return(vapply(col, function(v) paste(v, collapse = ", "), character(1)))
            if (is.factor(col)) return(as.character(col))
            if (inherits(col, "difftime")) return(as.numeric(col))
            col
      })
      excel_table(df, groups = groups, left_cols = left_cols)
}

#' @rdname as_excel_table
#' @param rownames_col Header of the column holding the matrix row names.
#' @export
as_excel_table.matrix <- function(x, rownames_col = "", ...) {
      # A printed TableOne (print(CreateTableOne(...))) is a plain matrix
      if (looks_like_printed_tableone(x)) return(tableone_to_table(x, ...))

      df <- as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
      rn <- rownames(x)
      if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(x))))) {
            df <- data.frame(rn, df, check.names = FALSE, stringsAsFactors = FALSE)
            names(df)[1] <- rownames_col
      }
      as_excel_table(df)
}

#' @rdname as_excel_table
#' @param percent For `table()` input: `"none"` (counts only), `"row"`,
#'   `"col"` or `"total"` percentages shown as `"n (pct)"`.
#' @param pct_digits Decimals of the percentages.
#' @export
as_excel_table.table <- function(x, percent = c("none", "row", "col", "total"),
                                 pct_digits = 1, ...) {
      percent <- match.arg(percent)
      dn <- names(dimnames(x))
      label <- function(v) {
            v[is.na(v)] <- "Missing"
            v
      }

      if (length(dim(x)) == 1) {
            pct <- 100 * as.vector(x) / sum(x)
            df <- data.frame(level = label(names(x)), n = as.vector(x),
                             check.names = FALSE, stringsAsFactors = FALSE)
            if (percent != "none") df[["%"]] <- round(pct, pct_digits)
            if (!is.null(dn) && nzchar(dn[1])) names(df)[1] <- dn[1]
            return(excel_table(df))
      }
      if (length(dim(x)) != 2) {
            return(as_excel_table(as.data.frame(x, stringsAsFactors = FALSE)))
      }

      counts <- unclass(x)
      cells <- if (percent == "none") {
            counts
      } else {
            p <- switch(percent,
                        row = counts / rowSums(counts),
                        col = t(t(counts) / colSums(counts)),
                        total = counts / sum(counts))
            matrix(sprintf(paste0("%d (%.", pct_digits, "f)"), as.integer(counts), 100 * p),
                   nrow = nrow(counts))
      }
      df <- data.frame(label(rownames(counts)), cells, check.names = FALSE,
                       stringsAsFactors = FALSE)
      dn <- dn[nzchar(dn)]
      names(df) <- c(if (length(dn) > 0) paste(dn, collapse = " / ") else "",
                     label(colnames(counts)))
      excel_table(df)
}

#' @rdname as_excel_table
#' @export
as_excel_table.gtsummary <- function(x, ...) {
      if (!requireNamespace("gtsummary", quietly = TRUE)) {
            stop("Install gtsummary to export gtsummary tables.", call. = FALSE)
      }
      df <- as.data.frame(gtsummary::as_tibble(x, col_labels = TRUE),
                          check.names = FALSE)
      names(df) <- gsub("\\*\\*|__|\\*", "", names(df))
      df[] <- lapply(df, function(col) {
            col <- as.character(col)
            col[is.na(col)] <- ""
            col
      })
      excel_table(df)
}

#' @rdname as_excel_table
#' @param stat,times,endpoint For `survfit` input, see [tidy_survfit()].
#' @export
as_excel_table.survfit <- function(x, stat = c("median", "times"), times = NULL,
                                   endpoint = NULL, ...) {
      stat <- match.arg(stat)
      if (stat == "times" && is.null(times)) {
            stop("Give `times` for stat = \"times\".", call. = FALSE)
      }
      as_excel_table(tidy_survfit(x, stat = stat, times = times, endpoint = endpoint))
}

#' @rdname as_excel_table
#' @param exponentiate For models: report HR/OR. Defaults to `TRUE` for Cox
#'   and logistic/Poisson models.
#' @param full For models: keep every broom.helpers column.
#' @export
as_excel_table.coxph <- function(x, exponentiate = TRUE, full = FALSE, ...) {
      as_excel_table(tidy_model(x, exponentiate = exponentiate, full = full))
}

#' @rdname as_excel_table
#' @export
as_excel_table.glm <- function(x, exponentiate = NULL, full = FALSE, ...) {
      if (is.null(exponentiate)) {
            exponentiate <- x$family$link %in% c("logit", "log")
      }
      as_excel_table(tidy_model(x, exponentiate = exponentiate, full = full))
}

#' @rdname as_excel_table
#' @export
as_excel_table.lm <- function(x, exponentiate = FALSE, full = FALSE, ...) {
      as_excel_table(tidy_model(x, exponentiate = exponentiate, full = full))
}

#' @rdname as_excel_table
#' @export
as_excel_table.htest <- function(x, ...) {
      ci <- x$conf.int
      df <- data.frame(
            method = trimws(x$method),
            data = x$data.name,
            statistic = if (is.null(x$statistic)) NA_real_ else unname(x$statistic),
            p.value = x$p.value,
            estimate = if (is.null(x$estimate)) NA_real_ else unname(x$estimate[1]),
            conf.low = if (is.null(ci)) NA_real_ else ci[1],
            conf.high = if (is.null(ci)) NA_real_ else ci[2],
            check.names = FALSE, stringsAsFactors = FALSE
      )
      # drop columns this test does not report
      keep <- vapply(df, function(col) !all(is.na(col)), logical(1))
      excel_table(df[keep])
}
