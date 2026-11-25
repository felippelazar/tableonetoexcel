#' Create a workbook of styled tables
#'
#' Shortcut for `ExcelBook$new(...)`. See [ExcelBook] for the methods.
#'
#' @param path File to write with `$save()` (can also be given there).
#' @param ... Passed to `ExcelBook$new()`: `labels`, `enabled`, `theme`,
#'   `show`, `digits`.
#' @return An [ExcelBook] object.
#' @export
#' @examples
#' library(survival)
#' book <- excel_book(tempfile(fileext = ".xlsx"), labels = c(ph.ecog = "ECOG"))
#'
#' head(lung) |> book$add("raw_data")                                  # data frame
#' table(lung$sex, lung$ph.ecog) |> book$add("cross", percent = "row")  # table()
#' coxph(Surv(time, status) ~ age + ph.ecog, data = lung) |> book$add("cox")
#' survfit(Surv(time, status) ~ sex, data = lung) |> book$add("median_os")
#' fisher.test(table(lung$sex, lung$status)) |> book$add("fisher")
#'
#' book$save()
excel_book <- function(path = NULL, ...) {
      ExcelBook$new(path = path, ...)
}

#' ExcelBook: add any table to a styled Excel workbook
#'
#' An R6 object holding an openxlsx2 workbook plus the options you want for
#' every sheet (labels, theme, decimals). `$add()` accepts any table-like
#' object (see [as_excel_table()] for the supported types) and writes it as a
#' formatted sheet. Methods modify the book in place: no `wb <- ...`
#' reassignment and no `if (create_excel)` wrappers (use
#' `enabled = create_excel`).
#'
#' @section Typical use:
#' ```
#' book <- excel_book("output/tables.xlsx", labels = var_labels, enabled = create_excel)
#'
#' CreateTableOne(data = df_temp, includeNA = TRUE) %>% book$add("baseline", nonnormal = "age")
#' coxph(Surv(os_time, os_status) ~ group, data = df) %>% book$add("cox_os")
#' survfit(Surv(os_time, os_status) ~ group, data = df) %>% book$add("os_median")
#' table(df$group, df$response) %>% book$add("response", percent = "row")
#' cox_results_df %>% book$add("uni_cox_os")
#'
#' book$save()
#' ```
#'
#' @export
ExcelBook <- R6::R6Class("ExcelBook",
      public = list(
            #' @field wb The underlying openxlsx2 workbook (`wbWorkbook`).
            wb = NULL,
            #' @field path Default file path used by `$save()`.
            path = NULL,
            #' @field labels Named vector: variable name -> label, applied to
            #'   every sheet (column names and cell text).
            labels = NULL,
            #' @field enabled If `FALSE`, tables are still shown in the
            #'   console but nothing is written to Excel.
            enabled = TRUE,
            #' @field theme An [excel_theme()].
            theme = NULL,
            #' @field show Print each table in the console when added.
            show = TRUE,
            #' @field digits Decimals shown for non-integer numbers.
            digits = 3,

            #' @description Start a new workbook.
            #' @param path File to write with `$save()` (can also be given there).
            #' @param labels Named vector/list: variable name -> label.
            #' @param enabled Write to Excel? Handy as `enabled = create_excel`.
            #' @param theme An [excel_theme()].
            #' @param show Print each table in the console when added.
            #' @param digits Decimals shown for non-integer numbers (the full
            #'   value is kept in the cell).
            initialize = function(path = NULL, labels = NULL, enabled = TRUE,
                                  theme = excel_theme(), show = TRUE, digits = 3) {
                  self$path <- path
                  self$labels <- labels
                  self$enabled <- enabled
                  self$theme <- theme
                  self$show <- show
                  self$digits <- digits
                  if (enabled) self$wb <- openxlsx2::wb_workbook()
            },

            #' @description Add any table as a formatted sheet.
            #' @param x A data frame, matrix, `table()`, `CreateTableOne()`
            #'   object, gtsummary table, `survfit`, model (`coxph`, `glm`,
            #'   `lm`), `htest`, ... — anything [as_excel_table()] handles.
            #' @param sheet Sheet name. Defaults to `table_1`, `table_2`, ...
            #'   Too long / duplicated names are fixed automatically.
            #' @param title Optional title written above the table.
            #' @param labels Extra labels for this sheet (merged with the
            #'   book's labels).
            #' @param digits Override the book's `digits`.
            #' @param show Override the book's `show`.
            #' @param ... Options for converting `x`, depending on its type
            #'   (e.g. `nonnormal`, `exact` for TableOne; `percent` for
            #'   `table()`; `stat`, `times` for `survfit`). See
            #'   [as_excel_table()].
            #' @return `x`, invisibly, so it can sit in the middle of a pipe:
            #'   `CreateTableOne(...) %>% book$add("baseline")`.
            add = function(x, sheet = NULL, title = NULL, labels = NULL,
                           digits = NULL, show = NULL, ...) {
                  if (is.null(sheet)) sheet <- paste0("table_", length(private$names_used) + 1)
                  df <- as_excel_table(x, ...)
                  df <- relabel_table(df, utils::modifyList(as.list(self$labels),
                                                            as.list(labels)))
                  private$write(df, sheet, title = title,
                                digits = if (is.null(digits)) self$digits else digits,
                                show = if (is.null(show)) self$show else show)
                  invisible(x)
            },

            #' @description Write the workbook to disk.
            #' @param path File path (defaults to the one given at creation).
            #'   Missing folders are created.
            #' @param open Open the file afterwards.
            save = function(path = NULL, open = FALSE) {
                  path <- if (is.null(path)) self$path else path
                  if (!self$enabled) {
                        message("ExcelBook disabled (enabled = FALSE): nothing saved.")
                        return(invisible(self))
                  }
                  if (is.null(path)) stop("No `path` given.", call. = FALSE)
                  if (length(self$sheets()) == 0) {
                        warning("The workbook has no sheets; nothing saved.",
                                call. = FALSE)
                        return(invisible(self))
                  }
                  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
                  self$wb$save(file = path, overwrite = TRUE)
                  message("Saved ", length(self$sheets()), " sheet(s) to ", path)
                  if (open) openxlsx2::xl_open(path)
                  invisible(self)
            },

            #' @description Names of the sheets written so far.
            sheets = function() {
                  if (is.null(self$wb)) return(private$names_used)
                  unname(self$wb$get_sheet_names())
            },

            #' @description Remove a sheet (e.g. to redo it).
            #' @param sheet Sheet name.
            remove = function(sheet) {
                  if (self$enabled && sheet %in% self$sheets()) {
                        self$wb$remove_worksheet(sheet)
                  }
                  private$names_used <- setdiff(private$names_used, sheet)
                  invisible(self)
            },

            #' @description Print a summary of the book.
            #' @param ... Ignored.
            print = function(...) {
                  cat("<ExcelBook>", if (!self$enabled) "(disabled)", "\n")
                  cat("  path:  ", if (is.null(self$path)) "<not set>" else self$path, "\n")
                  sheets <- self$sheets()
                  cat("  sheets:", if (length(sheets)) paste(sheets, collapse = ", ") else "<none>", "\n")
                  invisible(self)
            }
      ),

      private = list(
            names_used = character(0),

            # Excel: max 31 chars, no []:*?/\ and unique (case-insensitive)
            safe_name = function(sheet) {
                  clean <- gsub("[\\[\\]:*?/\\\\]", "_", sheet, perl = TRUE)
                  clean <- substr(clean, 1, 31)
                  existing <- tolower(private$names_used)
                  candidate <- clean
                  i <- 2
                  while (tolower(candidate) %in% existing) {
                        suffix <- paste0("_", i)
                        candidate <- paste0(substr(clean, 1, 31 - nchar(suffix)), suffix)
                        i <- i + 1
                  }
                  if (candidate != sheet) {
                        message("Sheet '", sheet, "' renamed to '", candidate, "'")
                  }
                  candidate
            },

            write = function(df, sheet, title, digits, show) {
                  sheet <- private$safe_name(sheet)
                  private$names_used <- c(private$names_used, sheet)

                  if (show) {
                        cat("\n==", sheet, "==\n")
                        print(as.data.frame(df), row.names = FALSE, right = FALSE)
                  }
                  if (self$enabled) {
                        write_styled_sheet(self$wb, sheet, df, title = title,
                                           theme = self$theme, digits = digits)
                  }
                  invisible(self)
            }
      )
)

# Labels replace variable names in column headers and in cell text. A cell
# matches when it equals the name or starts with it followed by " " or "="
# ("age (median [IQR])", "sex = F (%)", "acc_subtype=ACC-I").
relabel_table <- function(df, labels) {
      labels <- unlist(labels)
      if (length(labels) == 0) return(df)
      hits <- names(df) %in% names(labels)
      names(df)[hits] <- labels[names(df)[hits]]

      relabel <- function(x) {
            out <- x
            for (v in names(labels)) {
                  hit <- !is.na(x) & (x == v | startsWith(x, paste0(v, " ")) |
                                            startsWith(x, paste0(v, "=")))
                  out[hit] <- paste0(labels[[v]], substring(x[hit], nchar(v) + 1))
            }
            out
      }
      is_chr <- vapply(df, is.character, logical(1))
      df[is_chr] <- lapply(df[is_chr], relabel)
      df
}
