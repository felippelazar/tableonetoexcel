# Columns of a printed TableOne that describe the whole variable, not a level
.block_cols <- c("Variable", "p", "test", "SMD")
.non_count_cols <- c("Variable", "level", "p", "test", "SMD")

#' @rdname as_excel_table
#' @param nonnormal For `CreateTableOne()` objects: variables shown as
#'   median \[IQR\]; `TRUE` = all continuous variables.
#' @param sort For `CreateTableOne()` objects: `"frequency"` sorts the levels
#'   of each categorical variable by count (missing level last); `"none"`
#'   keeps the original order.
#' @param na_label For `CreateTableOne()` objects: text of the missing level.
#' @param counts For `CreateTableOne()` objects: append numeric `n_<column>`
#'   columns with the absolute counts.
#' @export
as_excel_table.TableOne <- function(x, nonnormal = NULL,
                                    sort = c("frequency", "none"),
                                    na_label = "Missing", counts = FALSE, ...) {
      tableone_to_table(x, nonnormal = nonnormal, sort = sort,
                        na_label = na_label, counts = counts, ...)
}

#' @rdname as_excel_table
#' @export
as_excel_table.CatTable <- as_excel_table.TableOne

#' @rdname as_excel_table
#' @export
as_excel_table.ContTable <- as_excel_table.TableOne

#' @rdname as_excel_table
#' @export
as_excel_table.svyTableOne <- as_excel_table.TableOne

# TableOne object or printed matrix -> data frame with one row group per variable
tableone_to_table <- function(x, sort = c("frequency", "none"),
                              na_label = "Missing", counts = FALSE,
                              nonnormal = NULL, ...) {
      sort <- match.arg(sort)
      m <- print_tableone(x, nonnormal = nonnormal, ...)

      rn <- rownames(m)
      if (is.null(rn)) rn <- rep("", nrow(m))
      df <- data.frame(Variable = rn, unclass(m), check.names = FALSE,
                       stringsAsFactors = FALSE)
      rownames(df) <- NULL
      df[] <- lapply(df, function(col) as.character(col))

      # Missing level: NA in the "level" column, or "   NA" rows without it
      has_level <- "level" %in% names(df)
      if (has_level) {
            df$level[is.na(df$level)] <- na_label
            names(df)[names(df) == "level"] <- "Level"
      } else {
            df$Variable <- sub("^(\\s+)NA$", paste0("\\1", na_label), df$Variable)
      }
      df[is.na(df)] <- ""

      # A variable block starts at a non-empty, non-indented row name
      is_start <- nzchar(trimws(df$Variable)) & !grepl("^\\s", df$Variable)
      block <- cumsum(is_start)

      if (sort == "frequency") {
            df <- sort_levels(df, block, has_level, na_label)
      }

      if (counts) {
            for (col in count_cols(df)) {
                  df[[paste0("n_", col)]] <- first_count(df[[col]])
            }
      }

      excel_table(df, groups = block,
                  left_cols = which(names(df) %in% c("Variable", "Level")))
}

# print(CreateTableOne(...)) output: first row "n" and tableone column names
looks_like_printed_tableone <- function(x) {
      rn <- rownames(x)
      !is.null(rn) && length(rn) > 0 && rn[1] == "n" &&
            any(colnames(x) %in% c("level", "Overall", "p", "test", "SMD"))
}

# Calls print() on the TableOne with the usual defaults unless `x` is already printed
print_tableone <- function(x, nonnormal = NULL, ...) {
      if (is.matrix(x)) return(x)

      tableone_classes <- c("TableOne", "svyTableOne", "CatTable", "ContTable",
                            "svyCatTable", "svyContTable")
      if (!inherits(x, tableone_classes)) {
            stop("`x` must be a TableOne object or a printed TableOne matrix.",
                 call. = FALSE)
      }

      if (isTRUE(nonnormal)) {
            nonnormal <- x$MetaData$varNumerics
            if (is.null(nonnormal)) nonnormal <- character(0)
      }

      args <- utils::modifyList(
            list(showAllLevels = TRUE, noSpaces = TRUE, quote = FALSE,
                 printToggle = FALSE),
            list(...)
      )
      if (!is.null(nonnormal)) args$nonnormal <- nonnormal
      do.call(print, c(list(x), args))
}

# Columns that hold "n (%)" style counts (prefer Overall to avoid double counting)
count_cols <- function(df) {
      cols <- setdiff(names(df), c(.non_count_cols, "Level"))
      cols <- cols[!startsWith(cols, "n_")]
      cols
}

# First integer of an "n (pct)" cell, NA for anything else (e.g. median [IQR])
first_count <- function(x) {
      out <- rep(NA_real_, length(x))
      hit <- grepl("^\\s*[0-9]+\\s*\\(", x) | grepl("^\\s*[0-9]+\\s*$", x)
      out[hit] <- as.numeric(sub("^\\s*([0-9]+).*$", "\\1", x[hit]))
      out
}

sort_levels <- function(df, block, has_level, na_label) {
      cols <- count_cols(df)
      score_cols <- if ("Overall" %in% cols) "Overall" else cols
      if (length(score_cols) == 0) return(df)

      score <- rowSums(vapply(score_cols, function(col) first_count(df[[col]]),
                              numeric(nrow(df))), na.rm = TRUE)

      for (b in unique(block[block > 0])) {
            rows <- which(block == b)

            if (has_level) {
                  # Level rows share the block; variable name / p stay on top
                  movable <- rows
                  is_missing <- df$Level[rows] == na_label
                  move_cols <- setdiff(names(df), .block_cols)
            } else {
                  # First row is the header ("var (%)"), levels are indented
                  movable <- rows[-1]
                  is_missing <- trimws(df$Variable[movable]) == na_label
                  move_cols <- names(df)
            }
            if (length(movable) < 2) next

            s <- score[movable]
            s[is_missing] <- -Inf
            ord <- movable[order(-s)]
            df[movable, move_cols] <- df[ord, move_cols]
      }
      df
}
