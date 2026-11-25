#' Visual theme for the Excel sheets
#'
#' Defaults reproduce the original `addStyledSheet()` look: Arial, gray bold
#' header, white / light-blue banded rows, all cells bordered.
#'
#' @param font Font name.
#' @param font_size Body font size.
#' @param header_size Header font size.
#' @param header_fill Header background colour.
#' @param band_fill Background of the shaded band.
#' @param base_fill Background of the unshaded band.
#' @param border Border style for every cell (`"thin"`, `"medium"`,
#'   `"dashed"`, ...; `"none"` for no borders).
#' @param banding `"row"` alternates every row; `"group"` alternates per row
#'   group (e.g. all levels of one variable share a colour) for tables that
#'   define groups (see [excel_table()]), falling back to rows otherwise.
#' @param min_width,max_width Column width limits (Excel character units).
#' @param max_width_first Width limit of the first (label) column.
#' @param freeze Freeze the header row.
#' @param p_format Show numeric p-value columns (named `p`, `p.value`,
#'   `pvalue`, `p_value`) as `<0.001` when below 0.001, keeping the number.
#'
#' @return A list used by [ExcelBook] and [write_styled_sheet()].
#' @export
excel_theme <- function(font = "Arial", font_size = 11, header_size = 12,
                           header_fill = "#D3D3D3", band_fill = "#e4e9f2",
                           base_fill = "#FFFFFF", border = "thin",
                           banding = c("row", "group"),
                           min_width = 8, max_width = 30, max_width_first = 45,
                           freeze = TRUE, p_format = TRUE) {
      list(font = font, font_size = font_size, header_size = header_size,
           header_fill = header_fill, band_fill = band_fill,
           base_fill = base_fill, border = border,
           banding = match.arg(banding), min_width = min_width,
           max_width = max_width, max_width_first = max_width_first,
           freeze = freeze, p_format = p_format)
}

#' Write a data frame to a styled worksheet
#'
#' Low-level writer used by [ExcelBook]. Adds the sheet, writes `df` with the
#' header and banded body styles, sizes columns to their content and sets row
#' heights so wrapped text is visible.
#'
#' @param wb An openxlsx2 workbook (`openxlsx2::wb_workbook()`).
#' @param sheet Sheet name (must not exist yet).
#' @param df A data frame.
#' @param title Optional title written above the table.
#' @param theme An [excel_theme()].
#' @param left_cols Columns aligned left (default: the `left_cols` attribute
#'   set by [excel_table()], else the first column).
#' @param groups Optional integer vector (one per row) grouping rows, used
#'   when `theme$banding == "group"` (default: the `groups` attribute).
#' @param digits Decimal places for non-integer numeric columns.
#'
#' @return `wb`, invisibly (the workbook is modified in place).
#' @export
write_styled_sheet <- function(wb, sheet, df, title = NULL,
                               theme = excel_theme(), left_cols = NULL,
                               groups = NULL, digits = 3) {
      if (is.null(left_cols)) left_cols <- attr(df, "left_cols")
      if (is.null(groups)) groups <- attr(df, "groups")
      df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
      n_row <- nrow(df)
      n_col <- ncol(df)
      if (is.null(left_cols)) left_cols <- 1
      color <- function(hex) openxlsx2::wb_color(hex = hex)
      # The wb$...() methods modify the workbook in place (the wb_*() functions
      # would return a copy). add_border() only draws the outline of a range
      # unless the inner grid is set too.
      add_borders <- function(dims) {
            if (is.null(theme$border) || theme$border == "none") return(invisible())
            wb$add_border(sheet = sheet, dims = dims,
                                     bottom_border = theme$border, top_border = theme$border,
                                     left_border = theme$border, right_border = theme$border,
                                     inner_hgrid = theme$border, inner_vgrid = theme$border)
      }

      wb$add_worksheet(sheet)

      header_row <- if (is.null(title)) 1 else 2
      if (!is.null(title)) {
            wb$add_data(sheet = sheet, x = title, dims = "A1", col_names = FALSE)
            wb$add_font(sheet = sheet, dims = "A1", name = theme$font,
                                   size = theme$header_size, bold = TRUE)
      }

      wb$add_data(sheet = sheet, x = df, start_row = header_row, na = "")

      header <- openxlsx2::wb_dims(rows = header_row, cols = seq_len(n_col))
      wb$add_font(sheet = sheet, dims = header, name = theme$font,
                             size = theme$header_size, bold = TRUE)
      wb$add_fill(sheet = sheet, dims = header, color = color(theme$header_fill))
      add_borders(header)
      wb$add_cell_style(sheet = sheet, dims = header, horizontal = "center",
                                   vertical = "center", wrap_text = TRUE)

      if (n_row > 0) {
            body_rows <- header_row + seq_len(n_row)
            body <- openxlsx2::wb_dims(rows = body_rows, cols = seq_len(n_col))

            band <- if (theme$banding == "group" && length(groups) == n_row) {
                  match(groups, unique(groups)) %% 2 == 0
            } else {
                  seq_len(n_row) %% 2 == 0
            }

            wb$add_font(sheet = sheet, dims = body, name = theme$font,
                                   size = theme$font_size)
            wb$add_fill(sheet = sheet, dims = body, color = color(theme$base_fill))
            if (any(band)) {
                  shaded <- openxlsx2::wb_dims(rows = body_rows[band], cols = seq_len(n_col))
                  wb$add_fill(sheet = sheet, dims = shaded, color = color(theme$band_fill))
            }
            add_borders(body)

            center_cols <- setdiff(seq_len(n_col), left_cols)
            for (cols in list(left_cols, center_cols)) {
                  if (length(cols) == 0) next
                  wb$add_cell_style(
                        sheet = sheet, dims = openxlsx2::wb_dims(rows = body_rows, cols = cols),
                        horizontal = if (identical(cols, left_cols)) "left" else "center",
                        vertical = "center", wrap_text = TRUE)
            }

            # Decimal places only where a numeric column actually has decimals
            num_fmt <- if (digits > 0) paste0("0.", strrep("0", digits)) else "0"
            p_fmt <- paste0('[<0.001]"<0.001";', num_fmt)
            for (j in seq_len(n_col)) {
                  v <- df[[j]]
                  fmt <- if (is.numeric(v) && isTRUE(theme$p_format) && is_p_column(names(df)[j])) {
                        p_fmt
                  } else if (is.numeric(v) && any(v != round(v), na.rm = TRUE)) {
                        num_fmt
                  }
                  if (!is.null(fmt)) {
                        wb$add_numfmt(sheet = sheet, numfmt = fmt,
                                      dims = openxlsx2::wb_dims(rows = body_rows, cols = j))
                  }
            }
      }

      widths <- column_widths(df, theme, digits)
      wb$set_col_widths(sheet = sheet, cols = seq_len(n_col), widths = widths)
      wb$set_row_heights(sheet = sheet, rows = c(header_row, header_row + seq_len(n_row)),
                                    heights = row_heights(df, widths, theme, digits))

      if (isTRUE(theme$freeze)) {
            wb$freeze_pane(sheet = sheet, first_active_row = header_row + 1)
      }
      invisible(wb)
}

is_p_column <- function(name) {
      tolower(name) %in% c("p", "p.value", "pvalue", "p_value", "p-value")
}

# Text as Excel will show it (numbers rounded to `digits`)
display_text <- function(v, digits) {
      if (is.numeric(v) && any(v != round(v), na.rm = TRUE)) {
            v <- formatC(v, format = "f", digits = digits)
      }
      v <- as.character(v)
      v[is.na(v)] <- ""
      v
}

# Width = longest cell (header counts ~20% more for the bold, bigger font), clamped
column_widths <- function(df, theme, digits) {
      vapply(seq_along(df), function(j) {
            body <- nchar(display_text(df[[j]], digits))
            longest <- max(c(0, body), nchar(names(df)[j]) * 1.2)
            limit <- if (j == 1) theme$max_width_first else theme$max_width
            min(max(longest * 1.1 + 2, theme$min_width), limit)
      }, numeric(1))
}

# Enough height for the number of wrapped lines in the tallest cell of each row
row_heights <- function(df, widths, theme, digits) {
      line_height <- theme$font_size * 1.35
      lines_needed <- function(text, width) {
            pmax(1, ceiling(nchar(text) / max(width / 1.1 - 1, 1)))
      }
      # header font is larger, so fewer characters fit per line
      header_lines <- max(mapply(lines_needed, names(df), widths / 1.2))
      body_lines <- if (nrow(df) == 0) numeric(0) else {
            per_col <- mapply(function(v, w) lines_needed(display_text(v, digits), w),
                              df, widths)
            apply(matrix(per_col, nrow = nrow(df)), 1, max)
      }
      pmax(c(header_lines * theme$header_size * 1.35, body_lines * line_height), 18)
}
