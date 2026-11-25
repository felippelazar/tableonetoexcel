# tableonetoexcel

Add **any** table to a formatted Excel workbook with one line, usually at the
end of a pipe:

```r
book <- excel_book("output/tables.xlsx", labels = var_labels, enabled = create_excel)

CreateTableOne(data = df_temp, includeNA = TRUE) %>% book$add("baseline", nonnormal = "age")
coxph(Surv(os_time, os_status) ~ subtype, data = df) %>% book$add("cox_os")
df_cox_results %>% book$add("uni_cox_os")

book$save()
```

Every sheet gets the same look: Arial, gray bold header, white/light-blue
banded rows, borders, column widths fitted to the content, wrapped text,
frozen header, numbers kept at full precision but shown with 3 decimals,
p-values shown as `<0.001`.

## Install

```r
remotes::install_github("<your-user>/tableonetoexcel")   # after pushing to GitHub
devtools::install("path/to/tableonetoexcel")             # or from the local folder
```

## What `$add()` accepts

`$add(x, sheet, title = NULL, labels = NULL, digits = NULL, show = NULL, ...)`
turns `x` into a table with `as_excel_table()`. The `...` options depend on
what `x` is:

| `x`                                          | what you get                       | options (`...`)                             |
|----------------------------------------------|------------------------------------|---------------------------------------------|
| data frame / tibble                          | as is                              |                                             |
| matrix                                       | row names as first column          | `rownames_col`                              |
| `table()` / `xtabs()`                        | counts or `n (%)`                  | `percent = "row"/"col"/"total"`, `pct_digits` |
| `CreateTableOne()` (or its `print()` matrix) | levels sorted by frequency, NA -> "Missing" | `nonnormal` (`TRUE` = all), `exact`, `smd`, `sort = "none"`, `counts`, any `print()` arg |
| gtsummary (`tbl_summary`, `tbl_regression`, `tbl_cross`, ...) | displayed table, plain headers |                                   |
| `survfit`                                    | median (95% CI) per stratum        | `stat = "times"`, `times`, `endpoint`        |
| `coxph`, `glm`, `lm`                         | HR/OR (95% CI), p, n, events, Ref. rows | `exponentiate`, `full = TRUE`          |
| `htest` (`fisher.test`, `chisq.test`, `wilcox.test`, ...) | one row: method, statistic, p, estimate, CI |                  |
| anything with `as.data.frame()`              | `as.data.frame(x)`                 |                                             |

`$add()` returns `x` invisibly, so it can also sit in the middle of a pipe.
Each table is printed in the console when added (`show = FALSE` to silence).

New type? Write a method:

```r
as_excel_table.myclass <- function(x, ...) excel_table(my_conversion(x))
```

## Book options

```r
book <- excel_book(
      "output/tables.xlsx",
      labels  = c(age_diagnosis = "Age at diagnosis", acc_subtype = "ACC subtype"),
      enabled = create_excel,   # FALSE: tables still print, nothing is written
      digits  = 3,              # decimals shown for non-integer numbers
      theme   = excel_theme(banding = "group", band_fill = "#e4e9f2")
)
```

- **labels** rename variable names everywhere: column headers and cell text
  (`age_diagnosis (median [IQR])`, `sex = F (%)`, `acc_subtype=ACC-I`).
- Sheet names that are too long or already used are fixed automatically
  (`baseline` -> `baseline_2`) with a message.
- `excel_theme(banding = "group")` shades by row group (e.g. all levels of a
  variable in a descriptive table) instead of every other row.

## Helpers that build tables

```r
# One model per variable, stacked (replaces tidy_cox / tidy_glm + lapply + bind_rows)
tidy_models(df, "Surv(os_time, os_status)", c("age", "sex", "subtype")) %>% book$add("uni_cox_os")
tidy_models(df, "Surv(os_time, os_status)", "subtype", adjust = "age") %>% book$add("adj_cox_os")
tidy_models(df, "response_bin", vars, model = "logistic") %>% book$add("uni_glm_orr")

# Median / landmark survival (replaces tidy_mstrata_survfit), stack several with bind_rows()
bind_rows(
      survfit(Surv(os_time, os_status) ~ 1, data = df) %>% tidy_survfit(endpoint = "OS All"),
      survfit(Surv(os_time, os_status) ~ subtype, data = df) %>% tidy_survfit(endpoint = "OS by subtype")
) %>% book$add("os_median")
```

## Old -> new

| old (`aux_functions.R`)                                        | new                                           |
|----------------------------------------------------------------|-----------------------------------------------|
| `if(create_excel){wb <- createWorkbook()}`                     | `book <- excel_book(path, enabled = create_excel)` |
| `temp_table <- print(CreateTableOne(...), showAllLevels = T, noSpaces = TRUE, quote = FALSE)` + `if(create_excel){wb <- addStyledSheet(wb, "x", temp_table)}` | `CreateTableOne(...) %>% book$add("x")` |
| sort by `abs_n` / `col_n` in Excel                             | levels already sorted; `counts = TRUE` for numeric counts |
| `addStyledSheet(wb, "x", df, print_table = FALSE)`             | `df %>% book$add("x")`                        |
| `new_label = c(...)`                                           | `labels = c(...)` once, on the book           |
| `do.call(bind_rows, lapply(vars, tidy_cox, "Surv(...)", df))`  | `tidy_models(df, "Surv(...)", vars)`          |
| `tidy_mstrata_survfit(fit)` / `glance()` for `~ 1` fits        | `tidy_survfit(fit)` or `fit %>% book$add()`   |
| `if(create_excel){saveWorkbook(wb, path, overwrite = TRUE)}`   | `book$save()`                                 |
