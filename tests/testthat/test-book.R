test_that("ExcelBook writes any table and fixes sheet names", {
      path <- tempfile(fileext = ".xlsx")
      book <- excel_book(path, show = FALSE)
      book$add(data.frame(term = "x", estimate = 1.23456, p.value = 0.00001), "cox")
      piped <- table(c("a", "b", "b")) |> book$add()
      expect_s3_class(piped, "table")   # $add() returns its input for piping
      book$add(data.frame(x = 1), "a name that is clearly way too long for excel")
      expect_message(book$add(data.frame(x = 1), "cox"), "renamed")
      book$save()

      sheets <- unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path)))
      expect_equal(sheets[1:2], c("cox", "table_2"))
      expect_true(all(nchar(sheets) <= 31))
      expect_true("cox_2" %in% sheets)
      expect_equal(openxlsx2::read_xlsx(path, sheet = "cox")$estimate, 1.23456)
})

test_that("labels rename headers and cell text", {
      df <- data.frame(variable = c("age", "sex"), strata = c("sex=F", "sex = M (%)"), age = 1:2)
      out <- relabel_table(df, list(age = "Age", sex = "Sex"))
      expect_equal(names(out)[3], "Age")
      expect_equal(out$variable, c("Age", "Sex"))
      expect_equal(out$strata, c("Sex=F", "Sex = M (%)"))
})

test_that("disabled books do not write", {
      path <- tempfile(fileext = ".xlsx")
      book <- excel_book(path, enabled = FALSE, show = FALSE)
      book$add(data.frame(x = 1:3))
      expect_message(book$save(), "disabled")
      expect_false(file.exists(path))
})
