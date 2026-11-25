lung2 <- survival::lung
lung2$sex <- factor(lung2$sex, labels = c("Male", "Female"))

test_that("data frames are kept as is (factors -> text, difftime -> number)", {
      d <- data.frame(a = factor(c("x", "y")), b = as.difftime(c(1, 2), units = "days"))
      out <- as_excel_table(d)
      expect_type(out$a, "character")
      expect_type(out$b, "double")
})

test_that("matrix row names become the first column", {
      m <- matrix(1:4, 2, dimnames = list(c("r1", "r2"), c("A", "B")))
      out <- as_excel_table(m, rownames_col = "row")
      expect_equal(names(out), c("row", "A", "B"))
})

test_that("table() gives counts or n (%)", {
      tab <- table(group = c("a", "a", "b"), resp = c("y", "n", "y"))
      expect_equal(as_excel_table(tab)$y, c(1, 1))
      expect_equal(as_excel_table(tab, percent = "row")$y, c("1 (50.0)", "1 (100.0)"))
      expect_equal(names(as_excel_table(table(c("a", "b"), c("y", "n"))))[1], "")
      one <- as_excel_table(table(c("a", "a", "b")), percent = "total")
      expect_equal(one$n, c(2, 1))
})

test_that("TableOne: levels sorted by frequency, Missing last, rows grouped", {
      skip_if_not_installed("tableone")
      d <- data.frame(age = c(50, 60, 70, 80, 55, 65),
                      site = c("B", "A", "A", "C", "A", NA))
      t1 <- tableone::CreateTableOne(data = d, includeNA = TRUE)
      out <- as_excel_table(t1, nonnormal = TRUE)
      site_rows <- which(attr(out, "groups") == attr(out, "groups")[startsWith(out$Variable, "site")])
      expect_equal(out$Level[site_rows], c("A", "B", "C", "Missing"))
      expect_true("age (median [IQR])" %in% out$Variable)
      expect_equal(as_excel_table(t1, sort = "none")$Level[site_rows], c("A", "B", "C", "Missing"))
      # the printed matrix (old workflow) gives the same table
      m <- print(t1, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
      expect_equal(as_excel_table(m)$Level, as_excel_table(t1)$Level)
})

test_that("TableOne: stratified keeps p-value on the variable row; counts", {
      skip_if_not_installed("tableone")
      d <- data.frame(site = c("B", "A", "A", "C", "A", "B"), grp = c("x", "x", "y", "y", "x", "y"))
      t1 <- tableone::CreateTableOne(vars = "site", strata = "grp", data = d)
      out <- as_excel_table(t1, exact = "site", counts = TRUE)
      expect_true(nzchar(out$p[startsWith(out$Variable, "site")]))
      expect_true(all(c("n_x", "n_y") %in% names(out)))
})

test_that("models, survfit and htest become tidy rows", {
      skip_if_not_installed("broom.helpers")
      cox <- as_excel_table(survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung2))
      expect_true("Ref." %in% cox$est.conf.interval)
      expect_true(all(cox$estimate > 0, na.rm = TRUE))
      lr <- as_excel_table(glm(I(status == 2) ~ sex, data = lung2, family = binomial))
      expect_equal(lr$est.conf.interval[1], "Ref.")

      fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = lung2)
      expect_equal(nrow(as_excel_table(fit)), 2)
      expect_equal(nrow(as_excel_table(fit, stat = "times", times = c(100, 200))), 4)

      ft <- as_excel_table(fisher.test(table(lung2$sex, lung2$status)))
      expect_true(all(c("method", "p.value", "estimate") %in% names(ft)))
})

test_that("gtsummary tables are exported with plain headers", {
      skip_if_not_installed("gtsummary")
      out <- as_excel_table(gtsummary::tbl_summary(lung2[, c("sex", "age")]))
      expect_false(any(grepl("\\*\\*", names(out))))
})

test_that("unknown objects give a helpful error", {
      expect_error(as_excel_table(function() 1), "as_excel_table\\(\\) method")
})
