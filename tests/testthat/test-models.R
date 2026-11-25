lung2 <- survival::lung
lung2$sex <- factor(lung2$sex, labels = c("Male", "Female"))

test_that("tidy_survfit works with and without strata", {
      m0 <- tidy_survfit(survival::survfit(survival::Surv(time, status) ~ 1, data = lung2))
      expect_equal(m0$strata, "All")
      m1 <- tidy_survfit(survival::survfit(survival::Surv(time, status) ~ sex, data = lung2),
                         endpoint = "OS")
      expect_equal(nrow(m1), 2)
      expect_equal(names(m1)[1], "endpoint")
      tt <- tidy_survfit(survival::survfit(survival::Surv(time, status) ~ sex, data = lung2),
                         stat = "times", times = c(180, 365))
      expect_equal(nrow(tt), 4)
})

test_that("tidy_models stacks one model per variable", {
      skip_if_not_installed("broom.helpers")
      res <- tidy_models(lung2, "Surv(time, status)", c("age", "sex"))
      expect_setequal(unique(res$variable), c("age", "sex"))
      expect_true("Ref." %in% res$est.conf.interval)
      expect_true(all(c("est.conf.interval", "n_obs", "n_event") %in% names(res)))
      adj <- tidy_models(lung2, "Surv(time, status)", "sex", adjust = "age")
      expect_equal(unique(adj$variable), "sex")
      lr <- tidy_models(lung2, "I(status == 2)", "sex", model = "logistic")
      expect_true(all(lr$estimate > 0, na.rm = TRUE))
})

test_that("tidy_models skips variables that cannot be modelled", {
      skip_if_not_installed("broom.helpers")
      d <- lung2
      d$constant <- "A"
      expect_warning(res <- tidy_models(d, "Surv(time, status)", c("age", "constant")),
                     "Skipping 'constant'")
      expect_equal(unique(res$variable), "age")
})
