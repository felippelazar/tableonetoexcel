#' Tidy a survfit: median survival or landmark estimates
#'
#' Replacement for `tidy_mstrata_survfit()`. Works with or without strata and
#' returns a data frame. `book$add(<survfit>)` uses it directly.
#'
#' @param fit A `survfit` object.
#' @param stat `"median"` (median with 95% CI per stratum) or `"times"`
#'   (survival estimates at `times`).
#' @param times Landmark times, used when `stat = "times"`.
#' @param endpoint Optional label stored in a first `endpoint` column, handy
#'   when binding several fits together (e.g. `"OS All"`).
#' @param digits Decimals in the formatted `"est (low - high)"` column.
#'
#' @return A data.frame. `stat = "median"` gives one row per stratum with
#'   `records`, `events`, `median`, `conf.low`, `conf.high`,
#'   `med_est_interval`; `stat = "times"` gives one row per stratum and time
#'   with `n.risk`, `n.event`, `n.censor`, `surv_est_interval`,
#'   `surv_estimate`, `surv_lower`, `surv_higher`.
#' @export
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#' tidy_survfit(fit, endpoint = "OS")
#' tidy_survfit(fit, stat = "times", times = c(180, 365))
tidy_survfit <- function(fit, stat = c("median", "times"),
                         times = c(6, 12, 18, 24, 30, 36), endpoint = NULL,
                         digits = 2) {
      stat <- match.arg(stat)
      if (!inherits(fit, "survfit")) stop("`fit` must be a survfit object.", call. = FALSE)
      fmt <- paste0("%.", digits, "f (%.", digits, "f - %.", digits, "f)")
      s <- if (stat == "times") summary(fit, times = times) else summary(fit)

      if (stat == "median") {
            tab <- s$table
            # Without strata summary()$table is a named vector, not a matrix
            if (is.null(dim(tab))) tab <- t(tab)
            strata <- if (is.null(rownames(tab))) "All" else rownames(tab)
            tab <- as.data.frame(tab, check.names = FALSE)
            out <- data.frame(strata = strata, tab, check.names = FALSE)
            names(out)[names(out) == "0.95LCL"] <- "conf.low"
            names(out)[names(out) == "0.95UCL"] <- "conf.high"
            out$med_est_interval <- sprintf(fmt, out$median, out$conf.low, out$conf.high)
      } else {
            strata <- if (is.null(s$strata)) rep("All", length(s$time)) else as.character(s$strata)
            out <- data.frame(strata = strata,
                              time = s$time,
                              n.risk = s$n.risk,
                              n.event = s$n.event,
                              n.censor = s$n.censor,
                              surv_est_interval = sprintf(fmt, s$surv, s$lower, s$upper),
                              surv_estimate = s$surv,
                              surv_lower = s$lower,
                              surv_higher = s$upper)
      }

      rownames(out) <- NULL
      if (!is.null(endpoint)) out <- data.frame(endpoint = endpoint, out, check.names = FALSE)
      out
}

#' Univariable (or adjusted) regression for many variables at once
#'
#' Replacement for the `tidy_cox()` / `tidy_glm()` + `lapply()` +
#' `bind_rows()` pattern: fits one model per variable in `vars` and stacks the
#' tidy results (via [broom.helpers::tidy_plus_plus()], which adds reference
#' rows and labels). A variable that cannot be fitted (e.g. a single level
#' left after filtering) is skipped with a warning.
#'
#' @param data A data frame.
#' @param outcome Left-hand side as text, e.g. `"Surv(os_time, os_status)"`
#'   or `"response_bin"`.
#' @param vars Character vector of predictors, one model each.
#' @param model `"cox"` ([survival::coxph()]), `"logistic"` (binomial
#'   [stats::glm()]) or `"linear"` (gaussian [stats::glm()]).
#' @param adjust Optional covariates added to every model (e.g. `"age"`); only
#'   the rows of the variable of interest are kept.
#' @param exponentiate Return HR/OR instead of log-scale coefficients.
#' @param digits Decimals in the formatted `"est (low - high)"` column.
#' @param full Keep every column from [broom.helpers::tidy_plus_plus()]
#'   instead of the short set (variable, label, n, estimate, CI, p-value).
#'
#' @return A data.frame with one block of rows per variable and the column
#'   `est.conf.interval` (`"Ref."` on reference rows), ready for `book$add()`.
#' @export
#' @examples
#' library(survival)
#' lung$sex <- factor(lung$sex, labels = c("Male", "Female"))
#' tidy_models(lung, "Surv(time, status)", c("age", "sex", "ph.ecog"))
tidy_models <- function(data, outcome, vars, model = c("cox", "logistic", "linear"),
                        adjust = NULL, exponentiate = model != "linear",
                        digits = 2, full = FALSE) {
      model <- match.arg(model)
      if (model == "cox" && !requireNamespace("survival", quietly = TRUE)) {
            stop("Install survival to fit Cox models.", call. = FALSE)
      }
      one <- function(v) {
            # Surv() must resolve even when survival is not attached, and `f`
            # and `data` must be found when broom.helpers re-evaluates the call
            env <- if (model == "cox") {
                  list2env(list(Surv = survival::Surv), parent = environment())
            } else {
                  environment()
            }
            f <- stats::as.formula(paste(outcome, "~", paste(c(v, adjust), collapse = " + ")),
                                   env = env)
            m <- switch(model,
                        cox = survival::coxph(f, data = data),
                        logistic = stats::glm(f, data = data, family = stats::binomial()),
                        linear = stats::glm(f, data = data))
            tidy_model(m, exponentiate = exponentiate, digits = digits,
                       full = TRUE, variables = v)
      }

      results <- lapply(vars, function(v) {
            tryCatch(one(v), error = function(e) {
                  warning("Skipping '", v, "': ", conditionMessage(e), call. = FALSE)
                  NULL
            })
      })
      results <- Filter(Negate(is.null), results)
      if (length(results) == 0) stop("No model could be fitted.", call. = FALSE)
      cols <- unique(unlist(lapply(results, names)))
      results <- lapply(results, function(r) {
            r[setdiff(cols, names(r))] <- NA
            r[cols]
      })
      out <- do.call(rbind, results)
      rownames(out) <- NULL
      if (!full) out <- short_columns(out)
      out
}

# One fitted model -> tidy data frame with "est (low - high)" and Ref. rows
tidy_model <- function(m, exponentiate = FALSE, digits = 2, full = FALSE,
                       variables = NULL) {
      if (!requireNamespace("broom.helpers", quietly = TRUE)) {
            stop("Install broom.helpers to export model results.", call. = FALSE)
      }
      fmt <- paste0("%.", digits, "f (%.", digits, "f - %.", digits, "f)")
      res <- as.data.frame(broom.helpers::tidy_plus_plus(m, exponentiate = exponentiate))
      if (!is.null(variables)) res <- res[res$variable %in% variables, , drop = FALSE]
      res$est.conf.interval <- ifelse(
            is.na(res$conf.low), "",
            sprintf(fmt, res$estimate, res$conf.low, res$conf.high))
      if ("reference_row" %in% names(res)) {
            res$est.conf.interval[res$reference_row %in% TRUE] <- "Ref."
      }
      rownames(res) <- NULL
      if (!full) res <- short_columns(res)
      res
}

short_columns <- function(df) {
      keep <- c("variable", "label", "n_obs", "n_event", "estimate",
                "conf.low", "conf.high", "p.value", "est.conf.interval")
      df[intersect(keep, names(df))]
}
