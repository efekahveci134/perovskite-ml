source("R/config.R"); source("R/utils.R"); source("R/01_data.R")
suppressPackageStartupMessages({
  library(rsample); library(dplyr); library(tibble); library(purrr); library(randomForest)
})

# -----------------------------------------------------------------------------
# Onisleme: egitim katmanindan ogrenilen medyan / mod ile doldurma
# -----------------------------------------------------------------------------
rf_prep <- function(train, test) {
  tr <- as.data.frame(select(train, -any_of("article")))
  te <- as.data.frame(select(test,  -any_of("article")))
  for (v in setdiff(names(tr), "y_log")) {
    if (is.numeric(tr[[v]])) {
      fill <- stats::median(tr[[v]], na.rm = TRUE)
      if (!is.finite(fill)) fill <- 0
      tr[[v]][is.na(tr[[v]])] <- fill
      te[[v]][is.na(te[[v]])] <- fill
    } else {
      tr[[v]] <- droplevels(factor(tr[[v]]))
      mode_lv <- names(which.max(table(tr[[v]])))
      # Egitimde hic gorulmemis kategori bilgi tasimaz -> egitim modu atanir
      x <- as.character(te[[v]])
      x[is.na(x) | !(x %in% levels(tr[[v]]))] <- mode_lv
      te[[v]] <- factor(x, levels = levels(tr[[v]]))
      tr[[v]][is.na(tr[[v]])] <- mode_lv
    }
  }
  list(train = tr, test = te)
}

rf_fit <- function(train, mtry, nodesize, ntree = 500L, importance = FALSE, seed = SEED) {
  set.seed(seed)
  randomForest(y_log ~ ., data = train, ntree = ntree,
               mtry = min(mtry, ncol(train) - 1L), nodesize = nodesize,
               importance = importance)
}

# -----------------------------------------------------------------------------
# 5-fold CV ile mtry x nodesize taramasi (agac sayisi makaledeki 500'de sabit)
# -----------------------------------------------------------------------------
rf_tune <- function(train, p, v = CFG$rf_cv_v, repeats = CFG$rf_cv_repeats, seed = SEED) {
  grid <- expand.grid(mtry = unique(pmin(c(2, 4, 6, 8, 10, 13, 17, 21, 26), p)),
                      nodesize = c(1L, 3L, 5L, 10L), KEEP.OUT.ATTRS = FALSE)
  set.seed(seed)
  folds <- vfold_cv(train, v = v, repeats = repeats, strata = y_log)
  map(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    per_fold <- map_dfr(folds$splits, function(sp) {
      d <- rf_prep(analysis(sp), assessment(sp))
      m <- rf_fit(d$train, g$mtry, g$nodesize)
      pr <- predict(m, d$test)
      tibble(rmse = sqrt(mean((d$test$y_log - pr)^2)),
             rsq_cor = suppressWarnings(stats::cor(d$test$y_log, pr)^2))
    })
    tibble(mtry = g$mtry, nodesize = g$nodesize,
           cv_rmse = mean(per_fold$rmse),
           cv_rmse_se = sd(per_fold$rmse) / sqrt(nrow(per_fold)),
           cv_rsq_cor = mean(per_fold$rsq_cor, na.rm = TRUE))
  }) |> bind_rows() |> arrange(cv_rmse)
}

# =============================================================================
run_rf <- function(data_phase, phase_name, seed = SEED, tag = "main") {
  msg("=== RF | ", phase_name, " | ", tag, " ===")

  # DT ile AYNI bolme (ayni seed, ayni tabakalar) -> iki model kiyaslanabilir
  set.seed(seed)
  split <- initial_split(data_phase, prop = 0.75, strata = yield, breaks = 4)
  d_all <- select(data_phase, -yield)
  train <- d_all[split$in_id, , drop = FALSE]
  test  <- d_all[-split$in_id, , drop = FALSE]
  p <- ncol(train) - 2L     # article ve y_log haric
  msg("Egitim ", nrow(train), " satir | Test ", nrow(test), " satir | ", p, " tanimlayici")

  # ---- Hiperparametre taramasi
  grid <- rf_tune(train, p)
  best <- grid[1, ]
  write_out(grid, "rf", sprintf("%s_%s_cv_grid.csv", phase_name, tag))
  msg("Secilen: mtry = ", best$mtry, ", nodesize = ", best$nodesize,
      "  (CV RMSE ", round(best$cv_rmse, 3), " +/- ", round(best$cv_rmse_se, 3), ")")

  # ---- Son model
  d <- rf_prep(train, test)
  model <- rf_fit(d$train, best$mtry, best$nodesize, ntree = PAPER$rf_ntree, importance = TRUE)
  pr_tr <- predict(model, d$train)
  pr_te <- predict(model, d$test)
  oob   <- model$predicted                       # OOB tahminleri

  met <- bind_rows(
    reg_metrics(d$train$y_log, pr_tr, "egitim_insample"),
    reg_metrics(d$train$y_log, oob,   "egitim_oob"),
    reg_metrics(d$test$y_log,  pr_te, "test")
  )
  write_out(met, "rf", sprintf("%s_%s_metrics.csv", phase_name, tag))
  print(as.data.frame(met |> mutate(across(where(is.numeric), ~ round(.x, 3)))))

  # ---- Gercek vs tahmin grafigi (makale Fig. 5 / 7 karsiligi)
  save_png(sprintf("rf/%s_%s_pred_vs_obs.png", phase_name, tag), {
    op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
    lim <- range(c(d$train$y_log, d$test$y_log, pr_tr, pr_te))
    plot(d$train$y_log, pr_tr, xlim = lim, ylim = lim, pch = 19, col = "#2c6e9bAA",
         xlab = "Gercek log10(verim + 1)", ylab = "Tahmin",
         main = sprintf("%s - egitim (R2 = %.2f)", phase_name, met$rsq_cor[1]))
    abline(0, 1, lty = 2)
    plot(d$test$y_log, pr_te, xlim = lim, ylim = lim, pch = 19, col = "#b3591aAA",
         xlab = "Gercek log10(verim + 1)", ylab = "Tahmin",
         main = sprintf("%s - test (R2 = %.2f)", phase_name, met$rsq_cor[3]))
    abline(0, 1, lty = 2)
    par(op)
  }, width = 2000, height = 1000, res = 150)

  # ---- Degisken onemi: iki olcut yan yana
  imp <- as.data.frame(importance(model))
  imp <- tibble(variable = rownames(imp),
                perm_IncMSE = imp[["%IncMSE"]],
                IncNodePurity = imp[["IncNodePurity"]]) |>
    mutate(n_levels = map_int(variable, ~ {
             x <- d$train[[.x]]; if (is.factor(x)) nlevels(droplevels(x)) else NA_integer_ }),
           rank_perm = rank(-perm_IncMSE), rank_nodepurity = rank(-IncNodePurity)) |>
    arrange(desc(perm_IncMSE))
  write_out(imp, "rf", sprintf("%s_%s_importance.csv", phase_name, tag))

  save_png(sprintf("rf/%s_%s_importance.png", phase_name, tag), {
    op <- par(mfrow = c(1, 2), mar = c(4, 9, 3, 1))
    t1 <- imp |> arrange(desc(perm_IncMSE)) |> head(12)
    barplot(rev(t1$perm_IncMSE), names.arg = rev(t1$variable), horiz = TRUE, las = 1,
            col = "#2c6e9b", cex.names = 0.8, main = "Permutasyon onemi (%IncMSE)")
    t2 <- imp |> arrange(desc(IncNodePurity)) |> head(12)
    barplot(rev(t2$IncNodePurity), names.arg = rev(t2$variable), horiz = TRUE, las = 1,
            col = "#b3591a", cex.names = 0.8, main = "IncNodePurity (makalede kullanilan)")
    par(op)
  }, width = 2200, height = 1000, res = 150)

  # ---- Agac sayisi yeterli mi? OOB hatasi egrisi
  curve_m <- rf_fit(d$train, best$mtry, best$nodesize, ntree = 2000L)
  save_png(sprintf("rf/%s_%s_ntree_curve.png", phase_name, tag), {
    plot(seq_along(curve_m$mse), curve_m$mse, type = "l", xlab = "Agac sayisi",
         ylab = "OOB MSE", main = sprintf("%s - agac sayisi yakinsamasi", phase_name))
    abline(v = 500, lty = 2, col = "red")
  }, width = 1400, height = 900, res = 150)

  # ---- Makale protokolu, AYNI bolmede
  pm <- rf_fit(d$train, PAPER$rf_mtry, 5L, ntree = PAPER$rf_ntree)
  pmet <- bind_rows(reg_metrics(d$train$y_log, predict(pm, d$train), "egitim_insample"),
                    reg_metrics(d$test$y_log,  predict(pm, d$test),  "test"))

  tibble(phase = phase_name, split = tag,
         n_train = nrow(train), n_test = nrow(test), p = p,
         mtry = best$mtry, nodesize = best$nodesize, ntree = PAPER$rf_ntree,
         cv_rmse = best$cv_rmse, cv_rmse_se = best$cv_rmse_se,
         train_rsq_cor = met$rsq_cor[1], train_rmse = met$rmse[1],
         oob_rsq_cor = met$rsq_cor[2], oob_rmse = met$rmse[2],
         test_rsq_cor = met$rsq_cor[3], test_rsq_trad = met$rsq_trad[3],
         test_rmse = met$rmse[3],
         paper_train_rsq_cor = pmet$rsq_cor[1], paper_train_rmse = pmet$rmse[1],
         paper_test_rsq_cor = pmet$rsq_cor[2], paper_test_rmse = pmet$rmse[2],
         oob_mse_500 = curve_m$mse[500], oob_mse_2000 = curve_m$mse[2000])
}

# =============================================================================
if (sys.nframe() == 0L) {
  dat <- load_phase_data()
  res <- list()
  for (ph in c("liq", "gas")) res[[ph]] <- run_rf(dat[[ph]], ph)
  out <- bind_rows(res)
  write_out(out, "rf", "rf_summary.csv")
  print(as.data.frame(out |> mutate(across(where(is.numeric), ~ round(.x, 3)))))
  msg("RF tamam.")
}
