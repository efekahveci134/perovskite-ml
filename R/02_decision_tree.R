source("R/config.R"); source("R/utils.R"); source("R/01_data.R")
suppressPackageStartupMessages({
  library(rsample); library(dplyr); library(tibble); library(purrr)
  library(rpart); library(rpart.plot)
})

# Makalenin bildirdigi izgara (s. 6: minsplit 5-30 adim 5, cp 0.01-0.1 adim
# 0.005) + cp'nin 0.01 altindaki bolgesi.
DT_GRID <- expand.grid(minsplit = seq(5L, 30L, by = 5L),
                       cp       = c(0.002, 0.004, 0.006, 0.008, seq(0.01, 0.10, by = 0.005)),
                       KEEP.OUT.ATTRS = FALSE)

dt_fit <- function(train, minsplit, cp) {
  rpart(class ~ ., data = train, method = "class",
        control = rpart.control(minsplit = minsplit, cp = cp, xval = 0))
}

# SM Sekil 1 ile ayni cizim: rattle::fancyRpartPlot.
# Kutuda sinif, uc sinif olasiligi ve ornek yuzdesi; yapraklar altta hizali.
plot_fancy_tree <- function(model, file, main) {
  if (!requireNamespace("rattle", quietly = TRUE)) {
    msg("rattle yuklu degil, fancy agac atlandi: ", file)
    return(invisible(NULL))
  }
  save_png(file,
           rattle::fancyRpartPlot(model, main = main, sub = "",
                                  caption = "yes", type = 2,
                                  palettes = c("Greens", "Blues", "Oranges")),
           width = 3600, height = 2000, res = 180)
}

# -----------------------------------------------------------------------------
# Tekrarli tabakali 5-fold CV ile izgara taramasi
# -----------------------------------------------------------------------------
# Tekrar NEDEN gerekli?  Egitim seti 102-142 satir; 5-fold'da her katman 20-28
# satir, yani dogruluk ancak 1/20 = 0.05 adimlarla olculebiliyor.  Tek bir CV
# ile bircok parametre ikilisi TAM BERABERE kaliyor ve secim fiilen esitlik
# bozma kuralinin eline geciyor.  Olctuk: tek 5-fold ile gas fazinda 25
# bolmenin 19'unda cp >= 0.015 seciliyor (medyan 9 yaprak, makale 14) ve test
# dogrulugu dusuyor.  CV'yi tekrarlamak bu gurultuyu dusurur.
dt_tune <- function(train, grid = DT_GRID, v = CFG$dt_cv_v,
                    repeats = CFG$dt_cv_repeats, seed = SEED) {
  set.seed(seed)
  folds <- vfold_cv(train, v = v, repeats = repeats, strata = class)
  acc <- map(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    per_fold <- map_dbl(folds$splits, function(sp) {
      tr <- prep_tree_data(analysis(sp))
      te <- match_levels(assessment(sp), tr)
      m  <- dt_fit(tr, g$minsplit, g$cp)
      mean(predict_class(m, te) == te$class)
    })
    tibble(minsplit = g$minsplit, cp = g$cp,
           cv_accuracy = mean(per_fold),
           cv_accuracy_se = sd(per_fold) / sqrt(length(per_fold)),
           n_fits = length(per_fold))
  }) |> bind_rows()
  list(grid = acc, best = dt_pick(acc))
}

# Secim: en iyi ortalama CV dogrulugu.  TAM esitlikte once tahmini EN KARARLI
# olan (standart hatasi kucuk) konfigurasyon, sonra daha basit agac (buyuk cp)
# tercih edilir.  "Esitlikte her zaman buyuk cp" kurali tek basina kullanildiginda
# sistematik asiri budamaya yol aciyordu.
dt_pick <- function(acc) {
  acc |> arrange(desc(cv_accuracy), cv_accuracy_se, desc(cp), desc(minsplit)) |> slice(1)
}

# =============================================================================
run_dt <- function(data_phase, phase_name, paper_cp, seed = SEED, tag = "main") {
  msg("=== DT | ", phase_name, " | ", tag, " ===")

  # ---- 1) 75/25 bolme (verim tabakali: her iki tarafta benzer verim dagilimi)
  set.seed(seed)
  split <- initial_split(data_phase, prop = 0.75, strata = yield, breaks = 4)

  # ---- 2) Sinif esikleri YALNIZCA egitim verisinden
  cuts <- tertile_cuts(training(split)$yield)
  labelled <- data_phase |> mutate(class = assign_class(yield, cuts)) |> select(-yield, -y_log)
  train <- labelled[split$in_id, , drop = FALSE]
  test  <- labelled[-split$in_id, , drop = FALSE]
  msg("Esikler (egitimden): C <= ", signif(cuts[1], 4), " < B <= ", signif(cuts[2], 4), " < A")
  msg("Egitim ", nrow(train), " satir ", paste(names(table(train$class)), table(train$class),
      sep = ":", collapse = " "), " | Test ", nrow(test), " satir ",
      paste(names(table(test$class)), table(test$class), sep = ":", collapse = " "))

  # ---- 3) Izgara taramasi
  tuned <- dt_tune(train)
  best  <- tuned$best
  msg("Secilen: minsplit = ", best$minsplit, ", cp = ", best$cp,
      "  (CV dogrulugu ", round(best$cv_accuracy, 3), " +/- ", round(best$cv_accuracy_se, 3), ")")
  write_out(tuned$grid, "dt", sprintf("%s_%s_cv_grid.csv", phase_name, tag))

  # ---- 4) Son model ve TEK test olcumu
  tr <- prep_tree_data(train)
  te <- match_levels(test, tr)
  model <- dt_fit(tr, best$minsplit, best$cp)
  pred_tr <- predict_class(model, tr)
  pred_te <- predict_class(model, te)
  train_acc <- mean(pred_tr == tr$class)
  test_acc  <- mean(pred_te == te$class)
  n_leaves  <- sum(model$frame$var == "<leaf>")
  msg("Egitim dogrulugu ", round(train_acc, 3), " | Test dogrulugu ", round(test_acc, 3),
      " | yaprak sayisi ", n_leaves)

  # ---- 5) Ciktilar
  write_out(bind_rows(class_report(tr$class, pred_tr, "train"),
                      class_report(te$class, pred_te, "test")),
            "dt", sprintf("%s_%s_confusion.csv", phase_name, tag))

  save_png(sprintf("dt/%s_%s_tree.png", phase_name, tag),
           rpart.plot(model, type = 2, extra = 104, under = TRUE, faclen = 0, varlen = 0,
                      roundint = FALSE, cex = 0.55,
                      box.palette = list("Greens", "Blues", "Oranges"),
                      main = sprintf("%s fazi DT (minsplit = %d, cp = %.3f)",
                                     phase_name, best$minsplit, best$cp)),
           width = 2800, height = 1700, res = 200)

  # Makaledeki SM Sekil 1 ile ayni gorsel: rattle::fancyRpartPlot
  # (3B kutular, dugum no, sinif olasiliklari, ornek yuzdesi).
  plot_fancy_tree(model,
                  sprintf("dt/%s_%s_fancy.png", phase_name, tag),
                  sprintf("%s phase  |  minsplit = %d, cp = %.3f",
                          phase_name, best$minsplit, best$cp))
  writeLines(capture.output(print(rpart.rules(model, cover = TRUE, roundint = FALSE))),
             out_path("dt", sprintf("%s_%s_rules.txt", phase_name, tag)))

  vi <- model$variable.importance
  if (is.null(vi)) vi <- c(none = 0)
  write_out(tibble(variable = names(vi), importance = round(unname(vi), 3)),
            "dt", sprintf("%s_%s_importance.csv", phase_name, tag))

  # ---- Makale protokolu, AYNI bolmede (karsilastirma icin)
  pm <- dt_fit(tr, PAPER$dt_minsplit, paper_cp)
  paper_tr <- mean(predict_class(pm, tr) == tr$class)
  paper_te <- mean(predict_class(pm, te) == te$class)
  plot_fancy_tree(pm,
                  sprintf("dt/%s_%s_paper_protocol_fancy.png", phase_name, tag),
                  sprintf("%s phase, paper protocol  |  minsplit = %d, cp = %.3f",
                          phase_name, PAPER$dt_minsplit, paper_cp))

  tibble(phase = phase_name, split = tag,
         n_train = nrow(train), n_test = nrow(test),
         cut_low = cuts[1], cut_high = cuts[2],
         minsplit = best$minsplit, cp = best$cp,
         cv_accuracy = best$cv_accuracy, cv_accuracy_se = best$cv_accuracy_se,
         n_leaves = n_leaves,
         train_acc = train_acc, test_acc = test_acc,
         paper_minsplit = PAPER$dt_minsplit, paper_cp = paper_cp,
         paper_train_acc = paper_tr, paper_test_acc = paper_te,
         paper_n_leaves = sum(pm$frame$var == "<leaf>"))
}

# =============================================================================
if (sys.nframe() == 0L) {
  dat <- load_phase_data()
  res <- list()
  for (ph in c("liq", "gas")) {
    res[[ph]] <- run_dt(dat[[ph]], ph,
                        paper_cp = if (ph == "liq") PAPER$dt_liq_cp else PAPER$dt_gas_cp)
  }
  out <- bind_rows(res)
  write_out(out, "dt", "dt_summary.csv")
  print(as.data.frame(out |> mutate(across(where(is.numeric), ~ round(.x, 3)))))
  msg("DT tamam.")
}
