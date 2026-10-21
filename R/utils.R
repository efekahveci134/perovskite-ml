msg <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
  flush.console()
}

out_path <- function(...) {
  p <- file.path(OUT_DIR, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

write_out <- function(x, ...) readr::write_csv(as.data.frame(x), out_path(...))

save_png <- function(file, expr, width = 1800, height = 1200, res = 170) {
  grDevices::png(out_path(file), width = width, height = height, res = res)
  on.exit(grDevices::dev.off())
  force(expr)
}

summ <- function(x) sprintf("%.3f +/- %.3f", mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))

# --- Hedef siniflar: EGITIM setinin tertilleri -------------------------------
# Sinirlar yalnizca egitim verisinden ogrenilir (test sizintisi yok) ve elle
# yazilmaz; orijinal koddaki 8.1 / 138.1 sabitlerinin yerine gecer.
tertile_cuts <- function(y_train) {
  stats::quantile(y_train, probs = c(1/3, 2/3), type = 7, names = FALSE)
}

assign_class <- function(y, cuts) {
  factor(ifelse(y <= cuts[1], "C", ifelse(y <= cuts[2], "B", "A")),
         levels = c("A", "B", "C"))   # A = yuksek, B = orta, C = dusuk
}

# --- Agac modelleri icin veri hazirligi -------------------------------------
# article sutunu yalnizca kimlik bilgisidir, tanimlayici degil; modele girmez.
# droplevels: faz sozlugunden sabitlenen faktor seviyeleri bir CV katmaninda
# sifir satirla kalabilir, bu da sonraki adimda "gorulmemis seviye" sorununa
# yol acar.
prep_tree_data <- function(df) {
  droplevels(as.data.frame(select(df, -any_of("article"))))
}

# Egitimde hic gorulmemis bir kategori test tarafinda bilgi tasimaz; NA'ya
# cevrilir ve CART'in kendi surrogate split mekanizmasina birakilir.
match_levels <- function(df, train_df) {
  out <- as.data.frame(select(df, -any_of("article")))
  for (v in names(train_df)) {
    if (is.factor(train_df[[v]]) && v %in% names(out))
      out[[v]] <- factor(as.character(out[[v]]), levels = levels(train_df[[v]]))
  }
  out
}

# rpart, bolme degiskeni eksik ve kullanilabilir surrogate yoksa ilgili satiri
# tahmin ciktisindan DUSURUR.  Kisalmis bir vektorle `mean(pred == truth)`
# hesaplamak R'in geri dondurme (recycling) kurali yuzunden sessizce yanlis
# dogruluk uretir.  Dusen satirlara egitim cogunluk sinifi atanir: dagitima
# cikmis bir model her satir icin bir cevap vermek zorundadir.
predict_class <- function(model, newdata, levels_out = c("A", "B", "C")) {
  nd <- as.data.frame(newdata)
  rownames(nd) <- seq_len(nrow(nd))
  pr <- predict(model, newdata = nd, type = "class")
  fallback <- names(which.max(table(model$y)))
  if (is.null(fallback)) fallback <- levels_out[1]
  if (!is.null(model$frame)) {
    root <- model$frame$yval[1]
    if (!is.na(root) && root <= length(levels_out)) fallback <- levels_out[root]
  }
  out <- rep(fallback, nrow(nd))
  idx <- suppressWarnings(as.integer(names(pr)))
  if (length(pr) == nrow(nd) && (is.null(names(pr)) || anyNA(idx))) {
    out <- as.character(pr)
  } else if (length(idx) == length(pr) && !anyNA(idx)) {
    out[idx] <- as.character(pr)
  }
  factor(out, levels = levels_out)
}

# --- Siniflandirma raporu (makale Table 3 formati) --------------------------
class_report <- function(truth, estimate, set_name) {
  tab <- table(Real = truth, Predicted = estimate)
  lv  <- rownames(tab)
  tibble::tibble(
    set        = set_name,
    real_class = lv,
    n          = as.integer(rowSums(tab)),
    pred_A     = as.integer(tab[, "A"]),
    pred_B     = as.integer(tab[, "B"]),
    pred_C     = as.integer(tab[, "C"]),
    recall     = round(diag(tab) / rowSums(tab), 3),
    precision  = round(diag(tab) / colSums(tab), 3),
    accuracy   = round(sum(diag(tab)) / sum(tab), 3)
  )
}

# --- Regresyon metrikleri ---------------------------------------------------
# R2'nin iki tanimi ayri ayri raporlanir:
#   rsq_cor  = gercek ile tahmin arasindaki korelasyonun karesi.  Makaledeki
#              Fig. 5/7 "original vs predicted" grafiklerinin karsiligi.
#   rsq_trad = 1 - SSE/SST, yani aciklanan varyans orani.  Yanliligi cezalar,
#              bu yuzden daha dusuk ve daha durust bir olcuttur.
reg_metrics <- function(truth, estimate, set_name) {
  res <- truth - estimate
  tibble::tibble(
    set      = set_name,
    rmse     = sqrt(mean(res^2)),
    mae      = mean(abs(res)),
    rsq_cor  = suppressWarnings(stats::cor(truth, estimate)^2),
    rsq_trad = 1 - sum(res^2) / sum((truth - mean(truth))^2)
  )
}

# --- Eslestirilmis karsilastirma (tekrarli bolmeler icin) -------------------
# Kazanma / berabere / kaybetme oranlari HER ZAMAN sayisal sutunlardan
# hesaplanir.  (Onceki surumde bu oran, zaten metne cevrilmis "0.711 +/- 0.041"
# gibi ozet sutunlari uzerinde karsilastirildigi icin alfabetik siralama
# yapiyor ve yanlis sonuc veriyordu.)
paired_compare <- function(ours, paper, higher_is_better = TRUE) {
  keep <- is.finite(ours) & is.finite(paper)
  d <- if (higher_is_better) ours[keep] - paper[keep] else paper[keep] - ours[keep]
  nz <- d[d != 0]
  tibble::tibble(
    n = length(d),
    ours = mean(ours[keep]), paper = mean(paper[keep]),
    mean_diff = mean(d), sd_diff = stats::sd(d),
    win = mean(d > 0), tie = mean(d == 0), loss = mean(d < 0),
    wilcoxon_p = if (length(nz) >= 2)
      suppressWarnings(stats::wilcox.test(d, mu = 0)$p.value) else NA_real_
  )
}
