# Karar agaci ve random forest: uc adimi sirayla calistirir.
# Bu klasorden:  Rscript calistir.R   (veya RStudio'da Source)
# Tek adim icin:  Rscript R/02_decision_tree.R  gibi.
# Cikti outputs/ altina yazilir; gosterilecek hazir sonuclar sonuclar/ klasorundedir.
# Toplam sure ~2.5 dakika.

if (!file.exists(file.path("data", "dataset.xlsm")))
  stop("Calisma klasoru proje klasoru olmali (setwd ile perovskite-sunum klasorune gecin)")

# Her script ayri bir R surecinde calisir: scriptlerin "dogrudan calistirildi mi"
# kontrolu (sys.nframe() == 0) source() ile cagrildiginda tetiklenmez.
rscript <- file.path(R.home("bin"), "Rscript")
for (s in c("R/01_data.R", "R/02_decision_tree.R", "R/03_random_forest.R")) {
  cat("\n>>> ", s, "\n", sep = "")
  if (system2(rscript, s) != 0) stop(s, " hata verdi")
}
cat("\nTamam. Ciktilar outputs/ klasorunde.\n")
