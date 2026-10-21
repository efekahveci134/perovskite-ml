DATA_PATH     <- file.path("data", "dataset.xlsm")
OUT_DIR       <- "outputs"

SEED          <- 2025L     # ana (raporlanan) split ve tuning için

# QUICK=TRUE ortam değişkeni ile hızlı "duman testi" (hata ayıklama) modu
QUICK <- identical(toupper(Sys.getenv("QUICK", "FALSE")), "TRUE")

# --- Veri seti tercihleri (duyarlılık analizi için ortam değişkeniyle değişir) ---
# FEATURE_SET:
#   "paper" = makalenin bildirdiği tanımlayıcı sayısı (26 liquid / 27 gas; B-site dahil)
#   "lean"  = B-site dışarıda (25 / 26); B, perovskit tipi ile büyük ölçüde gereksiz
FEATURE_SET <- tolower(Sys.getenv("FEATURE_SET", "paper"))

# COCAT_PCT_RULE: "cocatalyst = none ama cocat % dolu" olan 4 gas satırı için
#   "asis" = kaynaktan doğrulanamadığı için dokunma (varsayılan)
#   "zero" = yüzdeyi 0 yap (etiket doğru, yüzde hatalı varsayımı)
#   "drop" = satırları çıkar (yüzde doğru, etiket hatalı varsayımı)
COCAT_PCT_RULE <- tolower(Sys.getenv("COCAT_PCT_RULE", "asis"))

# --- Deney boyutları --------------------------------------------------------
CFG <- list(
  dt_cv_v       = 5,                      # makalede de 5-fold
  dt_cv_repeats = if (QUICK) 1 else 5,    # CV'yi 5 kez tekrarla (seçim gürültüsünü düşürür)
  rf_cv_v       = 5,
  rf_cv_repeats = if (QUICK) 1 else 3
)

# Makalede raporlanan değerler (karşılaştırma tablosu için)
PAPER <- list(
  dt_liq_train_acc = 0.88, dt_liq_test_acc = 0.73,        # Table 3
  dt_gas_train_acc = 0.92,                                 # SM Fig.1 yaprak saflıklarından hesaplandı
  dt_gas_test_acc  = 0.88,                                 # Özet (abstract)
  dt_liq_cp = 0.015, dt_gas_cp = 0.010, dt_minsplit = 5,
  rf_liq_train_r2 = 0.96, rf_liq_train_rmse = 0.21, rf_liq_test_r2 = 0.84, rf_liq_test_rmse = 0.36,
  rf_gas_train_r2 = 0.91, rf_gas_train_rmse = 0.22, rf_gas_test_r2 = 0.87, rf_gas_test_rmse = 0.24,
  rf_ntree = 500, rf_mtry = 5
)
