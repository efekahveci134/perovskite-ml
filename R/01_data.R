suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tibble); library(stringr); library(tidyr)
})

# Tanımlayıcı seti config'teki anahtarlara bağlı olduğu için config bu dosyanın
# ÜST seviyesinde gerekli; tek başına çalıştırıldığında da yüklenmeli.
if (!exists("FEATURE_SET")) source("R/config.R")
if (!exists("msg"))         source("R/utils.R")

# Excel başlığı  ->  kısa, sözdizimsel ad
HEADER_MAP <- c(
  id = "no", article = "Article No", doi = "Doi Number",
  used_form = "Used form Perovskite", type = "Type of perovskite", perovskite = "Perovskite",
  A = "A", x_A = "x of A", A1B1 = "A1/B1", x_A1B1 = "x of A1/B1",
  B = "B", x_B = "x of B", X = "X", x_X = "x of X",
  additional = "Additional material", additional_ratio = "Ratio of additional to the perovskite",
  synthesis = "Synthesis Method of Perovskite", dope = "Dope", dope_pct = "deope percentage (%)",
  cocat_pct = "cocatalyst %", cocat_method = "cocatalyst method", cocat = "cocatalyst",
  cocat2_pct = "cocat-2 %", cocat2_method = "cocat-2 method", cocat2 = "cocat-2",
  bet = "Bet Surface Area (m2g-1)", calc_time = "Calcination Time (h)",
  calc_temp = "Calcination Temperature (C )", cat_w = "cat W (g)", wavelength = "wavelenght",
  light = "Light Type", rxn_temp = "Reaction Temperature (C )",
  rxn_pressure = "Reaction Pressure (bar)", phase = "Liq or Gas", band_gap = "Band Gap*",
  crystal = "Crystal stracture", h2o_co2 = "H2O:CO2", yield = "total yield"
)

# Modelde kullanılan tanımlayıcılar.
#
# Makale Table 1 + Table 2'de 29 tanımlayıcı listeleniyor (15 kategorik + 14
# sayısal). Faz sütunu modele girmez (zaten ayrı modelleniyor) ve X sütunu
# incelenen inorganik perovskitlerde sabittir; kalan set tam olarak makalenin
# metinde bildirdiği sayıyı verir: "138 samples, with 27 descriptors ... for the
# gas phase while these numbers were 190 data points and 26 descriptors for
# liquid phase" (s. 3). FEATURE_SET = "paper" bu seti kurar.
#
# Kasıtlı olarak DIŞARIDA: wavelength (%45 eksik ve Table 1'de yok), X (sabit
# "O"), Perovskite adı (42 seviyeli, kimlik benzeri), cat_w (verim zaten
# g-başına normalize), additional_ratio (%88 eksik), faz sütunu (faz içinde
# sabit), kimlik/DOI sütunları.
FEATURES_PAPER <- c(
  "type", "A", "x_A", "A1B1", "x_A1B1", "B", "x_B", "x_X", "additional", "synthesis",
  "dope", "dope_pct", "cocat_pct", "cocat_method", "cocat", "cocat2_pct",
  "cocat2_method", "cocat2", "bet", "calc_time", "calc_temp", "light",
  "rxn_temp", "rxn_pressure", "band_gap", "crystal"
)
# Duyarlılık seti: B-site dışarıda. B, perovskit tipi (Table 2 kategorileri
# B-site'a göre tanımlanmış) ile büyük ölçüde gereksizdir.
FEATURES_LEAN <- setdiff(FEATURES_PAPER, "B")

FEATURES_COMMON <- if (identical(FEATURE_SET, "lean")) FEATURES_LEAN else FEATURES_PAPER

features_for <- function(phase) {
  if (phase == "gas") c(FEATURES_COMMON, "h2o_co2") else FEATURES_COMMON
}
CATEGORICAL <- c("type", "A", "A1B1", "B", "additional", "synthesis", "dope", "cocat_method",
                 "cocat", "cocat2_method", "cocat2", "light", "crystal")

# -----------------------------------------------------------------------------
read_raw_dataset <- function(path = DATA_PATH) {
  raw <- readxl::read_excel(path, sheet = 1, n_max = 340, .name_repair = "minimal",
                            guess_max = 400)
  names(raw) <- stringr::str_squish(names(raw))
  ycol <- grep("^total yield", names(raw), value = TRUE)
  if (length(ycol) != 1) stop("Verim sutunu bulunamadi")
  names(raw)[names(raw) == ycol] <- "total yield"
  miss <- setdiff(HEADER_MAP, names(raw))
  if (length(miss)) stop("Beklenen sutunlar yok: ", paste(miss, collapse = ", "))
  out <- raw[, unname(HEADER_MAP)]
  names(out) <- names(HEADER_MAP)
  out <- out |> filter(!is.na(phase), !is.na(yield))   # dipnot/boş satırlar düşer
  if (nrow(out) != 328) stop("Beklenen 328 satir, bulunan: ", nrow(out))
  num_cols <- c("x_A", "x_A1B1", "x_B", "x_X", "dope_pct", "cocat_pct", "cocat2_pct", "bet",
                "calc_time", "calc_temp", "cat_w", "wavelength", "rxn_temp", "rxn_pressure",
                "band_gap", "h2o_co2", "yield", "article", "id")
  out |> mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))),
                across(where(is.character), ~ stringr::str_squish(.x)))
}

# -----------------------------------------------------------------------------
# Veri denetimi: tutarsızlıkları raporlar, DEĞİŞTİRMEZ.
audit_dataset <- function(raw) {
  flag <- function(cond, issue, action) {
    rows <- which(cond)
    if (!length(rows)) return(NULL)
    tibble(issue = issue, n_rows = length(rows),
           excel_rows = paste(rows + 1, collapse = " "),   # +1: başlık satırı
           phase = paste(unique(raw$phase[rows]), collapse = "/"), action = action)
  }
  desc_cols <- c(FEATURES_COMMON, "h2o_co2", "phase")
  key <- do.call(paste, c(lapply(raw[desc_cols], as.character), sep = "|"))
  dup_rows <- key %in% key[duplicated(key)]
  bind_rows(
    flag(raw$cocat == "Nİ", "cocatalyst = 'Nİ' (Turkce buyuk I ile yazilmis 'Ni')",
         "Ni olarak duzeltildi (kodlama hatasi)"),
    flag(raw$dope != "none" & is.na(raw$dope_pct), "Dopant var ama dope % bos",
         "0 YAPILMADI; egitim katmaninda medyan ile dolduruldu (RF) / surrogate (DT)"),
    flag(raw$cocat == "none" & !is.na(raw$cocat_pct), "cocatalyst = none ama cocat % dolu",
         "Degistirilmedi (kaynak makaleden dogrulanamiyor); hocaya soruldu"),
    flag(raw$cocat2 != "none" & is.na(raw$cocat2_pct), "cocat-2 var ama yuzdesi bos",
         "Egitim katmaninda medyan ile dolduruldu"),
    flag(is.na(raw$x_A) | is.na(raw$x_B) | is.na(raw$x_X), "x of A/B/X bos (oksinitrurler)",
         "Egitim katmaninda medyan ile dolduruldu"),
    flag(is.na(raw$crystal), "Kristal yapi bos", "'unknown' kategorisi (makaledeki bos seviye ile esdeger)"),
    flag(is.na(raw$synthesis), "Sentez yontemi bos", "'unknown' kategorisi"),
    flag(is.na(raw$bet), "BET bos", "Egitim katmaninda medyan ile dolduruldu"),
    flag(is.na(raw$calc_temp), "Kalsinasyon sicakligi bos",
         "25 C (kalsinasyon yapilmadi) ve sure = 0 (makale: 25 C + ortalama sure)"),
    flag(!is.na(raw$calc_temp) & is.na(raw$calc_time), "Kalsinasyon suresi bos (sicaklik var)",
         "Egitim katmaninda medyan ile dolduruldu"),
    flag(is.na(raw$rxn_temp), "Reaksiyon sicakligi bos", "25 C (oda sicakligi; makale ve hoca kurali)"),
    flag(is.na(raw$rxn_pressure), "Reaksiyon basinci bos", "1 bar (makale ve hoca kurali)"),
    flag(is.na(raw$wavelength), "Dalga boyu bos", "Degisken modelden cikarildi (%45 eksik)"),
    flag(raw$yield == 0, "Verim = 0", "log10(y + 1) donusumu ile tanimli"),
    flag(dup_rows, "Tum tanimlayicilar ayni, verim farkli",
         "Dokunulmadi; kaydedilmemis degisken var -> ulasilabilir dogruluga ust sinir"),
    flag(raw$article %in% (raw |> distinct(article, doi) |> count(article) |> filter(n > 1) |> pull(article)),
         "Ayni makale numarasinda farkli DOI", "Modele etkisi yok (DOI kullanilmiyor)")
  )
}

# -----------------------------------------------------------------------------
clean_dataset <- function(raw) {
  raw |>
    mutate(
      # 1) Kodlama hatası
      cocat = if_else(cocat == "Nİ", "Ni", cocat),
      # 2) Kategorik eksikler: anlamına göre iki farklı etiket
      #    "none"    = bileşen kullanılmamış (gerçek bir bilgi)
      #    "unknown" = bilgi raporlanmamış
      across(c(A1B1, dope, cocat, cocat_method, cocat2, cocat2_method), ~ coalesce(.x, "none")),
      across(c(crystal, synthesis), ~ coalesce(.x, "unknown")),
      # 3) Alan bilgisine dayalı kurallar (makale + hoca)
      not_calcined = is.na(calc_temp),
      calc_temp    = if_else(not_calcined, 25, calc_temp),
      calc_time    = if_else(not_calcined & is.na(calc_time), 0, calc_time),
      rxn_temp     = coalesce(rxn_temp, 25),
      rxn_pressure = coalesce(rxn_pressure, 1),
      # 4) Yapısal sıfırlar: bileşen yoksa miktarı 0'dır (eksik değil)
      x_A1B1     = if_else(A1B1   == "none" & is.na(x_A1B1),     0, x_A1B1),
      dope_pct   = if_else(dope   == "none" & is.na(dope_pct),   0, dope_pct),
      cocat_pct  = if_else(cocat  == "none" & is.na(cocat_pct),  0, cocat_pct),
      cocat2_pct = if_else(cocat2 == "none" & is.na(cocat2_pct), 0, cocat2_pct),
      # 5) Hedef: log10(y + 1). Makaledeki eksenler (0-2.8) ve RMSE/R2 oranı ile
      #    doğrulandı; +1, sıfır verimli 3 satırı tanımlı tutar.
      y_log = log10(yield + 1)
    ) |>
    select(-not_calcined) |>
    apply_cocat_pct_rule()
}

# "cocatalyst = none ama cocat % dolu" olan 4 gas satırı kaynak makalelerden
# doğrulanamıyor. Varsayılan davranış veriye dokunmamaktır; iki alternatif
# varsayım COCAT_PCT_RULE ortam değişkeniyle seçilebilir (bkz. config.R).
apply_cocat_pct_rule <- function(d) {
  bad <- d$cocat == "none" & !is.na(d$cocat_pct) & d$cocat_pct > 0
  switch(COCAT_PCT_RULE,
    asis = d,
    zero = d |> mutate(cocat_pct = if_else(bad, 0, cocat_pct)),
    drop = d[!bad, , drop = FALSE],
    stop("Bilinmeyen COCAT_PCT_RULE: ", COCAT_PCT_RULE)
  )
}

# -----------------------------------------------------------------------------
# Faz verisini modele hazır hale getir (orijinal satır sırası korunur)
prepare_phase <- function(clean, phase_name) {
  feats <- features_for(phase_name)
  d <- clean |>
    filter(phase == phase_name) |>
    select(article, all_of(feats), yield, y_log)
  # Faktör seviyeleri fazın tüm "sözlüğünden" sabitlenir -> CV katmanlarında
  # yeni seviye hatası oluşmaz (seviye adı etiket bilgisi taşımaz, sızıntı değil)
  cats <- intersect(CATEGORICAL, feats)
  d |> mutate(across(all_of(cats), ~ factor(.x, levels = sort(unique(.x)))))
}

load_phase_data <- function() {
  raw   <- read_raw_dataset()
  clean <- clean_dataset(raw)
  list(raw = raw, clean = clean,
       liq = prepare_phase(clean, "liq"),
       gas = prepare_phase(clean, "gas"))
}

# Script olarak çalıştırılırsa: denetim raporunu ve veri özetini yaz
if (sys.nframe() == 0L || isTRUE(getOption("run_data_audit"))) {
  dat <- load_phase_data()
  audit <- audit_dataset(dat$raw)
  write_out(audit, "data", "data_audit.csv")
  na_after <- lapply(c("liq", "gas"), function(p) {
    d <- dat[[p]]
    tibble(phase = p, variable = names(d), n_missing = colSums(is.na(d)))
  }) |> bind_rows() |> filter(n_missing > 0)
  write_out(na_after, "data", "missing_after_domain_rules.csv")
  msg("Liquid: ", nrow(dat$liq), " satir, ", length(features_for("liq")), " tanimlayici")
  msg("Gas   : ", nrow(dat$gas), " satir, ", length(features_for("gas")), " tanimlayici")
  print(audit, n = 50, width = 200)
  cat("\nAlan kurallarindan sonra kalan eksikler (egitim katmaninda doldurulacak):\n")
  print(na_after, n = 50)
}
