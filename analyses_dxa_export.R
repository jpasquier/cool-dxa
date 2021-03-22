library(officer)
library(rvg)
library(readxl)
library(writexl)
#library(knitr)
#library(dplyr)
#library(tidyr)
library(ggplot2)
#library(gridExtra)
#library(stargazer)
library(broom)

# Working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Export directory
outdir <- path.expand(file.path(getwd(), "results/export_20210308"))
if (!dir.exists(outdir)) dir.create(outdir)

# Import data
dxa <- read_xlsx("data-raw/COOL-OsteoLaus for stat (clean).xlsx",
                 sheet = "COOL data")
dxa <- as.data.frame(dxa)
dxa <- dxa[!(dxa$Id %in% c("Dx00000255", "Dx00001488")), ]
#dxa$`Follow-up DXA Years...16`
#dxa$`Follow-up DXA Years...17`

# Recode variables
for (v in names(dxa)) {
  x <- dxa[[v]]
  if (is.character(x)) {
    x <- trimws(x)
    if (all(is.na(x) | grepl("^[0-9]+((\\.|,)[0-9]+)?$", x))) {
      x <- sub(",", "\\.", x)
      x <- as.numeric(x)
    }
    dxa[[v]] <- x
  }
}
rm(v, x)
dxa$DXA_Date <- as.Date(dxa$DXA_Date, format = "%d/%m/%Y")
dxa$`Date Bariatric surgery` <- 
  as.Date(dxa$`Date Bariatric surgery`, format = "%d/%m/%Y")

# Help function(s)
getfml <- function(fit) {
  as.character(fit$call$formula) %>% {paste(.[2], .[1], .[3])}
}


# Rename variables and new variables
rename_matrix <- matrix(byrow = TRUE, ncol = 2, c(
  "DX03_age",                          "DX03_Age",
  "DX06_BMI",                          "DX06_BMI",
  "Total weight loss (TWL)",           "TotalWeightLoss", 
  "Excès weight loss",                 "ExcessWeightLoss",
  "DX58_LeanMassTot%",                 "DX58_LeanMassTotPercent",
  "DX62_ALMI (indice de masse maigre appendiculaire ajusté)", "DX62_ALMI",
  "Nadir weight",                      "NadirWeight",
  "DX55_Tissu adipeux viscéral (VAT)", "DX55_VAT",
  "T2D-PreOp",                         "T2DPreOp",
  "HTA-PreOp",                         "HTAPreOp",
  "Dyslipidemia-PreOp",                "DyslipidemiaPreOp",
  "T2D-PostOp",                        "T2DPostOp",
  "HTA-PostOp",                        "HTAPostOp",
  "Dyslipidemia-PostOp",               "DyslipidemiaPostOp",
  "DX59_FatMassTotale%",               "DX59_FatMassTotPercent"
))
colnames(rename_matrix) <- c("Name in xlsx file", "Name in analyses")
dta <- dxa
for (i in 1:nrow(rename_matrix)) {
  names(dta)[names(dta) == rename_matrix[i, 1]] <- rename_matrix[i, 2]
}
rm(rename_matrix, i)
dta$FollowUpYears <- as.numeric(dta$DXA_Date - dxa$`Date Bariatric surgery`)
dta$FollowUpYears <- dta$FollowUpYears / 365.2425
dta$DX03_Age_C <- dta$DX03_Age - mean(dta$DX03_Age)
dta$TotalWeightLoss_C <- dta$TotalWeightLoss - mean(dta$TotalWeightLoss)
dta$ExcessWeightLoss_C <- dta$ExcessWeightLoss - mean(dta$ExcessWeightLoss)
dta$NadirWeight_C <- dta$NadirWeight - mean(dta$NadirWeight)
dta$FollowUpYears_C <- dta$FollowUpYears - mean(dta$FollowUpYears)
dta$DX59_FatMassTotPercent_C <-
  dta$DX59_FatMassTotPercent - mean(dta$DX59_FatMassTotPercent)

# Export graphics of univariable regressions
X <- c("TotalWeightLoss", "ExcessWeightLoss", "NadirWeight", "DX03_Age",
       "FollowUpYears")
figs <- lapply(X, function(x) {
  fml <- as.formula(paste("DX58_LeanMassTotPercent ~", x))
  fit <- lm(fml, dta)
  b <- signif(coef(fit)[[2]], 3)
  ci <- signif(confint(fit)[2, ], 3)
  p <- coef(summary(fit))[2, "Pr(>|t|)" ]
  p <- if (p >= 0.001) paste0("p=", round(p, 3)) else "p<0.001"
  r2 <- paste0("R2=", round(summary(fit)$r.squared, 3))
  cap <- paste0("b = ", b, " (", ci[1], ",", ci[2], "), ", p, ", ", r2)
  fig <- ggplot(dta, aes_string(y = "DX58_LeanMassTotPercent", x = x)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    labs(caption = cap, y = "LeanMassTotPrct") +
    theme(axis.title=element_text(size = rel(.75)))
})
for (i in 1:length(figs)) {
  doc <- read_pptx()
  doc <- add_slide(doc, 'Title and Content', 'Office Theme')
  anyplot <- dml(ggobj = figs[[i]])
  doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
  print(doc, target = file.path(outdir, paste0("fig_page2_", i, ".pptx")))
}
rm(X, figs, i, doc, anyplot)

# Régressions multivariables DX58_LeanMassTotPercent ~TotalWeightLoss
fits <- list(
  lm(DX58_LeanMassTotPercent ~ TotalWeightLoss_C * NadirWeight_C, dta),
  lm(DX58_LeanMassTotPercent ~ TotalWeightLoss_C * DX03_Age_C, dta),
  lm(DX58_LeanMassTotPercent ~ TotalWeightLoss_C * FollowUpYears_C, dta),
  lm(DX58_LeanMassTotPercent ~ TotalWeightLoss_C + NadirWeight_C +
       FollowUpYears_C + DX03_Age_C, dta),
  lm(DX58_LeanMassTotPercent ~ TotalWeightLoss_C * NadirWeight_C +
       FollowUpYears_C + DX03_Age_C, dta),
  lm(DX58_LeanMassTotPercent ~ TotalWeightLoss_C * (NadirWeight_C +
       FollowUpYears_C + DX03_Age_C), dta)
)
for(i in 1:3) {
  fit <- fits[[i + 3]]
  write_xlsx(list(coefficients = tidy(fit), stats = glance(fit)),
             file.path(outdir, paste0("tbl_page4_", i, ".xlsx")))
}
rm(fits, i, fit)

# Export graphics of univariable regressions
X <- c("TotalWeightLoss", "ExcessWeightLoss", "NadirWeight", "DX03_Age",
       "FollowUpYears")
figs <- lapply(X, function(x) {
  fml <- as.formula(paste("DX62_ALMI ~", x))
  fit <- lm(fml, dta)
  b <- signif(coef(fit)[[2]], 3)
  ci <- signif(confint(fit)[2, ], 3)
  p <- coef(summary(fit))[2, "Pr(>|t|)" ]
  p <- if (p >= 0.001) paste0("p=", round(p, 3)) else "p<0.001"
  r2 <- paste0("R2=", round(summary(fit)$r.squared, 3))
  cap <- paste0("b = ", b, " (", ci[1], ",", ci[2], "), ", p, ", ", r2)
  fig <- ggplot(dta, aes_string(y = "DX62_ALMI", x = x)) +
    geom_point() +
    geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
    labs(caption = cap) +
    theme(axis.title=element_text(size = rel(.75)))
})
for (i in 1:length(figs)) {
  doc <- read_pptx()
  doc <- add_slide(doc, 'Title and Content', 'Office Theme')
  anyplot <- dml(ggobj = figs[[i]])
  doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
  print(doc, target = file.path(outdir, paste0("fig_page7_", i, ".pptx")))
}
rm(X, figs, i, doc, anyplot)

# Régressions multivariables DX62_ALMI ~ TotalWeightLoss
fits <- list(
  lm(DX62_ALMI ~ TotalWeightLoss_C * NadirWeight_C, dta),
  lm(DX62_ALMI ~ TotalWeightLoss_C * DX03_Age_C, dta),
  lm(DX62_ALMI ~ TotalWeightLoss_C * FollowUpYears_C, dta),
  lm(DX62_ALMI ~ TotalWeightLoss_C + NadirWeight_C +
       FollowUpYears_C + DX03_Age_C, dta),
  lm(DX62_ALMI ~ TotalWeightLoss_C * NadirWeight_C +
       FollowUpYears_C + DX03_Age_C, dta),
  lm(DX62_ALMI ~ TotalWeightLoss_C * (NadirWeight_C +
       FollowUpYears_C + DX03_Age_C), dta)
)
for(i in 1:3) {
  fit <- fits[[i + 3]]
  write_xlsx(list(coefficients = tidy(fit), stats = glance(fit)),
             file.path(outdir, paste0("tbl_page8_", i, ".xlsx")))
}
rm(fits, i, fit)
