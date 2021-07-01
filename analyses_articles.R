library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)

# Working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Import data
dxa <- read_xlsx("data-raw/COOL-OsteoLaus for stat (clean) (2).xlsx",
                 sheet = "COOL data", range = cell_rows(1:61))
dxa <- as.data.frame(dxa)
dxa <- dxa[!(dxa$Id %in% c("Dx00000255", "Dx00001488")), ]

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
dta$DX03_age_C <- dta$DX03_age - mean(dta$DX03_age)
dta$TotalWeightLoss_C <- dta$TotalWeightLoss - mean(dta$TotalWeightLoss)
dta$ExcessWeightLoss_C <- dta$ExcessWeightLoss - mean(dta$ExcessWeightLoss)
dta$NadirWeight_C <- dta$NadirWeight - mean(dta$NadirWeight)
dta$FollowUpYears_C <- dta$FollowUpYears - mean(dta$FollowUpYears)
dta$DX59_FatMassTotPercent_C <-
  dta$DX59_FatMassTotPercent - mean(dta$DX59_FatMassTotPercent)

# Remission rate
with(dta, table(T2DPreOp, T2DPostOp))
with(dta, table(HTAPreOp, HTAPostOp))
with(dta, table(DyslipidemiaPreOp, DyslipidemiaPostOp))

# Tables of multivariable regressions
fit1 <- lm(DX58_LeanMassTotPercent ~ TotalWeightLoss_C * NadirWeight_C +
             FollowUpYears_C + DX03_age_C, dta)
fit2 <- lm(DX62_ALMI ~ TotalWeightLoss_C * NadirWeight_C +
             FollowUpYears_C + DX03_age_C, dta)
tab <- function(fit) {
  ci <- round(confint(fit), 3) %>% {paste(.[, 1], .[, 2], sep = " ; ")}
  pv <- coef(summary(fit))[, 4] %>% {ifelse(. < 0.001, "<.001", round(., 3))}
  as.data.frame(cbind(names(coef(fit)), b = round(coef(fit), 3), ci, pv))
}
tab(fit1) %>% write_xlsx("~/tmp_fit1.xlsx")
tab(fit2) %>% write_xlsx("~/tmp_fit2.xlsx")

# VAT
list(
  fct_list = c("n", "mean", "sd", "min", "quantile25", "median", "quantile75",
               "max"),
  Merge = function(u, v) merge(u, v, by = "group", all = TRUE, sort = FALSE)
) %>% 
  {Reduce(.[[2]], lapply(.[[1]], function(fct) {
    n <- function(x, na.rm) sum(!is.na(x))
    quantile25 <- function(x, na.rm) quantile(x, 0.25, na.rm = na.rm)
    quantile75 <- function(x, na.rm) quantile(x, 0.75, na.rm = na.rm)
    z0 <- data.frame("All", get(fct)(dta$DX55_VAT, na.rm = TRUE))
    z1 <- aggregate(DX55_VAT ~ T2DPreOp, dta, get(fct), na.rm = TRUE)
    z1[[1]] <- c("No T2D", "T2D")[z1[[1]] + 1]
    names(z0) <- c("group", fct)
    names(z1) <- c("group", fct)
    rbind(z0, z1)
  }))} %>%
  write_xlsx("~/VAT_T2D_table.xlsx")
p <- dta %>%
  mutate(T2D = factor(T2DPreOp, 0:1, c("No T2D", "T2D"))) %>%
  ggplot(aes(x = T2D, y = DX55_VAT)) +
  geom_boxplot() +
  labs(x = "", y = "VAT (g)")

# VAT - Figures
jpeg("~/VAT_T2D_figure.jpg", width = 4800, height = 4800, res = 900)
print(p)
dev.off()
tiff("~/VAT_T2D_figure.tiff", width = 4800, height = 4800, res = 900,
     compression = "zip")
print(p)
dev.off()

# VAT - t-test with equal variance
t.test(DX55_VAT ~ T2DPreOp, dta, var.equal = TRUE)
t.test(DX55_VAT ~ T2DPostOp, dta, var.equal = TRUE)

# VAT - regressions
summary(lm(DX55_VAT ~ T2DPreOp + DX59_FatMassTotPercent, dta))
summary(lm(DX55_VAT ~ T2DPostOp + DX59_FatMassTotPercent, dta))
