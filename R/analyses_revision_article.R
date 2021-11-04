library(car)
library(ggplot2)
library(officer)
library(parallel)
library(readxl)
library(rvg)
library(writexl)

options(mc.cores = detectCores() - 1) 

# Working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Output directory
outdir <- paste0("results/analyses_revision_article_",
                 format(Sys.Date(), "%Y%m%d"))
if (!dir.exists(outdir)) dir.create(outdir)

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
  "DX58_LeanMassTot%",                 "DX58_LeanMassTotPercent",
  "DX62_ALMI (indice de masse maigre appendiculaire ajusté)", "DX62_ALMI",
  "Nadir weight",                      "NadirWeight"
))
colnames(rename_matrix) <- c("Name in xlsx file", "Name in analyses")
dta <- dxa
for (i in 1:nrow(rename_matrix)) {
  names(dta)[names(dta) == rename_matrix[i, 1]] <- rename_matrix[i, 2]
}
rm(rename_matrix, i)
dta$FollowUpYears <- as.numeric(dta$DXA_Date - dxa$`Date Bariatric surgery`)
dta$FollowUpYears <- dta$FollowUpYears / 365.2425
dta$WeightRegain <- (dta$DX05_weight / dta$NadirWeight - 1) * 100
dta$DX03_age_C <- dta$DX03_age - mean(dta$DX03_age)
dta$TotalWeightLoss_C <- dta$TotalWeightLoss - mean(dta$TotalWeightLoss)
dta$NadirWeight_C <- dta$NadirWeight - mean(dta$NadirWeight)
dta$FollowUpYears_C <- dta$FollowUpYears - mean(dta$FollowUpYears)
dta$WeightRegain_C <- dta$WeightRegain - mean(dta$WeightRegain)

# Regression variables
Y <- c("DX58_LeanMassTotPercent", "DX62_ALMI", "DX62_LeanMassIndex")
X <- c("DX03_age", "FollowUpYears", "TotalWeightLoss", "NadirWeight",
       "WeightRegain")

# Univariable regressions
uvreg <- mclapply(setNames(Y, Y), function(y) {
  do.call(rbind, lapply(X, function(x) {
    fml <- as.formula(paste(y, "~", x))
    fit <- do.call("lm", list(formula = fml, data = quote(dta)))
    b <- cbind(coef(fit), suppressMessages(confint(fit)))
    colnames(b)[1] <- "beta"
    pv <- coef(summary(fit))[, 4]
    s <- ifelse(pv > 0.1, NA, ifelse(pv > 0.05, "≤0.1",
           ifelse(pv > 0.01, "≤0.05", ifelse(pv > 0.001, "≤0.01" ,"≤0.001"))))
    s[1] <- NA
    b <- cbind(b, `p-value` = pv, significativity = s)
    na <- rep(NA, nrow(b) - 1)
    r <- data.frame(
      `dependent variable` = c(y, na),
      `number of observations` = c(nrow(fit$model), na),
      coefficient = rownames(b)
    )
    cbind(r, b)
  }))
})
write_xlsx(uvreg, file.path(outdir, "regressions_univariables.xlsx"))

# Univariable regressions - diagnostic plots
pdf(file.path(outdir, "regressions_univariables_diagnostic_plots.pdf"))
for (y in Y) {
  for (x in X) {
    fml <- paste(y, "~", x)
    fit <- lm(as.formula(fml), dta)
    par(mfrow = c(2, 2))
    for (i in 1:4) plot(fit, i)
    par(mfrow = c(1, 1))
    mtext(fml, outer = TRUE, line = -1.8, cex = 1)
  }
}
dev.off()
rm(i, fit, fml, x, y)

# Univariable regressions - figures
uvreg_figs <- mclapply(setNames(Y, Y), function(y) {
  lapply(setNames(X, X), function(x) {
    fml <- as.formula(paste(y, "~", x))
    fit <- do.call("lm", list(formula = fml, data = quote(dta)))
    b <- signif(coef(fit)[[2]], 3)
    ci <- signif(confint(fit)[2, ], 3)
    p <- coef(summary(fit))[2, "Pr(>|t|)" ]
    p <- if (p >= 0.001) paste0("p=", round(p, 3)) else "p<0.001"
    r2 <- paste0("R2=", round(summary(fit)$r.squared, 3))
    cap <- paste0("b = ", b, " (", ci[1], ",", ci[2], "), ", p, ", ", r2)
    fig <- ggplot(dta, aes_string(y = y, x = x)) +
      geom_point() +
      geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
      labs(caption = cap) +
      theme(axis.title=element_text(size = rel(.75)))
  })
})
pdf(file.path(outdir, "regressions_univariables.pdf"))
for (figs in uvreg_figs) {
  for (fig in figs) {
    print(fig)
  }
}
dev.off()
o <- file.path(outdir, "regressions_univariables")
if (!dir.exists(o)) dir.create(o)
for (figs in uvreg_figs) {
  for (fig in figs) {
    f <- paste0(o, "/", fig$labels$y, "_", fig$labels$x)
    f <- paste0(f, c(".tiff", ".pptx"))
    #tiff(f[1], width = 4800, height = 4800, res = 1152, compression = "zip")
    #print(fig)
    #dev.off()
    doc <- read_pptx()
    doc <- add_slide(doc, 'Title and Content', 'Office Theme')
    anyplot <- dml(ggobj = fig)
    doc <- ph_with(doc, anyplot, location = ph_location_fullsize())
    print(doc, target = f[2])
  }
}
rm(figs, fig, o, f, doc, anyplot)

# Multivariable regressions
mvreg <- mclapply(setNames(Y, Y), function(y) {
  X <- paste0(X, "_C")
  fml <- as.formula(paste(y, "~", paste(X, collapse = " + ")))
  fml <- update(fml, . ~ . + TotalWeightLoss_C * NadirWeight_C)
  fit <- do.call("lm", list(formula = fml, data = quote(dta)))
  b <- cbind(coef(fit), suppressMessages(confint(fit)))
  colnames(b)[1] <- "beta"
  pv <- coef(summary(fit))[, 4]
  s <- ifelse(pv > 0.1, NA, ifelse(pv > 0.05, "≤0.1",
         ifelse(pv > 0.01, "≤0.05", ifelse(pv > 0.001, "≤0.01" ,"≤0.001"))))
  s[1] <- NA
  b <- cbind(b, `p-value` = pv, significativity = s)
  b <- cbind(data.frame(coefficient = rownames(b)), b)
  vif <- vif(fit)
  vif <- data.frame(coefficient = names(vif), vif = vif)
  b$dummy_row_number <- 1:nrow(b)
  b <- merge(b, vif, by = "coefficient", all = TRUE)
  b <- b[order(b$dummy_row_number), ]
  b$dummy_row_number <- NULL
  na <- rep(NA, nrow(b) - 1)
  r <- data.frame(
    `dependent variable` = c(y, na),
    `number of observations` = c(nrow(fit$model), na),
    check.names = FALSE
  )
  cbind(r, b)
})
write_xlsx(mvreg, file.path(outdir, "regressions_multivariables.xlsx"))

# Multivariable regressions - diagnostic plots
pdf(file.path(outdir, "regressions_multivariables_diagnostic_plots.pdf"))
for (y in Y) {
  fml <- as.formula(paste(y, "~", paste(paste0(X, "_C"), collapse = " + ")))
  fml <- update(fml, . ~ . + TotalWeightLoss_C * NadirWeight_C)
  fit <- lm(as.formula(fml), dta)
  par(mfrow = c(2, 2))
  for (i in 1:4) plot(fit, i)
  par(mfrow = c(1, 1))
  mtext(fml, outer = TRUE, line = -1.8, cex = 1)
}
dev.off()
rm(i, fit, fml, y)

# Session Info
sink(file.path(outdir, "sessionInfo.txt"))
print(sessionInfo(), locale = FALSE)
sink()
