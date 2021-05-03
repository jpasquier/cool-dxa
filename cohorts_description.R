library(ggplot2)
library(lme4)
library(lmerTest)
library(parallel)
library(rlang)
library(writexl)

# Set working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Options
options(mc.cores = detectCores())

# Import data
load("data/osteolaus.rda")
load("data/cool.rda")

# Import matching data
load("results/matching_20210322/1233_controls/mlist.rda")

# Definition of groups according to matching
# for(s in names(mlist)) {
#   u0 <- paste0("m_", s)
#   for (r in names(mlist[[s]])) {
#     u <- paste0(u0, "_", r)
#     mm <- mlist[[s]][[r]]$mm
#     cool[[u]] <- cool$Id %in% mm[!is.na(mm$id_ctrl1), "id_exp"]
#     ctrl_ids <- na.omit(do.call(base::c, mm[grep("^id_ctrl", names(mm))]))
#     osteolaus[[u]] <- osteolaus$`ID controles` %in% ctrl_ids
#   }
# }
# rm(ctrl_ids, mm, r, s, u, u0)

# Help function: find variable names in the two datasets
cn <- function(s) {
  s <- tolower(s)
  c(osteolaus = names(osteolaus)[grepl(s, tolower(names(osteolaus)))],
    cool = names(cool)[grepl(s, tolower(names(cool)))])
}

# Rename variables
names(osteolaus)[names(osteolaus) == "ID controles"] <- "Id"
names(osteolaus)[names(osteolaus) == "T2D"] <- "T2D-PostOp"
names(osteolaus)[names(osteolaus) == "HTA"] <- "HTA-PostOp"
names(osteolaus)[names(osteolaus) == "Dyslipidémia"] <- "Dyslipidemia-PostOp"
names(osteolaus)[names(osteolaus) == "TTT_lipid"] <- "TTT_lipid PostOp"
names(osteolaus)[names(osteolaus) == "TTT_DT2"] <- "TTT_DT2 PostOp"
names(osteolaus)[names(osteolaus) == "DX16_FMTot"] <- "DX16_FatMassTot"
names(cool)[grep("DX62_ALMI \\(.+\\)$", names(cool))] <- "DX62_ALMI"

# Rescale variables
osteolaus[["DX49_FatMassAndroide%"]] <-
  osteolaus[["DX49_FatMassAndroide%"]] * 100
osteolaus[["DX50_FatMassGynoide%"]] <-
  osteolaus[["DX50_FatMassGynoide%"]] * 100

# Calculate HOMA in control group
osteolaus$HOMA <- osteolaus$Insul * osteolaus$glyc / 22.5

# Variables to describe and compare
Y <- list(
  list(y = "DX06_BMI", subgrp = "all"),
  list(y = "DX03_age", subgrp = "all"),
  list(y = "T2D-PostOp", subgrp = "all"),
  list(y = "HTA-PostOp", subgrp = "all"),
#  list(y = "Dyslipidemia-PostOp", subgrp = "all"),
  list(y = "TTT_lipid PostOp", subgrp = "all"),
  list(y = "TTT_DT2 PostOp", subgrp = "all"),
  list(y = "glyc", subgrp = "all"),
  list(y = "Insul", subgrp = "all"),
  list(y = "HOMA", subgrp = "all"),
  list(y = "CaCo", subgrp = "all"),
  list(y = "urate", subgrp = "all"),
  list(y = "Chol-t", subgrp = "all"),
  list(y = "LDL-C", subgrp = "all"),
  list(y = "HDL-C", subgrp = "all"),
  list(y = "TG", subgrp = "all"),
  list(y = "TSH", subgrp = "all"),
  list(y = "Albumine", subgrp = "all"),
  list(y = "creat", subgrp = "all"),
  list(y = "ASAT", subgrp = "all"),
  list(y = "ALAT", subgrp = "all"),
  list(y = "GGT", subgrp = "all"),
  list(y = "PA", subgrp = "all"),
#  list(y = "CRP", subgrp = "all"),
  list(y = "PTH", subgrp = "all"),
  list(y = "VitD1", subgrp = "all"),
  list(y = "DX09_FatMass-membre superieur", subgrp = "all"),
  list(y = "DX17_Lean Mass-membre superieur", subgrp = "all"),
  list(y = "DX15_FatMass-tronc", subgrp = "all"),
  list(y = "DX23_lean mass-tronc", subgrp = "all"),
  list(y = "DX68_FM-tronc-pc", subgrp = "all"),
  list(y = "DX66_LM-tronc-pc", subgrp = "all"),
  list(y = "DX16_FatMassTot", subgrp = "all"),
  list(y = "DX24_Lean Mass Tot", subgrp = "all"),
  list(y = "DX38_MassTotale", subgrp = "all"),
  list(y = "DX59_FatMassTotale%", subgrp = "all"),
  list(y = "DX58_LeanMassTot%", subgrp = "all"),
  list(y = "DX41_FatMassAndroid", subgrp = "all"),
  list(y = "DX43_LeanMassAndroid", subgrp = "all"),
  list(y = "DX49_FatMassAndroide%", subgrp = "all"),
  list(y = "DX42_FatMassGynoide", subgrp = "all"),
  list(y = "DX44_LeanMassGynoide", subgrp = "all"),
  list(y = "DX50_FatMassGynoide%", subgrp = "all"),
  #list(y = "Ratio android %fat/gynoid %fat", subgrp = "all"),
  list(y = "DX63_FatMassIndex", subgrp = "all"),
  list(y = "DX62_LeanMassIndex", subgrp = "all"),
  list(y = "DX62_ALMI", subgrp = "all"),
  list(y = "DX55_Tissu adipeux viscéral (VAT)", subgrp = "all"),
  list(y = "DX56_VATvol", subgrp = "all"),
  list(y = "DX52_Contenu minéral osseux", subgrp = "all"),
  list(y = "DX58_LeanMassTot%", subgrp = "age > 60y"),
  list(y = "DX62_ALMI", subgrp = "age > 60y"),
  list(y = "DX55_Tissu adipeux viscéral (VAT)", subgrp = "age > 60y"),
  list(y = "DX24_Lean Mass Tot", subgrp = "age > 60y"),
  list(y = "DX59_FatMassTotale%", subgrp = "age > 60y"),
  list(y = "DX58_LeanMassTot%", subgrp = "surg. >= 50y"),
  list(y = "DX62_ALMI", subgrp = "surg. >= 50y"),
  list(y = "DX55_Tissu adipeux viscéral (VAT)", subgrp = "surg. >= 50y"),
  list(y = "DX24_Lean Mass Tot", subgrp = "surg. >= 50y"),
  list(y = "DX59_FatMassTotale%", subgrp = "surg. >= 50y")
)

# List of matchings
M <- list(c("cs0", "r11"), c("cs0", "r21"), c("cs0", "r31"),
          c("cs1", "r11"), c("cs1", "r21"), c("cs1", "r31"))
M <- setNames(M, sapply(M, paste, collapse = "_"))

# Rename variable for the analyses
renvar <- function(z) {
  z <- gsub("-| ", "_", z)
  z <- gsub("%", "Pt", z)
  z <- gsub("\\(|\\)", "", z)
}
names(osteolaus) <- renvar(names(osteolaus))
names(cool) <- renvar(names(cool))
Y <- lapply(Y, function(y) {y$y <- renvar(y$y); y})

# Results
tbls <- mclapply(M, function(m) {
  Y_cool_only <- c("CRP", "PTH", "VitD1")
  m0 <- paste(m, collapse = "_")
  L <- mclapply(Y, function(y) {
    subgrp <- y$subgrp
    y <- y$y
    mm <- mlist[[m[1]]][[m[2]]]$mm
    i.id <- !is.na(mm$id_ctrl1)
    id_exp <- mm[i.id, "id_exp"]
    id_ctrl <- lapply(grep("^id_ctrl", names(mm)), function(z) mm[i.id, z])
    dta_exp <- cool[match(id_exp, cool$Id),
                    unique(c(y, "DX06_BMI", "DX03_age", "Age_at_surgery"))]
    i_exp <- !is.na(dta_exp[[y]])
    if (subgrp == "age > 60y") {
      i_subgrp <- dta_exp$DX03_age > 60
    } else if (subgrp == "surg. >= 50y") {
      i_subgrp <- dta_exp$Age_at_surgery >= 50
    } else {
      i_subgrp <- rep(TRUE, nrow(dta_exp))
    }
    dta_exp$Age_at_surgery <- NULL
    if (y %in% Y_cool_only) {
      i <- i_exp
      fig <- ggplot(dta_exp[i, ], aes(y = !!sym(y))) +
        geom_boxplot() +
        labs(x = "COOL")
        attr(fig, "var") <- y
      r <- data.frame(
        variable = y, subgroup = subgrp, n = sum(i), mean_osteolaus = NA,
        sd_osteolaus = NA, mean_cool = mean(dta_exp[i, y]), 
        sd_cool = sd(dta_exp[i, y]), diff = NA, diff_se = NA, diff_pv = NA,
        paired_diff = NA, paired_diff_se = NA, paired_diff_pv = NA,
        adj_diff = NA, adj_diff_se = NA, adj_diff_pv = NA,
        adj_paired_diff = NA, adj_paired_diff_se = NA, adj_paired_diff_pv = NA,
        stringsAsFactors = FALSE
      )
      attr(r, "fig") <- fig
      return(r)
    }
    dta_ctrl <- lapply(id_ctrl, function(id) {
      osteolaus[match(id, osteolaus$Id), unique(c(y, "DX06_BMI", "DX03_age"))]
    })
    i_ctrl <- apply(sapply(dta_ctrl, function(z) !is.na(z[[y]])), 1, all)
    i <- i_subgrp & i_ctrl & i_exp
    lg_ctrl <- do.call(rbind, lapply(dta_ctrl, function(dta) {
      cbind(id = 1:sum(i), grp = 0, dta[i, ])
    }))
    lg_exp <- cbind(id = 1:sum(i), grp = 1, dta_exp[i, ])
    lg <- rbind(lg_ctrl, lg_exp)
    fml <- list(list(as.formula(paste(y, "~ grp"))))
    if (y == "DX06_BMI") {
        fml[[1]][[2]] <- update(fml[[1]][[1]], . ~ . + DX03_age)
    } else if (y == "DX03_age") {
        fml[[1]][[2]] <- update(fml[[1]][[1]], . ~ . + DX06_BMI)
    } else {
        fml[[1]][[2]] <- update(fml[[1]][[1]], . ~ . + DX06_BMI + DX03_age)
    }
    fml[[2]] <- lapply(fml[[1]], function(f) update(f, . ~ . + (1 | id)))
    fit <- list(lapply(fml[[1]], lm, data = lg),
                suppressMessages(lapply(fml[[2]], lmer, data = lg)))
    fit <- unlist(fit, recursive = FALSE)
    if (length(id_ctrl) == 1) {
      tt <- t.test(dta_exp[i, y], dta_ctrl[[1]][i, y], paired = TRUE)
      paired_diff <- tt$estimate
      paired_diff_se <- tt$stderr
      paired_diff_pv <- tt$p.value
    } else {
      if (!isSingular(fit[[3]])) {
        paired_diff <- fixef(fit[[3]])[["grp"]]
        paired_diff_se <- coef(summary(fit[[3]]))["grp", "Std. Error"]
        paired_diff_pv <- coef(summary(fit[[3]]))["grp", "Pr(>|t|)"]
      } else {
        paired_diff <- NA
        paired_diff_se <- NA
        paired_diff_pv <- NA
      }
    }
    if (!isSingular(fit[[4]])) {
      adj_paired_diff <- fixef(fit[[4]])[["grp"]]
      adj_paired_diff_se <- coef(summary(fit[[4]]))["grp", "Std. Error"]
      adj_paired_diff_pv <- coef(summary(fit[[4]]))["grp", "Pr(>|t|)"]
    } else {
      adj_paired_diff <- NA
      adj_paired_diff_se <- NA
      adj_paired_diff_pv <- NA
    }
    lg$grp <- factor(lg$grp, 0:1, c("OsteoLaus", "COOL"))
    fig <- ggplot(lg, aes(x = grp, y = !!sym(y))) +
      geom_boxplot() +
      labs(x = "", caption = paste("subgroup:", subgrp))
    attr(fig, "var") <- y
    attr(fig, "subgrp") <- gsub(" |\\.|=|>", "", subgrp)
    r <- data.frame(
      variable           = y,
      subgroup           = subgrp,
      n                  = sum(i),
      mean_osteolaus     = mean(lg_ctrl[[y]]),
      sd_osteolaus       = sd(lg_ctrl[[y]]),
      mean_cool          = mean(lg_exp[[y]]),
      sd_cool            = sd(lg_exp[[y]]),
      diff               = coef(fit[[1]])[["grp"]],
      diff_se            = coef(summary(fit[[1]]))["grp", "Std. Error"],
      diff_pv            = coef(summary(fit[[1]]))["grp", "Pr(>|t|)"],
      paired_diff        = paired_diff,
      paired_diff_se     = paired_diff_se,
      paired_diff_pv     = paired_diff_pv,
      adj_diff           = coef(fit[[2]])[["grp"]],
      adj_diff_se        = coef(summary(fit[[2]]))["grp", "Std. Error"],
      adj_diff_pv        = coef(summary(fit[[2]]))["grp", "Pr(>|t|)"],
      adj_paired_diff    = adj_paired_diff,
      adj_paired_diff_se = adj_paired_diff_se,
      adj_paired_diff_pv = adj_paired_diff_pv,
      stringsAsFactors   = FALSE
    )
    attr(r, "fig") <- fig
    return(r)
  })
  figs <- lapply(L, attr, "fig")
  tbl <- do.call(rbind, L)
  attr(tbl, "figs") <- figs
  return(tbl)
})

# Export results
u <- "results/cohorts_description_20210503"
write_xlsx(tbls, paste0(u, ".xlsx"))
d0 <- paste0(u, "_figs")
if (!dir.exists(d0)) dir.create(d0)
for(m in names(tbls)) {
  d1 <- file.path(d0, m)
  if (!dir.exists(d1)) dir.create(d1)
  figs <- attr(tbls[[m]], "figs")
  for (fig in figs) {
    f <- file.path(d1, paste0(attr(fig, "var"), "_",
                              attr(fig, "subgrp"), ".ps"))
    postscript(f)
    print(fig)
    dev.off()
  }
}
sink(paste0(u, "_sessionInfo.txt"))
print(sessionInfo(), locale = FALSE)
sink()
