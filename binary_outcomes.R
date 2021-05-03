library(parallel)
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
for(s in names(mlist)) {
  u0 <- paste0("m_", s)
  for (r in names(mlist[[s]])) {
    u <- paste0(u0, "_", r)
    mm <- mlist[[s]][[r]]$mm
    cool[[u]] <- cool$Id %in% mm[!is.na(mm$id_ctrl1), "id_exp"]
    ctrl_ids <- na.omit(do.call(base::c, mm[grep("^id_ctrl", names(mm))]))
    osteolaus[[u]] <- osteolaus$`ID controles` %in% ctrl_ids
  }
}
rm(ctrl_ids, mm, r, s, u, u0)

# Rename variables
names(osteolaus)[names(osteolaus) == "ID controles"] <- "Id"
names(osteolaus)[names(osteolaus) == "T2D"] <- "T2D-PostOp"
names(osteolaus)[names(osteolaus) == "HTA"] <- "HTA-PostOp"
names(osteolaus)[names(osteolaus) == "Dyslipidémia"] <- "Dyslipidemia-PostOp"
names(osteolaus)[names(osteolaus) == "TTT_lipid"] <- "TTT_lipid PostOp"
names(osteolaus)[names(osteolaus) == "TTT_DT2"] <- "TTT_DT2 PostOp"
names(cool)[grep("DX62_ALMI \\(.+\\)$", names(cool))] <- "DX62_ALMI"

# Sarcopenia
cool$Sarcopenia <- as.integer(cool$DX62_ALMI < 5.67)
osteolaus$Sarcopenia <- as.integer(osteolaus$DX62_ALMI < 5.67)


# List of matchings
M <- list(c("cs0", "r11"), c("cs0", "r21"), c("cs0", "r31"),
          c("cs1", "r11"), c("cs1", "r21"), c("cs1", "r31"))
M <- setNames(M, sapply(M, paste, collapse = "_"))

# Variables to compare
Y <- c("T2D-PostOp", "HTA-PostOp", "Dyslipidemia-PostOp", "TTT_lipid PostOp",
       "TTT_DT2 PostOp", "Sarcopenia")

# Rename variable for the analyses
renvar <- function(z) {
  z <- gsub("-| ", "_", z)
  z <- gsub("%", "Pt", z)
  z <- gsub("\\(|\\)", "", z)
}
names(osteolaus) <- renvar(names(osteolaus))
names(cool) <- renvar(names(cool))
Y <- renvar(Y)

# Results
tbls <- mclapply(M, function(m) {
  do.call(rbind, mclapply(Y, function(y) {
    mm <- mlist[[m[1]]][[m[2]]]$mm
    i.id <- !is.na(mm$id_ctrl1)
    id_exp <- mm[i.id, "id_exp"]
    id_ctrl <- lapply(grep("^id_ctrl", names(mm)), function(z) mm[i.id, z])
    dta_exp <- cool[match(id_exp, cool$Id), y, drop = FALSE]
    i_exp <- !is.na(dta_exp[[y]])
    dta_ctrl <- lapply(id_ctrl, function(id) {
      osteolaus[match(id, osteolaus$Id), y, drop = FALSE]
    })
    i_ctrl <- apply(sapply(dta_ctrl, function(z) !is.na(z[[y]])), 1, all)
    i <- i_ctrl & i_exp
    lg_ctrl <- do.call(rbind, lapply(dta_ctrl, function(dta) {
      cbind(id = 1:sum(i), grp = 0, dta[i, , drop = FALSE])
    }))
    lg_exp <- cbind(id = 1:sum(i), grp = 1, dta_exp[i, , drop = FALSE])
    lg <- rbind(lg_ctrl, lg_exp)
    chisq_pv <- suppressWarnings(chisq.test(lg[[y]], lg$grp)$p.value)
    chisq_warn <- tryCatch(chisq.test(lg[[y]], lg$grp)$p.value,
                           warning = conditionMessage)
    if (class(chisq_warn) == "numeric") chisq_warn <- NA
    data.frame(
      variable           = y,
      n                  = sum(i),
      n_pos_osteolaus    = sum(lg_ctrl[[y]]),
      prop_pos_osteolaus = mean(lg_ctrl[[y]]),
      n_pos_cool         = sum(lg_exp[[y]]),
      prop_pos_cool      = mean(lg_exp[[y]]),
      fisher_pval        = fisher.test(lg[[y]], lg$grp)$p.value,
      chisq_pval         = chisq_pv,
      chisq_warning      = chisq_warn,
      stringsAsFactors   = FALSE
    )
  }))
})

# Export results
u <- "results/binary_variables_20210503"
write_xlsx(tbls, paste0(u, ".xlsx"))
sink(paste0(u, "_sessionInfo.txt"))
print(sessionInfo(), locale = FALSE)
sink()
rm(u)

