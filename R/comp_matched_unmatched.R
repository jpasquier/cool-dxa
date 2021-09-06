library(writexl)
library(pander)

# Set working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Import data
load("data/cool.rda")

# Import matching data
load("results/matching_20210322/1233_controls/mlist.rda")

# 
tmp <- mlist$cs1$r11$mm
tmp$matched <- !is.na(tmp$id_ctrl1)
cool <- merge(cool, tmp[c("id_exp", "matched")], by.x = "Id", by.y = "id_exp")

# Variables
X1 <- c("weight_preOp", "Follow-up DXA months", "weight loss kg",
        "Total weight loss (TWL)", "Excès weight loss", "Nadir weight",
        "Time Nadir weight (months)", "PTH", "VitD1")
X2 <- c("T2D-PreOp", "HTA-PreOp", "Dyslipidemia-PreOp", "T2D-PostOp",
        "HTA-PostOp", "Dyslipidemia-PostOp")

# Rename variable for the analyses
renvar <- function(z) {
  z <- gsub("-| ", "_", z)
  z <- gsub("%", "Pt", z)
  z <- gsub("\\(|\\)", "", z)
}
names(cool) <- renvar(names(cool))
X1 <- renvar(X1)
X2 <- renvar(X2)

# Numeric variables
tbl_num <- do.call(rbind, lapply(X1, function(x) {
  u0 <- cool[[x]][!is.na(cool[[x]]) & !cool$matched]
  u1 <- cool[[x]][!is.na(cool[[x]]) & cool$matched]
  u <- c(u0, u1)
  n <- length
  fcts <- c("n", "mean", "sd")
  fcts <- setNames(fcts, fcts)
  Z <- list(all = u, unmatched = u0, matched = u1)
  r <- do.call(c, lapply(names(Z), function(z) {
    r <- sapply(fcts, function(fct) get(fct)(Z[[z]]))
    names(r) <- paste(names(r), z, sep = ".")
    r
  }))
  pv <- t.test(u0, u1)$p.value 
  cbind(data.frame(variable = x), t(r), ttest.pval = pv)
}))

# Binary variables
tbl_bin <- do.call(rbind, lapply(X2, function(x) {
  u0 <- cool[[x]][!is.na(cool[[x]]) & !cool$matched]
  u1 <- cool[[x]][!is.na(cool[[x]]) & cool$matched]
  u <- c(u0, u1)
  fcts <- setNames(list(length, sum, mean), c("n", "npos", "prop"))
  Z <- list(all = u, unmatched = u0, matched = u1)
  r <- do.call(c, lapply(names(Z), function(z) {
    r <- sapply(fcts, function(fct) fct(Z[[z]]))
    names(r) <- paste(names(r), z, sep = ".")
    r
  }))
  tbl <- cbind(table(u0), table(u1))
  e <- evals("chisq.test(tbl)$p.value", env = environment(),
             graph.dir = "/tmp/r_pander_graphdir")[[1]]
  pv1 <- e$result
  wrn <- e$msg$warnings
  if (is.null(wrn)) wrn = ""
  pv2 <- fisher.test(tbl)$p.value
  cbind(data.frame(variable = x), t(r), chisq.pval = pv1, chisq.warn = wrn,
        fisher.pval = pv2)
}))

# Export results
write_xlsx(list(numeric = tbl_num, binary = tbl_bin),
           "results/comp_matched_unmatched_20210722.xlsx")
