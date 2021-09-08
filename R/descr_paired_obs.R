library(writexl)

# Set working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Import data
load("data/cool.rda")

# Import matching data
load("results/matching_20210322/1233_controls/mlist.rda")
mm <- mlist$cs1$r11$mm

# Subset of paired observation
paired <- cool[cool$Id %in% mm[!is.na(mm$id_ctrl1), "id_exp"], ]

# Descriptive analyses - continuous variables
V <- c("weight_preOp", "Follow-up DXA months", "weight loss kg",
       "Total weight loss (TWL)", "Excès weight loss", "Nadir weight",
       "Time Nadir weight (months)")
tbl_num <- do.call(rbind, lapply(V, function(v) {
  x <- na.omit(paired[[v]])
  data.frame(
    variable = v,
    n = length(x),
    mean = mean(x),
    sd = sd(x),
    median = median(x),
    q25 = quantile(x, 0.25),
    q75 = quantile(x, 0.75),
    min = min(x),
    max = max(x)
  )
}))
rownames(tbl_num) <- NULL
rm(V)

# Descriptive analyses - binary variables
V <- c("T2D-PreOp", "HTA-PreOp", "Dyslipidemia-PreOp", "T2D-PostOp",
       "HTA-PostOp", "Dyslipidemia-PostOp", "TTT_DT2 PostOp",
       "TTT_lipid PostOp")
tbl_bin <- do.call(rbind, lapply(V, function(v) {
  x <- na.omit(paired[[v]])
  if (!all(is.na(x) | x %in% 0:1)) stop(paste(v, "not binary"))
  data.frame(
    variable = v,
    n = length(x),
    npos = sum(x),
    `prop (%)` = mean(x) * 100,
    check.names = FALSE
  )
}))
rownames(tbl_bin) <- NULL
rm(V)

# Export tables
write_xlsx(list(numeric = tbl_num, binary = tbl_bin),
           "results/descr_paired_obs_20210906.xlsx")
