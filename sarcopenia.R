# Set working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Import data
load("data/osteolaus.rda")
load("data/cool.rda")

# Rename variables
names(osteolaus)[names(osteolaus) == "ID controles"] <- "Id"
names(cool)[grep("DX62_ALMI \\(.+\\)$", names(cool))] <- "DX62_ALMI"

# Import matching data
load("results/matching_20210322/1233_controls/mlist.rda")

# List of matchings
M <- list(c("cs0", "r11"), c("cs0", "r21"), c("cs0", "r31"),
          c("cs1", "r11"), c("cs1", "r21"), c("cs1", "r31"))
M <- setNames(M, sapply(M, paste, collapse = "_"))

# Compare sarcopenia
sink("results/sarcopenia_20210421.txt")
for (m in M) {
  m0 <- paste(m, collapse = "_")
  cat(paste0("## Matching: ", m0, "\n\n"))
  mm <- mlist[[m[1]]][[m[2]]]$mm
  i.id <- !is.na(mm$id_ctrl1)
  id_exp <- mm[i.id, "id_exp"]
  id_ctrl <- lapply(grep("^id_ctrl", names(mm)), function(z) mm[i.id, z])
  dta_exp <- cool[match(id_exp, cool$Id),
                  c("DX62_ALMI", "DX06_BMI", "DX03_age")]
  i_exp <- !is.na(dta_exp$DX62_ALMI)
  dta_ctrl <- lapply(id_ctrl, function(id) {
    osteolaus[match(id, osteolaus$Id), 
              c("DX62_ALMI", "DX06_BMI", "DX03_age")]
  })
  i_ctrl <- apply(sapply(dta_ctrl, function(z) !is.na(z$DX62_ALMI)), 1, all)
  i <- i_ctrl & i_exp
  lg_ctrl <- do.call(rbind, lapply(dta_ctrl, function(dta) {
    cbind(id = as.integer(1:sum(i)), Grp = "OsteoLaus", dta[i, ])
  }))
  lg_exp <- cbind(id = as.integer(1:sum(i)), Grp = "COOL", dta_exp[i, ])
  lg <- rbind(lg_ctrl, lg_exp)
  lg$Grp <- factor(lg$Grp, c("OsteoLaus", "COOL"))
  lg$Sarcopenia <- as.integer(lg$DX62_ALMI < 5.67)
  
  tbl_srcp <- with(lg, table(Sarcopenia, Grp))
  cat("### Count and proportion tables\n\n")
  print(tbl_srcp)
  print(prop.table(tbl_srcp, 2))
  cat("\n\n### Fisher's test\n\n")
  print(fisher.test(tbl_srcp))
  if (length(id_ctrl) == 1) {
    lg$S2 <- ifelse(lg$Grp == "COOL", 2, 1) * lg$Sarcopenia
    x0 <- table(aggregate(S2 ~ id, lg, sum)[2])
    x <- setNames(rep(0, 4), 0:3)
    x[names(x0)] <- x0
    x <- cbind(x[1:2], x[3:4])
    rownames(x) <- c("OsteoLaus---", "OsteoLaus+++")
    colnames(x) <- c("COOL---", "COOL+++")
    cat("### Matched table\n\n")
    print(x)
    tbl_srcp_matched <- x
    cat("\n\n### McNemar's test\n\n")
    print(mcnemar.test(tbl_srcp_matched))
  }
  cat("\n------------------------------------\n\n\n")
}
sink()
