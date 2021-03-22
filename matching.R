library(readxl)
library(MatchIt)

set.seed(666)
options(width = 120)

# Working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Data
data0 <- read_xlsx("data-raw/COOL-OsteoLaus for stat (clean).xlsx",
                   sheet = "OsteoLaus + COOL subject")
data0 <- as.data.frame(data0)
data0$DX03_age <- as.numeric(data0$DX03_age)
data0$DX06_BMI <- as.numeric(data0$DX06_BMI)
coln <- c("id", "age", "bmi", "oh")
data <- rbind(cbind(grp = 0, setNames(data0[1:4], coln)),
              cbind(grp = 1, setNames(data0[9:12], coln)))
data <- data[!is.na(data$age) & !is.na(data$bmi), ]
data <- data[!with(data, grp == 1 & !is.na(oh) & oh == 1), ]
sapply(data, function(x) sum(is.na(x)))
rm(coln)

# Matching
mlist <- lapply(c(cs0 = 1, cs1 = 2), function(l) {
  lapply(c(r11 = 1, r21 = 2, r31 = 3), function(r) {
    dis <- c("none", "both")[l]
    m <- matchit(grp ~ age + bmi, method = "optimal", data = data,
                 ratio = r, discard = dis)
    v <- c("id", "age", "bmi")
    mm <- cbind(data[rownames(m$match.matrix), v],
                ps = m$distance[rownames(m$match.matrix)])
    names(mm) <- paste0(names(mm), "_exp")
    for (u in 1:r) {
      mm1 <- cbind(data[m$match.matrix[, u], v],
                   ps = m$distance[m$match.matrix[, u]])
      names(mm1) <- paste0(names(mm1), "_ctrl", u)
      mm <- cbind(mm, mm1)
    }
    list(m = m, mm = mm)
  })
})

# Nombre de contrôles à inclure si on veut considérer tous les appariements
ctrl_ids <- c()
for (cs in names(mlist)) {
  for (r in names(mlist[[cs]])) {
    mm <- mlist[[cs]][[r]]$mm
    ctrl_ids <-
      c(ctrl_ids, na.omit(do.call(c, mm[grep("^id_ctrl", names(mm))])))
  }
}
ctrl_ids <- unique(ctrl_ids)
length(ctrl_ids)

# Export results
mdir <- "results/matching_20210211"
if (!dir.exists(mdir)) dir.create(mdir)
for (cs in names(mlist)) {
  for (r in names(mlist[[cs]])) {
    m <- mlist[[cs]][[r]]$m
    mm <- mlist[[cs]][[r]]$mm
    s <- paste(cs, r, sep = "_")
    sink(file.path(mdir, paste0("match_smy_", s, ".txt")))
    print(summary(m))
    sink()
    pdf(file.path(mdir, paste0("match_figs_", s, ".pdf")))
    plot(m, interactive = FALSE)
    plot(m, "ecdf", interactive = FALSE)
    plot(m, type = "jitter", interactive = FALSE)
    plot(m, type = "hist")
    dev.off()
    write.table(mm, file.path(mdir, paste0("matching_matrix_", s, ".csv")),
                sep = ";", row.names = FALSE)
  }
}
write.table(data.frame(`OsteoLaus ID` = ctrl_ids), row.names = FALSE,
            quote = FALSE, file = file.path(mdir, "matched_osteolaus_ids.csv"))
save(mlist, file = file.path(mdir, "mlist.rda"), compress = "xz")
sink("results/matching_sessionInfo_20210211.R")
print(sessionInfo(), locale = FALSE)
sink()

# --------------------------------------------------------------------------- #

fit <- glm(grp ~ age + bmi, family = binomial, data = data)
summary(fit)
data$ps <- predict(fit, type = "response")
sapply(mlist, function(z) {
  sapply(z, function(w) {
           max(abs(data$ps - w$m$distance))
  })
})
boxplot(ps~factor(grp), data)
sort(data[data$grp == 0, "ps"], decreasing = TRUE)[1:10]
