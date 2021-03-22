#  __  __         _         _      _                 ____ __  __    _
# |  \/  |  __ _ | |_  ___ | |__  (_) _ __    __ _  |  _ \\ \/ /   / \
# | |\/| | / _` || __|/ __|| '_ \ | || '_ \  / _` | | | | |\  /   / _ \
# | |  | || (_| || |_| (__ | | | || || | | || (_| | | |_| |/  \  / ___ \
# |_|  |_| \__,_| \__|\___||_| |_||_||_| |_| \__, | |____//_/\_\/_/   \_\
#                                            |___/

library(readxl)
library(MatchIt)
library(ggplot2)

set.seed(666)
options(width = 120)

# Working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Data
data0 <- read_xlsx("data-raw/OsteoLaus DXA no blank.xlsx")
data0 <- as.data.frame(data0)
data1 <- read_xlsx("data-raw/COOL-OsteoLaus for stat (clean).xlsx",
                   sheet = "OsteoLaus + COOL subject")
data1 <- as.data.frame(data1[data1[[12]] %in% 0, 9:11])
data1$DX03_age <- as.numeric(data1$DX03_age)
data1$DX06_BMI <- as.numeric(data1$DX06_BMI)
coln <- c("id", "age", "bmi")
data <- rbind(cbind(grp = 0, setNames(data0, coln)),
              cbind(grp = 1, setNames(data1, coln)))
data <- data[!is.na(data$age) & !is.na(data$bmi), ]
if (any(is.na(data))) stop("missing values")
rm(data0, data1, coln)

# Apperçu des données
tmp <- data
tmp$grp <- factor(tmp$grp, 0:1, c("OsteoLaus", "DXA"))
bps <- list()
bps$age <- ggplot(tmp, aes(x = factor(grp), y = age)) +
  geom_boxplot() + labs(title = "Age", x = "")
bps$bmi <- ggplot(tmp, aes(x = factor(grp), y = bmi)) +
  geom_boxplot() + labs(title = "BMI", x = "")
rm(tmp)

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
mdir <- "results/matching_20210322/1233_controls"
if (!dir.exists(mdir)) dir.create(mdir, recursive = TRUE)
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
pdf(file.path(mdir, "boxplot_age.pdf"))
print(bps$age)
dev.off()
pdf(file.path(mdir, "boxplot_bmi.pdf"))
print(bps$bmi)
dev.off()
save(mlist, file = file.path(mdir, "mlist.rda"), compress = "xz")
sink("results/matching_sessionInfo_20210322.txt")
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
