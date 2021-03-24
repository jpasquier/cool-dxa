library(readxl)
library(writexl)

# Set working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Import data
labo <- read_xlsx("data-raw/OsteoLaus Données (labo 1 year).xlsx", "labo")
dxa <- read_xlsx("data-raw/OsteoLaus Données (labo 1 year).xlsx", "DXA")

# Rename variables
all(dxa$`DX09_FatMass-membre superieur...7` == 
  dxa$`DX09_FatMass-membre superieur...37`)
names(dxa)[names(dxa) == "DX09_FatMass-membre superieur...7"] <-
  "DX09_FatMass-membre superieur"
names(dxa)[names(dxa) == "DX09_FatMass-membre superieur...37"] <-
  "DX09_FatMass-membre superieur (2)"

# Recoding
labo$V3_exam_DATE <- as.Date(labo$V3_exam_DATE)
labo$V3_SCAN_DATE <- as.Date(labo$V3_SCAN_DATE)
dxa$V3_SCAN_DATE <- as.Date(dxa$V3_SCAN_DATE)

# Merge tables
osteolaus <- merge(labo, dxa, by = "ID controles", all = TRUE,
                   suffixes = c(" (labo)", " (dxa)"))

# Differences
grep("\\((labo|dxa)\\)$", names(osteolaus), value = TRUE)
osteolaus[1:6, c("DX03_age (labo)", "DX03_age (dxa)")]
osteolaus[21:26, c("V3_SCAN_DATE (labo)", "V3_SCAN_DATE (dxa)")]

# Export
write_xlsx(list(osteolaus = osteolaus), "~/osteolaus_data.xlsx")
