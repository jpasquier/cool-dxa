library(readxl)
library(writexl)

# Set working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Import data
file_name <- "data-raw/All controles OsteoLaus et CoLaus (2).xlsx"
dxa <- read_xlsx(file_name, sheet = "DXA données no blanck")
labo <- read_xlsx(file_name, sheet = "Labo données tot")
rm(file_name)

# Recoding
dxa$V3_SCAN_DATE <- as.Date(dxa$V3_SCAN_DATE)
labo$V3_exam_DATE <- as.Date(labo$V3_exam_DATE)

# Merge tables
all(labo$`ID controles` %in% dxa$`ID controles`)
osteolaus <- merge(dxa, labo, by = "ID controles", all = TRUE,
                   suffixes = c("", " (labo)"))

# Select observations
osteolaus$labo_within_one_year_from_scan <-
  with(osteolaus, abs(V3_SCAN_DATE - V3_exam_DATE) <= 365)

# Export
if (FALSE) write_xlsx(list(osteolaus = osteolaus), "~/osteolaus_data.xlsx")
if (!dir.exists("data")) dir.create("data")
save(osteolaus, file = "data/osteolaus.rda", compress = "xz")
