library(readxl)

# Set working directory
setwd("~/Projects/Consultations/Favre Lucie (DXA)")

# Help function(s)
`%==%` <- function(x, y) !is.na(x) & !is.na(y) & x == y | is.na(x) & is.na(y)

# Import data
cool <- read_xlsx("data-raw/COOL-OsteoLaus for stat (clean) (2).xlsx",
                   sheet = "COOL data", range = cell_rows(1:61))
cool <- as.data.frame(cool)

# Rename variables
names(cool)[names(cool) == "Follow-up DXA Years...17"] <- "Follow-up DXA Years"
names(cool)[names(cool) == "Follow-up DXA Years...19"] <-
  "Follow-up DXA Years (rounded)"
if (all(cool$PTH...43 %==% cool$PTH...117)) {
  names(cool)[names(cool) == "PTH...43"] <- "PTH"
  cool$PTH...117 <- NULL
}
cool$...131 <- NULL

# Recode variables
for (v in names(cool)) {
  x <- cool[[v]]
  if (is.character(x)) {
    x <- trimws(x)
    if (all(is.na(x) | grepl("^[0-9]+((\\.|,)[0-9]+)?$", x))) {
      x <- sub(",", "\\.", x)
      x <- as.numeric(x)
    }
    cool[[v]] <- x
  }
}
rm(v, x)
cool$DXA_Date <- as.Date(cool$DXA_Date, format = "%d/%m/%Y")
cool$`Date Bariatric surgery` <- 
  as.Date(cool$`Date Bariatric surgery`, format = "%d/%m/%Y")

# Save data
save(cool, file = "data/cool.rda", compress = "xz")
