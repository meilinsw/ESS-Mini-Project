install.packages("tidyverse")
install.packages("haven") 
install.packages("dplyr")

library(tidyverse)
library(dplyr)
library(haven)

installed.packages()[, "Package"]

library(tidyverse)
library(dplyr)
library(haven)

setwd("/Users/MeilinWeathington/Downloads/ESS11-subset-4")
getwd()

ess_data <- read.csv("ESS11-subset.csv")
head(ess_data)
str(ess_data)

ess_data$brncntr <- ifelse(ess_data$brncntr == 2, 0, 1)
ess_data$gndr <- ifelse(ess_data$gndr == 2, 1, 0)
table(ess_data$brncntr)
table(ess_data$gndr)

summary(ess_data)

library(tidyverse)

ess_data <- ess_data %>%
  filter(lrscale <= 10 | is.na(lrscale),
         imbgeco <= 10 | is.na(imbgeco),
         hinctnta <= 10 | is.na(hinctnta),
         iphlppla <= 10 | is.na(iphlppla),
         !(eduyrs %in% c(38, 40, 44, 50, 77, 88, 99)) | is.na(eduyrs))
sapply(ess_data[, c("lrscale", "imbgeco", "hinctnta", "iphlppla", "eduyrs")], max, na.rm = TRUE)

summary(ess_data$eduyrs)

library(tidyverse)

ess_data$cntry <- as.character(ess_data$cntry)
results <- list()
for (country in unique(ess_data$cntry)) {
  country_data <- filter(ess_data, cntry == country)
  cat("Running model for:", country, "- Observations:", nrow(country_data), "\n")
  if (nrow(country_data) > 10) {  
    model <- lm(imbgeco ~ gincdif + trstprl + lrscale + brncntr + gndr + eduyrs + hinctnta + iphlppla,
                data = country_data)
    results[[country]] <- summary(model)
  }
}
print(names(results))
print(results[["HU"]])

sink("country_regression_results.txt")
print(results)
sink()

getwd()
file.show("country_regression_results.txt")