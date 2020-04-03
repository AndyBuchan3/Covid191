install.packages("readxl")
#these libraries are necessary
library(readxl)

# Downloaded file must be placed in R working directory and have an XLSX extension (even though it is a CSV)
tf <- "covid.xlsx"
world_data <- read_excel(tf)


# Save data for later
save(world_data, file = "covid19/rdas/covid19.rda")
