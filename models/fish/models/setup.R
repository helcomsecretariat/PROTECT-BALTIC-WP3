library(dplyr)
library(tidyr)

spec_list <- read.csv("../observations/outputs/species_list_SE_FI_PO.csv")

# Order species list by quantity
spec_list <- spec_list %>% arrange(desc(quantity))
spec_list$run_initiated <- 0
spec_list$run_completed <- 0
spec_list$run_time <- 0
spec_list$models <- NA
spec_list$num_presence <- 0
spec_list$num_absence <- 0
spec_list$run_name <- NA
write.csv(spec_list, "spec_list.csv", row.names = FALSE)