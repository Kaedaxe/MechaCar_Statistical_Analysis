# Import dplyr
library(dplyr)

#1
# Read CSV
mechCar_df <- read.csv(file = 'MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)

# Generate mechCar_df linear model summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechCar_df))


#2
# Read CSV
susCoil_df <- read.csv(file = 'Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

# Generate susCoil_df summary
sc_summary <- susCoil_df %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

#3
# Run overall T-Test
t.test(susCoil_df$PSI, mu = 1500)

# Create subsets for lots
lot1 <- subset(susCoil_df, Manufacturing_Lot == "Lot1")
lot2 <- subset(susCoil_df, Manufacturing_Lot == "Lot2")
lot3 <- subset(susCoil_df, Manufacturing_Lot == "Lot3")

# Run subset T-Tests
t.test(lot1$PSI, mu = 1500)
t.test(lot2$PSI, mu = 1500)
t.test(lot3$PSI, mu = 1500)
