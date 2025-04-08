# Load necessary libraries
library(readr)
library(dplyr)

# Read the data with fixed-width formatting
col_positions <- fwf_positions(
  start = c(1, 2, 7, 12, 15, 21, 24, 32, 41, 48, 53),
  end = c(1, 4, 10, 13, 19, 22, 30, 38, 46, 51, 53),
  col_names = c("Company", "TestNumber", "Year", "N_bearings", "P", "Z", "D", "L10", "L50", "Weibull_slope", "Bearing_type")
)
# setwd("/home/greenflame/Desktop/isi/stat/7_Greece_Engineering_3,14,38")
data <- read_fwf("./ballbearings.dat.txt", col_positions = col_positions,
                 col_types = cols(
                   Company = col_factor(levels = c("1", "2", "3")),
                   TestNumber = col_integer(),
                   Year = col_integer(),
                   N_bearings = col_integer(),
                   P = col_number(),
                   Z = col_integer(),
                   D = col_number(),
                   L10 = col_number(),
                   L50 = col_number(),
                   Weibull_slope = col_number(),
                   Bearing_type = col_factor(levels = c("0", "1", "2", "3"))
                 ))

# Create log-transformed variables
data <- data %>%
  mutate(
    ln_L10 = log(L10),
    ln_L50 = log(L50),
    ln_P = log(P),
    ln_Z = log(Z),
    ln_D = log(D)
  )

# Create dummy variables for Companies B and C
data$CompanyB <- ifelse(data$Company == "2", 1, 0)
data$CompanyC <- ifelse(data$Company == "3", 1, 0)

# Subset data for Company B and create bearing type dummies
data_companyB <- data %>%
  filter(Company == "2") %>%
  mutate(
    Type1 = ifelse(Bearing_type == "1", 1, 0),
    Type2 = ifelse(Bearing_type == "2", 1, 0),
    Type3 = ifelse(Bearing_type == "3", 1, 0)
  )

# Define weights (number of bearings)
weights <- data$N_bearings
#plot(data$L10,data$N_bearings,cex=0.0001)
# Analysis for Table 1: Tests between companies (using L10 as example)
# Full model with all interactions (different parameters per company)
full_model <- lm(ln_L10 ~ Company * (ln_Z + ln_D + ln_P), data = data, weights = weights)

# Reduced model (same parameters for all companies)
reduced_model <- lm(ln_L10 ~ ln_Z + ln_D + ln_P, data = data, weights = weights)

# F-test for hypothesis (a)
anova_result_a <- anova( reduced_model,full_model)
anova_result_a

# Model with common p (only ln_P coefficient same across companies)
common_p_model <- lm(ln_L10 ~ Company * (ln_Z + ln_D) + ln_P ,       data = data, weights = weights)
#common_p_model <- lm(ln_L10 ~  (ln_Z + ln_D + ln_P) , data = data, weights = weights)

# F-test for hypothesis (b)
anova_result_b <- anova(common_p_model, full_model)
anova_result_b

# Analysis for Table 2: Tests within Company B (using L10)
# Full model with interactions for bearing type
full_model_B <- lm(ln_L10 ~ Bearing_type * (ln_Z + ln_D + ln_P), data = data_companyB, weights = N_bearings)

# Reduced model (same parameters for all types)
reduced_model_B <- lm(ln_L10 ~ ln_Z + ln_D + ln_P, data = data_companyB, weights = N_bearings)

# F-test for hypothesis (c)
anova_result_c <- anova(reduced_model_B, full_model_B)
anova_result_c
# Model with common p within Company B
common_p_model_B <- lm(ln_L10 ~  + Bearing_type * (ln_Z + ln_D) + ln_P,
                       data = data_companyB, weights = N_bearings)

# F-test for hypothesis (d)
anova_result_d <- anova(full_model_B,common_p_model_B)
anova_result_d

data_companyB$ln_Z=data_companyB$ln_Z*3
data_companyB$ln_D=data_companyB$ln_D*3
data_companyB$ln_P=data_companyB$ln_P*-3
data_companyB$ln_L10=data_companyB$ln_L10-data_companyB$ln_P
p_3_model_B<- lm(ln_L10 ~  + Bearing_type * (ln_Z + ln_D),
                       data = data_companyB, weights = N_bearings)
summary(p_3_model_B)
anova(p_3_model_B,full_model_B)
