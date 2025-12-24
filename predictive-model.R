library(dplyr)
library(scales)

model_df <- df2 %>%
  select(Business.ID, Business, `Accredited.`, Size, Employees, Customers,
         Revenue, County, Rating, Joined, NAICS_2digit, NAICS.Text, Dropped) %>%
  mutate(Accredited = ifelse(tolower(as.character(`Accredited.`)) == "true", 1, 0),
         Employees  = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(Employees)))),
         Customers  = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(Customers)))),
         Revenue    = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(Revenue)))),
         Size         = as.factor(Size),
         County       = as.factor(County),
         Rating       = as.factor(Rating),
         NAICS_2digit = as.factor(NAICS_2digit),
         Joined       =  parse_joined(df2$Joined),
         Joined_Year  = as.numeric(format(Joined, "%Y")), # Extract year only
         Dropped      = as.numeric(Dropped)) %>%
  select(-`Accredited.`, -Joined) %>%     # Remove redundant columns
  filter(!is.na(Dropped)) 


# Define predictor variable names for modeling
score_vars <- c("Size","Employees","Customers","Revenue",
                "County","Rating","Joined_Year","NAICS_2digit")


# Format model data for logistic regression
model_data <- model_df %>%
  select(Business.ID, Business, Accredited, Dropped, NAICS.Text,all_of(score_vars)) %>%
  mutate(
    Size         = factor(Size),
    County       = factor(County),
    Rating       = factor(Rating),
    NAICS_2digit = factor(NAICS_2digit),
    Employees    = suppressWarnings(as.numeric(Employees)),
    Customers    = suppressWarnings(as.numeric(Customers)),
    Revenue      = suppressWarnings(as.numeric(Revenue))
  ) %>%
  tidyr::drop_na(Dropped, all_of(score_vars)) # Remove rows with missing predictors


# Logistic regression predicting likelihood of business dropping
drop_model <- glm(
  Dropped ~ Size + Employees + Customers + Revenue +
    County + Rating + Joined_Year + NAICS_2digit,
  data = model_data, family = binomial
)

# Predict probability of dropping for each business
model_data$pred_prob <- predict(drop_model, type = "response")
model_data$pred_class <- ifelse(model_data$pred_prob >= 0.5, 1, 0)
# Find accuracy of model
conf_mat <- table(
  Predicted = model_data$pred_class,
  Actual = model_data$Dropped
)
conf_mat
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy

# Create risk table with predicted probabilities and risk bands
risk_table <- model_data %>%
  select(Business.ID, Business, County, Rating, Size, Employees, Revenue,
         Joined_Year, NAICS_2digit, NAICS.Text, Accredited, Dropped, pred_prob) %>%
  filter(Dropped==0)%>% # Focus on active members
  arrange(desc(pred_prob)) %>% # Sort by highest risk
  mutate(
    RiskBand = cut(
      pred_prob,
      breaks = c(-Inf, 0.50, 0.65, 0.80, 0.90, Inf),
      labels = c("Low", "Guarded", "Elevated", "High", "Critical")
    )
  )


# Check counts by risk band
table(risk_table$RiskBand)


#- Revenue at Risk from predicted drop probabilities-
# Assumptions from business rules:
# Micro: $300–$500
# Small: $500–$800
# Medium: $800–$1,500
# Large & above (Large, Giant, Mega-*): $1,500–$4,000+

risk_table_rev <- risk_table %>%
  # Normalize size labels, then bucket into revenue tiers
  mutate(
    .SizeNorm = tolower(as.character(Size)),
    FeeTier = dplyr::case_when(
      grepl("micro", .SizeNorm) ~ "Micro",
      .SizeNorm %in% c("small") ~ "Small",
      grepl("medium", .SizeNorm) ~ "Medium",
      TRUE ~ "LargePlus"  # Large, Giant, Mega-Giant, Mega-Colossal, etc.
    ),
    fee_low  = dplyr::case_when(
      FeeTier == "Micro"     ~  300,
      FeeTier == "Small"     ~  500,
      FeeTier == "Medium"    ~  800,
      FeeTier == "LargePlus" ~ 1500
    ),
    fee_high = dplyr::case_when(
      FeeTier == "Micro"     ~   500,
      FeeTier == "Small"     ~   800,
      FeeTier == "Medium"    ~  1500,
      FeeTier == "LargePlus" ~  4000
    ),
    fee_mid = (fee_low + fee_high) / 2,
    
    # Revenue at risk per company (expected value = p(drop) * fee)
    rev_at_risk_low  = pred_prob * fee_low,
    rev_at_risk_mid  = pred_prob * fee_mid,
    rev_at_risk_high = pred_prob * fee_high
  )

#- Summaries-
# Overall totals
rev_summary_overall <- risk_table_rev %>%
  summarise(
    Businesses = n(),
    RevAtRisk_Low  = sum(rev_at_risk_low,  na.rm = TRUE),
    RevAtRisk_Mid  = sum(rev_at_risk_mid,  na.rm = TRUE),
    RevAtRisk_High = sum(rev_at_risk_high, na.rm = TRUE)
  )

# By risk band (Low / Guarded / Elevated / High / Critical)
rev_by_band <- risk_table_rev %>%
  group_by(RiskBand) %>%
  summarise(
    Businesses = n(),
    AvgPredProb = mean(pred_prob, na.rm = TRUE),
    RevAtRisk_Low  = sum(rev_at_risk_low,  na.rm = TRUE),
    RevAtRisk_Mid  = sum(rev_at_risk_mid,  na.rm = TRUE),
    RevAtRisk_High = sum(rev_at_risk_high, na.rm = TRUE)
  ) %>%
  arrange(desc(RevAtRisk_Mid))

# Top accounts by expected revenue at risk
top_accounts <- risk_table_rev %>%
  arrange(desc(rev_at_risk_mid)) %>%
  select(
    Business.ID, Business, County, Rating, Size, Employees, Revenue,
    Joined_Year, NAICS_2digit, pred_prob,
    fee_low, fee_mid, fee_high,
    rev_at_risk_low, rev_at_risk_mid, rev_at_risk_high
  ) %>%
  slice_head(n = 25)

# Quick console printouts (nicely formatted)
cat("\n=== Overall Revenue at Risk ===\n")
print(rev_summary_overall %>%
        mutate(across(starts_with("RevAtRisk_"), scales::dollar)))

cat("\n=== Revenue at Risk by RiskBand (sorted by expected/mid) ===\n")
print(rev_by_band %>%
        mutate(across(starts_with("RevAtRisk_"), scales::dollar),
               AvgPredProb = percent(AvgPredProb, accuracy = 0.1)))

#- Exports-
write.csv(risk_table_rev,"business_drop_risk_with_revenue.csv",row.names = FALSE)
write.csv(rev_by_band,   "revenue_at_risk_by_band.csv",        row.names = FALSE)
write.csv(top_accounts,  "top_revenue_at_risk_accounts.csv",   row.names = FALSE)
