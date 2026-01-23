library(dplyr)

# Logistic regression predicting the likelihood of a business dropping
drop_model <- glm(
  Dropped ~ Size + Employees + Customers + Revenue +
    County + Rating + Joined_Year + industry,
  data = model_data, family = binomial
)

# Predict the probability of dropping for each business
model_data$pred_prob <- predict(drop_model, type = "response")
model_data$pred_class <- ifelse(model_data$pred_prob >= 0.5, 1, 0)

# Find the accuracy of the model
conf_mat <- table(
  Predicted = model_data$pred_class,
  Actual = model_data$Dropped
)
conf_mat
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy

# Create risk table with predicted probabilities and risk bands
risk_table <- model_data %>%
  select(BusinessID, Business_Name, County, Rating, Size, Employees, Revenue,
         Joined_Year, Industry, Accredited, Dropped, pred_prob) %>%
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
# Membership costs by size from business rules:
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
    Business_ID, Business_Name, County, Rating, Size, Employees, Revenue,
    Joined_Year, Industry, pred_prob,
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

