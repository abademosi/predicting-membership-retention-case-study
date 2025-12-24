## Overview
This project is a professional case study demonstrating how data analytics and predictive modeling can be used to understand membership retention risk and revenue exposure for an accredited business organization.

Using internal membership and accreditation data, I built an **interactive analytics system** and a **logistic regression model** to identify at-risk members, quantify expected revenue loss, and support targeted retention strategies.
To protect confidentiality, this repository contains **redacted code and documentation only**; no proprietary data is shared.

## Business Problem
Membership retention is a critical driver of revenue and organizational stability. Leadership needed a data-driven way to:
* Understand which accredited businesses are most likely to drop membership
* Identify key predictors of churn
* Estimate **revenue at risk** under current retention trends
* Prioritize outreach using limited staff resources

## Tools & Technologies
* **R / RStudio**
* **Shiny** (interactive dashboard development)
* **Logistic Regression**
* Data cleaning, transformation, and feature engineering
* Visualization and KPI reporting

## Data (Confidential)
The original analysis used internal accredited business reports, membership records, and standardized NAICS industry codes.
To ensure ethical and professional handling of sensitive data:
* No raw data is included in this repository
* File names and identifiers have been redacted
* The code demonstrates methodology and structure, not proprietary content

## Approach
### 1. Data Preparation & Feature Engineering
* Filtered records to active Maryland counties
* Removed outdated historical records
* Standardized inconsistent join-date formats
* Engineered key features:
  * Join year
  * Accreditation status
  * County
  * Business size
  * BBB rating
  * 2-digit NAICS industry classification
### 2. Interactive Shiny Application
Developed a Shiny dashboard to allow stakeholders to:
* Filter businesses by county, industry, size, and accreditation status
* Explore trends in membership growth and attrition
* View summary KPIs and export filtered results
This provided a self-service analytics tool comparable to Tableau or Power BI, but fully reproducible and locally deployable.
### 3. Predictive Modeling (Logistic Regression)
A logistic regression model was used to predict whether a business would drop or retain membership.
**Target variable**
* Drop (1) vs. Retain (0)
**Key predictors**
* Year joined
* BBB rating
* County
* Business size indicators
* Industry classification
**Model performance**
* Accuracy: ~71.7%
* Strong interpretability and stable coefficients
* Most influential predictors:
  * Tenure (older members more likely to drop)
  * BBB rating (NR and low ratings highest risk)
  * Geography (specific counties with elevated risk)
### 4. Risk Scoring & Revenue Impact
Each business received:
* A predicted probability of dropping
* Assignment to a five-level risk band (Low → Critical)
Using membership fee ranges, I estimated expected revenue at risk by multiplying drop probability by tier-based dues estimates.

## Key Results
* **~$500,000** in annual membership dues estimated at risk under current trends
* **High and Critical risk accounts** (~10% of members) represent ~$75,000 in near-term risk
* **Guarded and Elevated bands** represent over $330,000 in potential losses if disengagement continues
* Retention risk is highly concentrated, enabling efficient prioritization

## Leadership Insights
* Retention risk is not evenly distributed
* BBB rating is the strongest behavioral signal
* Renewal fatigue increases with tenure
* Geographic patterns matter
* Targeted intervention yields high ROI compared to blanket outreach

## Deliverables (Original Project)
* Interactive Shiny dashboard for exploration and filtering
* Member-level risk scoring with estimated revenue impact
* Aggregated revenue-at-risk summaries
* Ranked list of high-value, high-risk accounts

## Ethical Note
This repository is intentionally designed as a **professional case study**.
All sensitive data has been removed or redacted to preserve confidentiality while still demonstrating analytical rigor, modeling decisions, and business impact.

## Why This Project Matters
This project demonstrates:
* End-to-end analytics thinking
* Responsible handling of sensitive data
* Predictive modeling for real business decisions
* Translation of statistical output into executive-level insights
* Alignment of data science with governance and risk management

*This case study reflects professional work completed during an internship. Code and data have been modified for portfolio use.*
