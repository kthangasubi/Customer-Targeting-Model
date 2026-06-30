# Customer-Targeting-Model

Overview

This project builds a predictive classification model to identify high-response prospects for Bruce Rauner's Governor Campaign — specifically, prospects likely to donate $100. It uses logistic regression with forward stepwise variable selection on a dataset of 5,422 records and 26 variables, and evaluates model performance using a gains chart, lift analysis, and marginal cut-point analysis.


Key Results


The top 75% of scored prospects contain 87% of total donations
An alternative cut-point at 65% of prospects yields 81% of total donations, identified via marginal analysis
Contacting only the top 75% increases the response rate from 5.65% to 6.55%



Dataset

The dataset (S23.csv) contains 5,422 rows and 28 variables including:


Geo-demographic ordinal variables (rescaled to midpoint percentages)
Spend variables (rescaled to midpoint dollar amounts)
Categorical variables (MOSTYP, MOSHOO) converted to binary dummy variables



Note: The dataset is not publicly available. To run this script you will need access to the original data file and must update the file path in the script accordingly.




Most Impactful Variables

VariableDescriptionImpactMSKB2Contribution of Trailer Policies+9.80% per unit increaseMOSTYP_39Large Religious Families segment+7.68%MOSTYP_34Large Family, Employed Child segment+5.64%MOSTYP_23Young and Rising segment+2.92%MOSHOO_9Modern Complete Families segment+2.13%

All 10 variables in the final model had p-values below 5%, confirming statistical significance.


Workflow Summary

1. Data Preparation


Rescaled 18 geo-demographic ordinal variables to midpoint percentages (e.g. 1–10% → 5.5%)
Rescaled 3 spend variables to midpoint dollar amounts (e.g. $1–$49 → $25)
Converted categorical variables MOSTYP and MOSHOO to binary dummy columns
Checked for non-linear relationships using AIC comparison (linear vs. quadratic); linear form was retained


2. Train/Test Split


70% training / 30% testing split using sample_frac and anti_join
SeqNum removed from both sets before modeling


3. Model Building


Forward stepwise logistic regression (stepAIC) starting from a null model
10 statistically significant variables selected for the final model


4. Linear Regression (for interpretability)


Final variables refit using linear regression to produce interpretable slopes
Equation used in the business memo to communicate variable impact


5. Gains & Lift Analysis


Test set scored and sorted by predicted probability (high to low)
Cumulative prospects and responses tracked to build the gains chart
Lift calculated as the difference between model response rate and baseline
Marginal analysis performed at every 5% interval (every 82 records)



Output Files

FileDescriptionPerf.csvScored test set with cumulative prospects, responses, and liftBusiness Memo (.doc)Summary of results, gains chart, and cut-point analysisGains Chart (.xlsx)Visual gains chart and marginal analysis table


Libraries Used

rdplyr       # Data manipulation and train/test split
fastDummies # Creating dummy variables for categorical columns
MASS        # Stepwise variable selection (stepAIC)
