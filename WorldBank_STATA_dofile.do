* ============================================================
* 0. Setup: Working directory and housekeeping
* ============================================================
cd "C:\Users\Luwandagga\Desktop\Master Card\World Bank\Codes\New folder\New folder"

clear all
set more off

* ============================================================
* 1. Load dataset
* ============================================================
use Data2006.dta, clear

* ============================================================
* 2. Data cleaning and recoding
* ============================================================

* Literacy variable
recode v155 (0 9 = 0 "cannot read at all") ///
            (1   = 1 "able to read only parts of the sentence") ///
            (2   = 2 "able to read whole sentence") ///
            (3   = 3 "no card with required language") ///
            (4   = 4 "blind/visually impaired"), generate(v155new)

* Weighting variable
gen wt = v005/1000000

* Education categories
recode v133 (0 = 0 "No education") ///
            (1/7 = 2 "Primary") ///
            (8/19 = 3 "Secondary+") ///
            (99 = 99 "Missing"), generate(v133new)

* Safe drinking water categories
recode v113 (35 36 44 45 46 51 = 0 "not safe") ///
            (11 12 13 22 23 33 34 61 62 71 91 96 = 1 "safe for drinking") ///
            (97 = .), gen(v113new)

* Create strata for survey design
egen strata = group(v024 v025)

* Define survey design
svyset v001 [pweight=wt], strata(strata)

* ============================================================
* 3. Weighted descriptives
* ============================================================

* Distribution of literacy
tab v155new [aw=wt]

* Distribution of education
tab v133new [aw=wt]

* Distribution of safe drinking water
tab v113new [aw=wt]

* Weighted mean and percentiles for age
mean v012 [aw=wt]
_pctile v012 [aw=wt], p(25 50 75)

* Mean children ever born by region and residence
mean v201 [aw=wt], over(v024)
mean v201 [aw=wt], over(v025)

* Correlation (unweighted, because corr does not allow weights)
corr v133 v201

* Weighted cross-tabs with survey design
svy: tab v133new v024, col pearson
svy: tab v133new v025, col pearson

* ============================================================
* 4. Visualization
* ============================================================

* Create age groups for weighted distribution (15–19, 20–24, …)
gen agegrp = floor(v012/5)*5
label define agegrp 15 "15-19" 20 "20-24" 25 "25-29" 30 "30-34" 35 "35-39" 40 "40-44" 45 "45-49"
label values agegrp agegrp

* Weighted distribution of age groups
svy: tab agegrp, percent


* ============================================================
* 5. Regression models
* ============================================================

* Linear regression (safe water vs predictors)
regress v113new v025 v106 v120 [aw=wt]
estat ic

* Multinomial logistic regression
mlogit v113new v025 v106 v120 [aw=wt]
estat ic
