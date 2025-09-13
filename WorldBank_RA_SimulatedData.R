# Load libraries
library(tidyverse)
library(fixest)
library(survey)


# 1. Simulate dataset
# -----------------------------
set.seed(42)
n <- 500
data <- tibble(
  household_id = 1:n,
  psu = sample(1:20, n, replace = TRUE),
  strata = sample(1:5, n, replace = TRUE),
  treated = rbinom(n, 1, 0.5),
  post = rbinom(n, 1, 0.5),
  age = sample(20:70, n, replace = TRUE),
  education_years = sample(0:15, n, replace = TRUE),
  income = rnorm(n, 1000, 300),
  farm_size = runif(n, 0.5, 5.0),
  adoption_score = sample(0:3, n, replace = TRUE)
)

# Outcome variable influenced by treatment and covariates
data <- data %>% mutate(
  outcome = 2*treated*post + 0.1*age + 0.2*education_years + 0.001*income + rnorm(n,0,1),
  adoption_binary = ifelse(adoption_score > 0, 1, 0),
  post_treat = treated*post
)

# Save dataset
write_csv(data, "simulated_coffee_farmers.csv")


# 2. Survey design
# -----------------------------
svy <- svydesign(ids = ~psu, strata = ~strata, weights = ~1, data = data)


# 3. Descriptive statistics
# -----------------------------
svymean(~income + adoption_score + farm_size, svy)

# -----------------------------
# 4. Logistic regression for CSA adoption
# -----------------------------
logit_model <- glm(adoption_binary ~ treated + post + age + education_years + income + farm_size,
                   data = data, family = binomial)
summary(logit_model)


# 5. Difference-in-Differences (DiD) analysis
# -----------------------------
did_model <- feols(outcome ~ treated*post + age + education_years + income + farm_size |
                     household_id + psu, data = data, cluster = ~psu)
summary(did_model)


# 6. Visualization
# -----------------------------
data %>%
  group_by(post, treated) %>%
  summarize(mean_outcome = mean(outcome, na.rm = TRUE)) %>%
  ggplot(aes(x=factor(post), y=mean_outcome, fill=factor(treated))) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Post-intervention", y="Mean Outcome", fill="Treated") +
  theme_minimal()
