#####
# Libraries
#####
# Dependencies: run Rscript scripts/install-packages.R before this script.
# Dependencies: run Rscript scripts/install-packages.R before this script.
library(tidyverse)
library(GGally)
library(ggplot2)
library(tidymodels) 
library(readr)

#####
# 1. Data
#####

traffic <- read_csv(
  "data/recipe_site_traffic_2212.csv"
)
glimpse(traffic)

#####
# Data wrangling
#####

# we need to specify that this variable is a category for the ML models, put just numbers is a mistake

traffic <- traffic %>%
  mutate(category = as.factor(category))

levels(traffic$category)

# recode high_traffic

traffic <- traffic %>%
  mutate(
    high_traffic_bin = ifelse(is.na(high_traffic), 0, 1)
  )

table(traffic$high_traffic_bin)

sum(is.na(traffic$high_traffic_bin))

# specify that servings is a number no a string

unique(traffic$servings)

traffic <- traffic %>%
  mutate(servings = as.numeric(servings))

# We are going to drop the lines with NA

traffic %>%
  mutate(
    na_count = rowSums(is.na(select(., -high_traffic)))
  ) %>%
  summarise(
    filas_con_algun_NA = sum(na_count > 0),
    filas_sin_NA = sum(na_count == 0)
  )

traffic_clean <- traffic %>%
  filter(
    rowSums(is.na(select(., -high_traffic))) == 0
  )

dim(traffic)
dim(traffic_clean)

sum(is.na(select(traffic_clean, -high_traffic)))

# We are going to work with this dataset "traffic_clean"

traffic_clean <- traffic_clean %>%
  select(-high_traffic)

#####
# 2. EDA
#####

# check that the data looks good for compare the groups

glimpse(traffic_clean)

traffic_clean %>%
  summarise(
    n = n(),
    prop_high = mean(high_traffic_bin == 1)
  )

table(traffic_clean$high_traffic_bin)

# descriptive statistic for high_traffic

traffic_clean %>%
  summarise(across(c(calories, carbohydrate, sugar, protein, servings),
                   list(min=min, p25=~quantile(.x,0.25), median=median, mean=mean,
                        p75=~quantile(.x,0.75), max=max)))

traffic_clean %>%
  group_by(high_traffic_bin) %>%
  summarise(
    n = n(),
    across(
      c(calories, carbohydrate, sugar, protein, servings),
      list(mean = ~mean(.x), sd = ~sd(.x), median = ~median(.x)),
      .names = "{.col}_{.fn}"
    )
  )

# Histograms

traffic_long <- traffic_clean %>%
  select(
    high_traffic_bin,
    calories,
    carbohydrate,
    sugar,
    protein,
    servings
  ) %>%
  pivot_longer(
    cols = -high_traffic_bin,
    names_to = "variable",
    values_to = "value"
  )

ggplot(
  traffic_long,
  aes(
    x = value,
    fill = factor(high_traffic_bin)
  )
) +
  geom_histogram(
    bins = 30,
    position = "identity",
    alpha = 0.4
  ) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  scale_fill_manual(
    values = c("0" = "#1F4E79", "1" = "#B22222"),
    labels = c("Low traffic", "High traffic")
  ) +
  labs(
    title = "Histograms of Covariates by Traffic Level",
    subtitle = "Comparison between high- and low-traffic recipes",
    x = NULL,
    y = "Count",
    fill = "Traffic level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Density plots

ggplot(
  traffic_long,
  aes(
    x = value,
    colour = factor(high_traffic_bin),
    linetype = factor(high_traffic_bin)
  )
) +
  geom_density(linewidth = 1) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  scale_colour_manual(
    values = c("0" = "#1F4E79", "1" = "#B22222"),
    labels = c("Low traffic", "High traffic")
  ) +
  scale_linetype_manual(
    values = c("0" = "solid", "1" = "longdash"),
    labels = c("Low traffic", "High traffic")
  ) +
  labs(
    title = "Kernel Density Estimates by Traffic Level",
    subtitle = "Smoothed distribution of covariates for high- vs low-traffic recipes",
    x = NULL,
    y = "Density",
    colour = "Traffic level",
    linetype = "Traffic level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Box plots

ggplot(
  traffic_long,
  aes(
    x = factor(high_traffic_bin),
    y = value,
    fill = factor(high_traffic_bin)
  )
) +
  geom_boxplot(width = 0.7, outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  scale_x_discrete(labels = c("0" = "Low traffic", "1" = "High traffic")) +
  scale_fill_manual(
    values = c("0" = "#1F4E79", "1" = "#B22222"),
    labels = c("Low traffic", "High traffic")
  ) +
  labs(
    title = "Boxplots of Covariates by Traffic Level",
    subtitle = "Median, interquartile range, and outliers by outcome group",
    x = NULL,
    y = NULL,
    fill = "Traffic level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# EDA for category (bar plots)

ggplot(
  traffic_clean,
  aes(
    x = category,
    fill = factor(high_traffic_bin)
  )
) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(
    values = c("0" = "#1F4E79", "1" = "#B22222"),
    labels = c("Low traffic", "High traffic")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of High-Traffic Recipes by Category",
    subtitle = "Within-category proportions of low vs high traffic outcomes",
    x = "Recipe category",
    y = "Proportion of recipes",
    fill = "Traffic level"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# correlations

num_vars <- c("calories", "carbohydrate", "sugar", "protein", "servings")

traffic_pairs <- traffic_clean %>%
  mutate(
    log_calories      = log1p(calories),
    log_carbohydrate  = log1p(carbohydrate),
    log_sugar         = log1p(sugar),
    log_protein       = log1p(protein)
  )

num_vars_log <- c("log_calories", "log_carbohydrate", "log_sugar", "log_protein", "servings")

p_pairs_log <- ggpairs(
  traffic_pairs %>% select(high_traffic_bin, all_of(num_vars_log)),
  columns = 2:(length(num_vars_log) + 1),
  mapping = aes(colour = factor(high_traffic_bin), fill = factor(high_traffic_bin)),
  upper = list(continuous = wrap("cor", size = 6)),
  lower = list(
    continuous = wrap(
      "points",
      alpha = 0.20,
      size = 0.8,
      stroke = 0.5,
      position = position_jitter(width = 0.02, height = 0.02)
    )
  ),
  diag = list(continuous = wrap("densityDiag", alpha = 0.25))
)

p_pairs_log +
  scale_colour_manual(values = c("0" = "#1F4E79", "1" = "#B22222"),
                      labels = c("Low traffic", "High traffic")) +
  scale_fill_manual(values = c("0" = "#1F4E79", "1" = "#B22222"),
                    labels = c("Low traffic", "High traffic")) +
  ggtitle("Pairwise Relationships Among Numerical Covariates (log scale)",
          subtitle = "Log-transformed nutrition variables; servings kept in levels") +
  labs(colour = "Traffic level", fill = "Traffic level") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#####
#3. train/test
#####

traffic_clean <- traffic_clean %>%
  mutate(
    high_traffic_bin = factor(high_traffic_bin, levels = c(0, 1),
                              labels = c("Low", "High"))
  )

table(traffic_clean$high_traffic_bin)

set.seed(123)  # para reproducibilidad

split_obj <- initial_split(
  traffic_clean,
  prop   = 0.80,                  # 80% train, 20% test
  strata = high_traffic_bin        # mantiene proporción de High/Low en ambos sets
)

train_data <- training(split_obj)
test_data  <- testing(split_obj)

# Easy check
nrow(train_data); nrow(test_data)

prop.table(table(train_data$high_traffic_bin))
prop.table(table(test_data$high_traffic_bin))

# 10 fold CV (just for training, the 10 fold CV for the tuning 
# parameters needs to be in each necessary model *not all models*)

set.seed(123)

cv_folds <- vfold_cv(
  train_data,
  v = 10,
  strata = high_traffic_bin
)

cv_folds
###
#4.Estimate a linear probability model (LPM) and a logit model
###


# LPM
# Make outcome numeric for the LPM (High = 1, Low = 0)
train_data <- train_data %>%
  mutate(y = ifelse(high_traffic_bin == "High", 1, 0))

test_data <- test_data %>%
  mutate(y = ifelse(high_traffic_bin == "High", 1, 0))

# Linear Probability Model formulas (no interactions)
mod1 <- y ~ calories + carbohydrate + sugar + protein + servings + category
mod2 <- y ~ log1p(calories) + log1p(carbohydrate) + log1p(sugar) + log1p(protein) + servings + category

# Fit LPMs
lm.fit1 <- lm(mod1, data = train_data)
lm.fit2 <- lm(mod2, data = train_data)

# Table 
# Dependencies: run Rscript scripts/install-packages.R before this script.
library(stargazer)

stargazer(
  lm.fit1, lm.fit2,
  dep.var.caption = "",
  dep.var.labels  = "",
  omit.table.layout = "n",
  star.cutoffs = NA, no.space = TRUE,
  keep.stat = c("rsq", "n"),
  header = FALSE,
  column.labels = c("LPM 1", "LPM 2"),
  title = "High traffic (Linear Probability Models)",
  type = "text"
)

#Both linear probability models display very similar goodness of fit. 
#Since results are robust across specifications, we retain the first model.


# Variable importance: t-statistics 
temp <- summary(lm.fit1)

varimp <- tibble(
  variable = rownames(temp$coefficients[-1, ]),  # drop Intercept
  score    = temp$coefficients[-1, 3]            # t-stat
) %>%
  arrange(desc(abs(score))) %>%
  mutate(variable = factor(variable, levels = variable[order(abs(score))]))

ggplot(varimp, aes(x = variable, y = score)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  ggtitle("Variable importance (t-stat): what predicts high traffic?")

# Predictions on TRAIN
train_data <- train_data %>%
  mutate(
    pred_prob = predict(lm.fit1),
    pred_class = ifelse(pred_prob >= 0.5, 1, 0)
  )

ggplot(train_data, aes(x = pred_prob, y = y)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(y = pred_prob), color = "red") +
  labs(
    x = "predicted probability (LPM)",
    y = "High traffic (0/1)",
    title = "High traffic: LPM predictions (train)"
  ) +
  theme_bw()

# Accuracy on TEST using 0.5 threshold
test_data <- test_data %>%
  mutate(
    pred_prob = predict(lm.fit1, newdata = test_data),
    pred_class = ifelse(pred_prob >= 0.5, 1, 0)
  )

mean(test_data$pred_class == test_data$y)

###
# LOGIT
###

# Formula (no interactions)
mod_logit <- high_traffic_bin ~ calories + carbohydrate + sugar + protein + servings + category

# Fit logit (glm with binomial)
logit_fit <- glm(mod_logit, data = train_data, family = binomial)

summary(logit_fit)

# Table 
library(stargazer)

stargazer(
  logit_fit,
  dep.var.caption = "",
  dep.var.labels  = "",
  omit.table.layout = "n",
  star.cutoffs = NA, no.space = TRUE,
  keep.stat = c("n", "aic"),
  header = FALSE,
  column.labels = c("Logit"),
  title = "High traffic (Logit model)",
  type = "text"
)

# Variable importance
temp <- summary(logit_fit)

varimp_logit <- tibble(
  variable = rownames(temp$coefficients[-1, ]),  # drop Intercept
  score    = temp$coefficients[-1, 3]            # z-stat
) %>%
  arrange(desc(abs(score))) %>%
  mutate(variable = factor(variable, levels = variable[order(abs(score))]))

ggplot(varimp_logit, aes(x = variable, y = score)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  ggtitle("Variable importance (z-stat): what predicts high traffic?")

# Predictions on TEST using 0.5 threshold
test_data <- test_data %>%
  mutate(
    pred_prob_logit  = predict(logit_fit, newdata = test_data, type = "response"),
    pred_class_logit = ifelse(pred_prob_logit >= 0.5, 1, 0)
  )

mean(test_data$pred_class_logit == ifelse(test_data$high_traffic_bin == "High", 1, 0))

# Simple diagnostic plot
ggplot(test_data, aes(x = pred_prob_logit, y = ifelse(high_traffic_bin == "High", 1, 0))) +
  geom_point(alpha = 0.25) +
  geom_smooth(se = FALSE) +
  labs(
    x = "predicted probability (Logit)",
    y = "High traffic (0/1)",
    title = "High traffic: Logit predictions (test)"
  ) +
  theme_bw()


### Now we will compare the marginal effects of the 2 models

# Marginal Effects comparison: LOGIT vs LPM1 
# Dependencies: run Rscript scripts/install-packages.R before this script.
library(mfx)

# LOGIT marginal effects 

mod_me <- y ~ calories + carbohydrate + sugar + protein + servings + category

me_logit_atmean  <- mfx::logitmfx(mod_me, data = train_data, atmean = TRUE)
me_logit_average <- mfx::logitmfx(mod_me, data = train_data, atmean = FALSE)  # AME

# Comparison table: Logit ME at mean, Logit AME, LPM coef (marginal effect in LPM)
ME <- cbind(
  me_logit_atmean$mfxest[, "dF/dx"],
  me_logit_average$mfxest[, "dF/dx"],
  coefficients(lm.fit1)[-1]   # drop intercept
)

colnames(ME) <- c("Logit ME (at mean)", "Logit AME", "LPM1 coef")
print(ME, digits = 3)

# Quick manual check for ONE continuous regressor

G_derivative <- function(z) exp(z) / (1 + exp(z))^2

temp <- train_data %>%
  mutate(
    index  = predict(logit_fit, newdata = train_data, type = "link"),
    gprime = G_derivative(index)
  )

c(
  manual_AME_servings = coefficients(logit_fit)["servings"] * mean(temp$gprime),
  mfx_AME_servings    = ME["servings", "Logit AME"]
)

###
#t-statistics of the estimated marginal effects as your variable importance score and plotting it 
###


# LOGIT: t-statistics AME
me_ame <- me_logit_average$mfxest  # matrix with dF/dx, Std. Err., z, P>|z|

varimp_logit_me <- tibble(
  variable = rownames(me_ame),
  score    = me_ame[, "z"],     # t/z-stat of the marginal effect
  model    = "Logit (AME)"
)

#LPM1: t-statistics of coefficients 
temp_lpm <- summary(lm.fit1)

varimp_lpm <- tibble(
  variable = rownames(temp_lpm$coefficients[-1, ]),  # drop intercept
  score    = temp_lpm$coefficients[-1, 3],           # t-stat
  model    = "LPM1"
)

# Combine and order by overall importance (|t|) ---
varimp_all <- bind_rows(varimp_lpm, varimp_logit_me) %>%
  group_by(variable) %>%
  mutate(order_score = max(abs(score), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(order_score), model) %>%
  mutate(variable = factor(variable, levels = unique(variable)))

# Column/bar plot 
ggplot(varimp_all, aes(x = variable, y = score, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Variable importance based on t-statistics of marginal effects",
    subtitle = "Logit: t/z-statistics of AME; LPM: t-statistics of coefficients",
    x = NULL,
    y = "t-statistic (signed)",
    fill = "Model"
  )
### To write in the markdown : **General conclusion**

#The variable-importance plot clearly shows that **recipe category is by far the main driver of high traffic**,
#dominating all other covariates. In both the **LPM** and the **Logit model (using Average Marginal Effects)**,
#category indicators exhibit the largest t-statistics, indicating a strong and robust positive association with the probability 
#that a recipe generates high traffic. By contrast, continuous nutritional characteristics—such as calories, carbohydrates, protein, and servings—display 
#very small t-statistics, suggesting that their explanatory power is limited once recipe category is taken into account. Sugar is the only nutritional
#variable with a consistently negative effect, although its importance remains modest relative to category.
#The close similarity in the **ranking of variable importance across models** highlights the **robustness of the results to model specification**. 
#While the Logit model delivers more conservative and theoretically consistent estimates due to its non-linear structure, both models convey the same 
#substantive message: **user engagement is primarily driven by the type of recipe rather than by its nutritional composition**.

###
#5. RANDOM FOREST
###

# Dependencies: run Rscript scripts/install-packages.R before this script.
library(ranger)
library(vip)

set.seed(123)

# RF specification (classification)
rf_spec <- rand_forest(
  trees = 1000,
  mtry  = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "permutation") %>%   # permutation importance
  set_mode("classification")

# Recipe (Converting the categories to dummies)
rf_rec <- recipe(
  high_traffic_bin ~ calories + carbohydrate + sugar + protein + servings + category,
  data = train_data
) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# Workflow
rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

# Tune (best parameters)
rf_grid <- grid_regular(
  mtry(range = c(2, 10)),
  min_n(range = c(2, 25)),
  levels = 5
)

rf_tuned <- tune_grid(
  rf_wf,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc, accuracy)
)

# pick best by ROC AUC
best_params <- select_best(rf_tuned, metric = "roc_auc")
best_params

# Final fit on training
rf_final_wf <- finalize_workflow(rf_wf, best_params)
rf_final_fit <- fit(rf_final_wf, data = train_data)

# Test performance 
rf_test_probs <- predict(rf_final_fit, new_data = test_data, type = "prob")
rf_test_class <- predict(rf_final_fit, new_data = test_data, type = "class")

rf_test_res <- test_data %>%
  bind_cols(rf_test_probs, rf_test_class)

metrics(rf_test_res, truth = high_traffic_bin, estimate = .pred_class)
roc_auc(
  rf_test_res,
  truth = high_traffic_bin,
  .pred_High,
  event_level = "second"   # High = clase positiva
)


# Variable importance (permutation importance from ranger)
rf_engine <- extract_fit_parsnip(rf_final_fit)$fit

vip(rf_engine, num_features = 15) +
  ggtitle("Random Forest variable importance (permutation)")

# Tidy table of importance 
rf_vi <- as_tibble(rf_engine$variable.importance, rownames = "variable") %>%
  rename(importance = value) %>%
  arrange(desc(importance)) %>%
  slice_head(n = 15)

rf_vi

##A Random Forest classifier was estimated using cross-validation to tune key hyperparameters. 
#The optimal model selected by ROC AUC uses a small number of candidate variables at each split (`mtry = 2`) 
#and a relatively large minimum node size (`min_n = 13`), indicating a conservative and well-regularized 
#forest. This specification limits overfitting while preserving predictive performance.
#When evaluated on the test set, the Random Forest achieves an accuracy of 77.1% and a Cohen’s Kappa of 0.51, 
#reflecting moderate agreement beyond chance. Importantly, the ROC AUC reaches 0.855 when “High traffic” is
#correctly defined as the positive class, indicating strong discriminatory power between high- and 
#low-traffic recipes. Overall, the Random Forest slightly outperforms the linear and logit models in terms
#of classification performance, particularly in ranking observations by their probability of high traffic.
#Permutation-based variable importance reveals that recipe category is the dominant source of predictive power, 
#confirming the findings from the Linear Probability Model and the Logit model. Among numerical covariates, 
#protein content emerges as the most important nutritional predictor, followed by calories, while sugar, 
#carbohydrates, and servings contribute little to predictive accuracy. The negligible or slightly negative 
#importance of servings suggests that this variable does not provide additional information once recipe category 
#and nutritional composition are taken into account.
#Taken together, these results indicate that the success of a recipe in generating high traffic is driven
#primarily by its categorical type rather than by fine-grained nutritional characteristics. The consistency 
#of variable importance rankings across econometric models and the Random Forest strengthens this conclusion 
#and suggests that non-linearities and interactions captured by the Random Forest do not fundamentally alter 
#the underlying predictive structure of the data.


###
#6. PREDICT COMPARE AND CHOOSE
###

# Clean unified prediction code 
test_eval <- test_data %>%
  mutate(
    truth_num = y,
    truth_fac = high_traffic_bin
  )
#truth_num : 0/1 (useful for LPM)
#truth_fac : Low/high (required fro ROC and confusion matrices)

#LPM predictions (clean version)
test_eval <- test_eval %>%
  mutate(
    prob_lpm  = predict(lm.fit1, newdata = test_eval),
    class_lpm = factor(
      ifelse(prob_lpm >= 0.5, "High", "Low"),
      levels = c("Low", "High")
    )
  )

#Logit predictions (clean version)
test_eval <- test_eval %>%
  mutate(
    prob_logit  = predict(logit_fit, newdata = test_eval, type = "response"),
    class_logit = factor(
      ifelse(prob_logit >= 0.5, "High", "Low"),
      levels = c("Low", "High")
    )
  )

#Random Forest predictions (aligned)
rf_probs  <- predict(rf_final_fit, new_data = test_eval, type = "prob")
rf_class  <- predict(rf_final_fit, new_data = test_eval, type = "class")

test_eval <- test_eval %>%
  bind_cols(rf_probs) %>%
  bind_cols(rf_class) %>%
  mutate(
    class_rf = .pred_class
  )

names(test_eval)

#We have predicted on the test data using our 3 different models
#Now we will use the ROC, AUC and the confusion matrix to compare 
#the accuracy of the 3 models.

# First: confusion matrix
library(yardstick)

# 1.1 LPM confusion matrix
conf_mat_lpm <- conf_mat(
  test_eval,
  truth = truth_fac,
  estimate = class_lpm
)

conf_mat_lpm

# 1.2 Logit confusion matrix
conf_mat_logit <- conf_mat(
  test_eval,
  truth = truth_fac,
  estimate = class_logit
)

conf_mat_logit

# 1.3 Random Forest confusion matrix
conf_mat_rf <- conf_mat(
  test_eval,
  truth = truth_fac,
  estimate = class_rf
)

conf_mat_rf

# 1.4 Accuracy comparison
accuracy_lpm <- accuracy(test_eval, truth_fac, class_lpm)
accuracy_logit <- accuracy(test_eval, truth_fac, class_logit)
accuracy_rf <- accuracy(test_eval, truth_fac, class_rf)

bind_rows(
  LPM   = accuracy_lpm,
  Logit = accuracy_logit,
  RF    = accuracy_rf,
  .id = "Model"
)

#Interpretation tip 
#Accuracy depends on the 0.5 threshold and class balance.
#That’s why ROC/AUC is usually preferred.

# Second: ROC curves (on the test set)
# 2.1: ROC objects
roc_lpm <- roc_curve(
  test_eval,
  truth = truth_fac,
  prob_lpm,
  event_level = "second"   # "High" is the positive class
)

roc_logit <- roc_curve(
  test_eval,
  truth = truth_fac,
  prob_logit,
  event_level = "second"
)

roc_rf <- roc_curve(
  test_eval,
  truth = truth_fac,
  .pred_High,
  event_level = "second"
)

# 2.2: Plot all ROC curves together
roc_lpm$model <- "LPM"
roc_logit$model <- "Logit"
roc_rf$model <- "Random Forest"

roc_all <- bind_rows(roc_lpm, roc_logit, roc_rf)

ggplot(roc_all, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(linewidth = 1.1) +
  geom_abline(linetype = "dashed", color = "grey50") +
  coord_equal() +
  theme_bw() +
  labs(
    title = "ROC curves on test data",
    subtitle = "Comparison of LPM, Logit, and Random Forest",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Model"
  )

#How to read
# Curves closer to the top-left are better
# Dashed line = random classifier
# If one curve dominates another -> better model

# Third: AUC comparison 
# 3.1: Compute AUCs 
auc_lpm <- roc_auc(
  test_eval,
  truth = truth_fac,
  prob_lpm,
  event_level = "second"
)

auc_logit <- roc_auc(
  test_eval,
  truth = truth_fac,
  prob_logit,
  event_level = "second"
)

auc_rf <- roc_auc(
  test_eval,
  truth = truth_fac,
  .pred_High,
  event_level = "second"
)

# 3.2: Put AUCs in one table 
bind_rows(
  LPM   = auc_lpm,
  Logit = auc_logit,
  RF    = auc_rf,
  .id = "Model"
)




