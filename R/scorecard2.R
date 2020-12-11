# Install required package -----
# install.packages("scorecard") # v0.3.1 

# Load scorecard package -----
library(scorecard)
## Help -----
help(topic = "scorecard", package = "scorecard")

# Load example data -----
cr_default <- read.csv("data-raw/CleanCreditScoring.csv", header = TRUE)
cr_default <- cr_default[, 1:16]
View(cr_default)

prop.table(table(cr_default$Status))

# Filter variable via missing rate, IV, identical value rate (optional) -----
dt_sel <- var_filter(dt = cr_default, 
                     y = "Status", 
                     iv_limit = 0.02, # should >= iv_limit to keep variable 
                     missing_limit = 0.95, # should <= missing_limit to keep variable 
                     identical_limit = 0.95, # should <= identical_limit to keep variable 
                     return_rm_reason = TRUE, 
                     positive = "bad|1" # Event/category that being a concern
)
names(cr_default)[!names(cr_default) %in% names(dt_sel$dt)]
View(dt_sel$rm)
dt_sel <- dt_sel$dt

# Split data -----
dt_split <- split_df(dt = dt_sel, 
                     y = "Status", 
                     ratio = c(0.75, 0.25), 
                     seed = 1001, 
                     name_dfs = c("train", "test"))

## Data train -----
train <- dt_split$train
dim(train)

## Data test -----
test <- dt_split$test
dim(test)

# WoE binning -----
bins <- woebin(dt_sel, y = "Status")
bins
## Plot bins -----
woebin_plot(bins)

# Apply woe to train data -----
train_woe <- woebin_ply(train, bins = bins)
print(train_woe)

# Model glm -----
model <- glm(Status ~ ., data = train_woe, family = binomial())
summary(model)

# Select a formula-based model by AIC -----
m_step <- step(model, direction = "both", trace = TRUE)
model_step <- eval(m_step$call)
summary(model_step)

# Predict probability -----
## Convert test data into woe -----
test_woe <- woebin_ply(test, bins = bins)
test_woe

## Predict woe converted test data ---- 
dt_pred <- predict(object = model_step, 
                   newdata = test_woe, type = "response")

# Performance -----
## KS & ROC Plot
perf_eva(pred = dt_pred, label = test$Status, 
         # binomial_metric = "auc", 
         confusion_matrix = TRUE)
perf_cv(train_woe, y = "Status", no_folds = 5, seeds = 101, binomial_metric = "auc")

# Scorecard -----
## Use created model -----
card <- scorecard(bins = bins, model = model_step, 
                  points0 = 600, odds0 = 1/19, pdo = 50)
card

## Without model definition manually (default glm) -----
card2 <- scorecard2(bins = bins, dt = cr_default, 
                    y = "Status", 
                    x = sub("_woe", "", names(coef(model_step))[-1]), 
                    points0 = 600, odds0 = 1/19, pdo = 50)
card2

# Scoring -----
score <- scorecard_ply(dt = test, card = card, 
                       only_total_score = FALSE)
print(score)
