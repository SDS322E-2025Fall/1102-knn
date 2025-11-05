library(tidyverse)
library(patchwork)
library(kknn)
library(caret)
penguins_clean <- as_tibble(datasets::penguins) |> na.omit()
penguins_clean


######################################################################
# script 1: Basic KNN with caret::knn3()
######################################################################
# Step 1: scale the predictors
penguins_std <- penguins_clean |>
  select(bill_len, bill_dep) |>
  ...|>
  ...

# Step 2: fit a knn model with caret::knn3()
knn_res <- knn3(sex ~ bill_len + bill_dep,  data = penguins_std)


# Step 3: predict on teh standardized full dataset
knn_pred <- predict(knn_res, newdata = penguins_std)

# Step 4: calculate classification metrics
pred_knn_df <- as_tibble(knn_pred) |>
  mutate(class = ifelse(male > female, "male", "female"))
table(pred_knn_df$class, penguins_std$sex)


######################################################################
# script 2: training/ testing split with logistic regression
######################################################################
# step 1: conduct a training testing split of 1/3 and 2/3 for testing and training
m <- nrow(penguins_clean)
set.seed(123)
val <- ...
penguins_training <- ...
penguins_testing <- ...


# step 2: fit a logistic regression model
mod_glm <- glm(sex ~ bill_len + bill_dep, family = binomial, data = ...)


# step 3: get the prediction for the testing set
lr_pred_vec <- predict(mod_glm, newdata = ..., type = "...")

# step 4: calculate classification metrics
lr_pred_res <- tibble(pred = lr_pred_vec, pred_class = ifelse(pred > 0.5, 1, 0))
table(..., ...)


######################################################################
# Your time: training/ testing split with KNN
######################################################################
# step 1

# step 2

# step 3

# step 4

# step 5




######################################################################
# Your time: training/ testing split with KNN for regression
######################################################################
penguins_clean2 <- as_tibble(datasets::penguins) |> na.omit() |>
  mutate(sex2 = ifelse(sex == "male", 1, 0))






######################################################################
# script 3: Hyperparameter tuning with different seed
# change the seed value to see how the training-testing split
######################################################################
# Step 1: conduct the same training testing split
penguins_std <- penguins_clean |>
  select(bill_len, bill_dep) |>
  scale() |>
  bind_cols(sex = penguins_clean2$sex)
m <- nrow(penguins_std)
set.seed(12345) # CHANGE ME
val <- sample(1:m, size = round(m/3), replace = FALSE)
penguins_training2 <- penguins_std[-val,]
penguins_testing2 <- penguins_std[val,]


# Step 2: fit the KNN model with different k_vec
# you can think of this as for each k, we make a prediction of the 111 testing points
# this gives us 20 * 111 = 2220 predictions
k_vec <- 1:20
k_res <- map_dfr(k_vec, function(k){
  knn_res2 <- kknn(sex ~ bill_len + bill_dep, k = k,
                   train = penguins_training2, test = penguins_testing2)
  res <- tibble(pred_class = predict(knn_res2))
  tibble(pred = res$pred_class, sex = penguins_testing2$sex)
}, .id = "k")

# Step 3: compute the prediction accuracy for each k and plot it
acc_count_df <- k_res |>
  group_split(k) |>
  map_dfr(~count(.x, pred, sex), .id = "k") |>
  filter(pred == sex) |>
  mutate(k = as.integer(k)) |>
  group_by(k) |>
  summarize(accuracy = sum(n) / 111)

acc_count_df |>
  ggplot(aes(x = k, y = accuracy)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:20) +
  ggtitle("With seed = 1105, best k = 4 or 5 or 18")


######################################################################
# script 4: Cross validation for KNN
######################################################################
library(kknn)
library(caret)
set.seed(123)
penguins_clean <- as_tibble(datasets::penguins) |> na.omit()

penguins_std <- penguins_clean |>
  select(bill_len, bill_dep) |>
  scale() |>
  bind_cols(sex = penguins_clean$sex)


folds <- createFolds(penguins_std$sex, k = 10)

k_values <- 1:20
acc_k <- numeric(length(k_values))

for (j in seq_along(k_values)) {
  acc_fold <- c()

  for (i in 1:10) {
    test_idx <- folds[[i]]
    train_data <- penguins_std[-test_idx, ]
    test_data  <- penguins_std[test_idx, ]

    mod <- kknn(sex ~ bill_len + bill_dep,
                train = train_data,
                test = test_data,
                k = k_values[j])

    pred <- fitted(mod)
    acc_fold[i] <- mean(pred == test_data$sex)
  }

  acc_k[j] <- mean(acc_fold)
}

# Show best k
best_k <- k_values[which.max(acc_k)]
tibble(x = 1:20, y = acc_k) |>
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:20)

