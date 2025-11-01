library(tidyverse)
library(caret)
library(kknn)

#################################
# A not so little script to do cross validation for KNN
set.seed(12345)
penguins_clean <- as_tibble(datasets::penguins) |> na.omit()
folds <- caret::createFolds(penguins_clean$sex, k = 10)

k_values <- 1:20
acc_k <- numeric(length(k_values))

for (j in seq_along(k_values)) {
  acc_fold <- c()

  for (i in 1:10) {
    test_idx <- folds[[i]]
    train_data <- penguins_clean[-test_idx, ]
    test_data  <- penguins_clean[test_idx, ]

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
  scale_x_binned(breaks = 1:20)
