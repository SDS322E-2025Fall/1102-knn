library(tidyverse)
library(caret)
library(kknn)

#################################
# A little script to play around with different seed for train-test split
penguins_clean2 <- as_tibble(datasets::penguins) |>
  na.omit() |>
  mutate(sex2 = ifelse(sex == "male", 1, 0))

# Step 1: conduct the same training testing split
m <- nrow(penguins_clean2)
set.seed(12345) # CHANGE ME
val <- sample(1:m, size = round(m/3), replace = FALSE)
penguins_training2 <- penguins_clean2[-val,]
penguins_testing2 <- penguins_clean2[val,]

# Step 2: fit the KNN model with different k_vec
# you can think of this as for each k, we make a prediction of the 111 testing points
# this gives us 20 * 111 = 2220 predictions
k_vec <- 1:20
k_res <- map_dfr(k_vec, function(k){
  knn_res2 <- kknn(sex2 ~ bill_len + bill_dep, k = k,
                   train = penguins_training2, test = penguins_testing2)
  res <- tibble(pred_num = predict(knn_res2),
                pred_class = ifelse(pred_num > 0.5, 1, 0))
  tibble(pred = res$pred_class, sex = penguins_testing2$sex2)
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
  scale_x_continuous(breaks = 1:20)
