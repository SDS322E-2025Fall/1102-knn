library(tidyverse)
library(caret)
library(kknn)

# get the penguins dataset
penguins_clean <- as_tibble(datasets::penguins) |> na.omit()
head(penguins_clean, 5)

# conduct a training testing split of 1/3 and 2/3 for testing and training
m <- nrow(penguins_clean)
set.seed(123)
val <- ...
val
penguins_training <- ...
penguins_testing <- ...

# fit a knn model
knn_res <- kknn(sex ~ bill_len + bill_dep, ..., ...)

# get the prediction for the testing set
...


#################################
# Your time
# use the penguins_clean2 dataset below
# perform the same knn analysis on sex2
# observe whether using a categorical (female and male) or a
# binary (0 and 1) variable produce the same or different results
penguins_clean2 <- as_tibble(datasets::penguins) |>
  na.omit() |>
  mutate(sex2 = ifelse(sex == "male", 1, 0))









