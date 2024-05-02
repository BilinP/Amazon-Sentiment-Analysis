library(tidyverse)
library(tm)
library(tidytext)
library(textclean)
library(hunspell)
library(yardstick)
library(keras)
library(furrr)
library(reticulate)
library(caret)

df_clean <- read.csv ("cleaned_all.csv", skipNul = TRUE)
word_count <- map_dbl(df_clean$Reviews, function(x) str_split(x, " ") %>% 
                        unlist() %>% 
                        length()
)

df_clean <- df_clean %>% 
  filter(word_count > 3)

set.seed(998) #randomly shuffle the datasets
row_data <- nrow(df_clean)
index <- sample (row_data, row_data*0.8)

data_train <- df_clean[ index, ]
data_test <- df_clean[-index, ]

paste(data_train$Reviews, collapse = " ") %>% 
  str_split(" ") %>% 
  unlist() %>% 
  n_distinct()

num_words <- 100000 # theres no reason to hard code this but im forced to
maxlen <- 1000
tokenizer <- text_tokenizer(num_words = num_words) %>% 
  fit_text_tokenizer(data_train$Reviews)

train_x <- texts_to_sequences(tokenizer, data_train$Reviews) %>% 
  pad_sequences (maxlen = maxlen, padding = "pre", truncating = "post")

test_x <- texts_to_sequences(tokenizer, data_test$Reviews) %>% 
  pad_sequences (maxlen = maxlen, padding = "pre", truncating = "post")

assignCategoryToInteger <- function (x) 
{
  if (x == 1)
  {
    x = c(1, 0, 0, 0, 0)
  }
  else if (x == 2)
  {
    x = c(0, 1, 0, 0, 0)
  }
  else if (x == 3)
  {
    x = c(0, 0, 1, 0, 0)
  }
  else if (x == 4)
  {
    x = c(0, 0, 0, 1, 0)
  }
  else if (x == 5)
  {
    x = c(0, 0, 0, 0, 1)
  }
}

plan (multisession, workers = 8) # use 8 cores to speed up conversion

data_train <- data_train %>%
  mutate (
    Stars = Stars %>% future_map (unlist(assignCategoryToInteger))
  )

data_test <- data_test %>%
  mutate (
    Stars = Stars %>% future_map (unlist(assignCategoryToInteger))
  )

test_y <- data_test$Stars
train_y <- data_train$Stars

tensorflow::tf$random$set_seed(123)

model <- keras_model_sequential(name = "lstm_model") %>% 
  layer_embedding(name = "input",
                  input_dim = num_words,
                  input_length = maxlen,
                  output_dim = 128
  ) %>% 
  layer_dropout(0.7) %>% 
  layer_conv_1d(128, 7, padding = "valid", activation = "relu", strides = 3) %>% 
  layer_conv_1d(128, 7, padding = "valid", activation = "relu", strides = 3) %>% 
  layer_global_max_pooling_1d() %>% 
  layer_dense(128, activation = "relu") %>% 
  layer_dropout(0.7) %>% 
  layer_dense(name = "Output",
              units = 5,
              activation = "softmax"
  )

model %>% 
  compile(optimizer = optimizer_adam(learning_rate = 0.001),
          metrics = "categorical_accuracy",
          loss = "categorical_crossentropy"
  )

epochs <- 39
batch_size <- 128

train_history <- model %>% 
  fit(x = train_x,
      y = as_tensor(train_y),
      batch_size = batch_size,
      epochs = epochs,
      validation_split = 0.2,
      verbose = 1,
      view_metrics = 0
  )

save_model_tf(model, "lstm1")
pred_test <- predict (model, test_x)

for (x in 1:nrow(pred_test))
{
  if (which.max(pred_test[x,]) == 1)
  {
    pred_test[x,] = c(1, 0, 0, 0, 0)
  }
  else if (which.max(pred_test[x,]) == 2)
  {
    pred_test[x,] = c(0, 1, 0, 0, 0)
  }
  else if (which.max(pred_test[x,]) == 3)
  {
    pred_test[x,] = c(0, 0, 1, 0, 0)
  }
  else if (which.max(pred_test[x,]) == 4)
  {
    pred_test[x,] = c(0, 0, 0, 1, 0)
  }
  else if (which.max(pred_test[x,]) == 5)
  {
    pred_test[x,] = c(0, 0, 0, 0, 1)
  }
}

test <- data.frame(data_test$Stars)
test <- test[,]
pred <- pred_test

result <- apply(pred, 1, function(row) {
  which(row == 1)
})

test <- factor(test, levels = c("1","2","3","4","5"))
result <- factor(result, levels = c("1","2","3","4","5"))
conf_matrix <- confusionMatrix(data = test , reference = result)
print (conf_matrix)