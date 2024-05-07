library (tidyverse)
library (tm)
library (tidytext)
library (textclean)
library (hunspell)
library (yardstick)
library (keras)
library (furrr)
library (reticulate)
library (caret)

########################## 
#
# DATA IMPORT, PREP AND CLEAN
#
##########################

df_clean <- read.csv ("cleaned_all.csv", skipNul = TRUE)
word_count <- map_dbl (df_clean$Reviews, function(x) str_split(x, " ") %>% 
                       unlist() %>% 
                       length()
)

df_clean <- df_clean %>% 
  filter (word_count > 3) # ignore reviews shorter than 3 words

set.seed (998) #randomly shuffle the x train dataset
row_data <- nrow (df_clean)
index <- sample (row_data, row_data * 0.8)

prep_x_train <- df_clean[index,] 
prep_x_test <- df_clean[-index,]

paste (prep_x_train$Reviews, collapse = " ") %>% 
       str_split(" ") %>% 
       unlist() %>% 
       n_distinct()

num_words <- 100000 # theres no reason to hard code this but im forced to
maxlen <- 1000	# cap the maximum length to 1000 words

########################## 
#
# TOKENIZE AND VECTORIZE
#
##########################

tokenizer <- text_tokenizer (num_words = num_words) %>% 
  fit_text_tokenizer(prep_x_train$Reviews)

train_x <- texts_to_sequences (tokenizer, prep_x_train$Reviews) %>% 
  pad_sequences (maxlen = maxlen, padding = "pre", truncating = "post")

test_x <- texts_to_sequences (tokenizer, prep_x_test$Reviews) %>% 
  pad_sequences (maxlen = maxlen, padding = "pre", truncating = "post")

# function to convert star integers data into a binary 5 tuple
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

prep_x_train <- prep_x_train %>% 
  mutate ( Stars = Stars %>% future_map (unlist (assignCategoryToInteger))) 
  # future_map turns everything to a list which cant be parsed so undo that

prep_x_test <- prep_x_test %>%
  mutate (Stars = Stars %>% future_map (unlist (assignCategoryToInteger)))

test_y <- prep_x_test$Stars
train_y <- prep_x_train$Stars

########################## 
#
# MODEL DEFINITION 
#
##########################

tensorflow::tf$random$set_seed (123)

model <- keras_model_sequential (name = "lstm_model") %>% 
  layer_embedding (name = "input", # this layer converts the integers into floats of a set size
                   input_dim = num_words,
                   input_length = maxlen,
                   output_dim = 128
  ) %>% 
  layer_dropout (0.7) %>%  # throw away 70% of the inputs randomly
  layer_conv_1d (128, 
				 7, 
				 padding = "valid", 
				 activation = "relu", # anything below 0 is dropped from the layer
				 strides = 3
  ) %>% 
  layer_conv_1d (128, 
				 7, padding = "valid", 
				 activation = "relu", 
				 strides = 3
  ) %>% 
  ########################
  #DO NOT REMOVE THIS LAYER 

  layer_global_max_pooling_1d() %>% # it defaults to pool_size = 2 if no para
  
  #super important magic layer for reading words in context to each
  ########################
  layer_dense (128, 
			  activation = "relu"	# another relu to really make sure no 0s are present
  ) %>% 
  layer_dropout (0.7) %>% 
  layer_dense (name = "output",
               units = 5,				# 5 ouputs for each class (star)
               activation = "softmax"	# sigmoid is binary while softmax is a float
  )

model %>% 
  compile (optimizer = optimizer_adam (learning_rate = 0.001),
           metrics = "categorical_accuracy", # raw accuracy isnt correct, needs categorical
           loss = "categorical_crossentropy" # binary entropy only outputs 1 or 0 which isnt what i need
  )

epochs <- 40 # 22 minutes * 40 = 14 hours training time
batch_size <- 128 # dont go over 256 or else it takes too long and takes too much memory

train_history <- model %>% 
  fit (x = train_x,
       y = as_tensor (train_y),		# for some reason keras doesnt allow raw integers
       batch_size = batch_size,
       epochs = epochs,
       validation_split = 0.2,
       verbose = 1,
       view_metrics = 0
  )

save_model_tf(model, "lstm1")
prediction_test <- predict (model, test_x)

########################## 
#
# CONFUSION MATRIX 
#
##########################

# function to convert prediction testing data into a binary 5 tuple
for (x in 1:nrow(prediction_test))
{
  if (which.max(prediction_test[x,]) == 1)
  {
    prediction_test[x,] = c(1, 0, 0, 0, 0)
  }
  else if (which.max(prediction_test[x,]) == 2)
  {
    prediction_test[x,] = c(0, 1, 0, 0, 0)
  }
  else if (which.max(prediction_test[x,]) == 3)
  {
    prediction_test[x,] = c(0, 0, 1, 0, 0)
  }
  else if (which.max(prediction_test[x,]) == 4)
  {
    prediction_test[x,] = c(0, 0, 0, 1, 0)
  }
  else if (which.max(prediction_test[x,]) == 5)
  {
    prediction_test[x,] = c(0, 0, 0, 0, 1)
  }
}

cf_test_x <- data.frame (prep_x_test$Stars)
cf_test_x <- cf_test_x[,]
p <- prediction_test # temp variable

result <- apply (p, 1, function(row) {which (row == 1)})
cf_test_x <- factor (cf_test_x, levels = c ("1","2","3","4","5")) # extremely tedious to use as factor 
# R for some reason is unwilling to use tables for this

# print out the confusion matrix at last
result <- factor (result, levels = c ("1","2","3","4","5"))
final_res <- confusionMatrix (data = cf_test_x , reference = result)
print (final_res)