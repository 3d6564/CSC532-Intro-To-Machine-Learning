# Parameters for dataset
FLAGS <-flags(flag_numeric("nodes", 128),
              flag_numeric("batch_size", 100),
              flag_string("activation", "relu"),
              flag_numeric("learning_rate", 0.01),
              flag_numeric("epochs", 100),
              flag_numeric("dropout1", .5),
              flag_numeric("dropout2", .5)
)

# Model
model = keras_model_sequential() %>%
  layer_dense(units=FLAGS$nodes, 
              activation=FLAGS$activation,
              input_shape=dim(housing_train_n)[2]) %>%
  layer_dropout(FLAGS$dropout1) %>%
  layer_dense(units=FLAGS$nodes, 
              activation=FLAGS$activation) %>%
  layer_dropout(FLAGS$dropout2) %>%
  layer_dense(units=1)

# Compile Model
model %>% compile(loss="mse",
                  optimizer=optimizer_adam(lr=FLAGS$learning_rate)
)

# Train Model
history <- model %>% fit(housing_train_n, 
                         housing_train_labels, 
                         batch_size=FLAGS$batch_size, 
                         epochs=FLAGS$epochs, 
                         validation_data=list(housing_val_n, housing_val_labels)
)

# Predict
predict_labels <- model %>% predict(housing_test_n)

rmse <- function(x,y) {
  return((mean((x-y)^2))^.5)
}

rmse(predict_labels,housing_test_labels)
