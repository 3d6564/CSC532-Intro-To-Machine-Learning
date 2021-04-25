# Parameters for dataset
FLAGS <-flags(flag_numeric("nodes", 128),
              flag_numeric("batch_size", 100),
              flag_string("activation", "relu"),
              flag_numeric("learning_rate", 0.01),
              flag_numeric("epochs", 30)
              )

# Model
model <- keras_model_sequential() %>%
  layer_dense(units=FLAGS$nodes, 
              activation=FLAGS$activation, 
              input_shape=dim(train_data)[2]) %>%
  layer_dense(units=FLAGS$nodes, 
              activation=FLAGS$activation) %>%
  layer_dense(units=1)

# Compile Model
model %>% compile(loss="mse",
                  optimizer=optimizer_adam(lr=FLAGS$learning_rate))

# Train Model
model %>% fit(train_data, 
              train_labels, 
              batch_size=FLAGS$batch_size, 
              epochs=FLAGS$epochs, 
              validation_data=list(val_data, val_labels)
              )

# Predict
predict_labels <- model %>% predict(val_data)

rmse <- function(x,y) {
  return((mean((x-y)^2))^.5)
}

rmse(predict_labels,val_labels)
