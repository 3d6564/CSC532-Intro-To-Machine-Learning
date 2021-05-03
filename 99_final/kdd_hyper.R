# Parameters for dataset
FLAGS <-flags(flag_numeric("nodes", 30),
              flag_numeric("batch_size", 100),
              flag_string("activation", "relu"),
              flag_numeric("learning_rate", 0.01),
              flag_numeric("epochs", 30)
              )

# Model
model <- keras_model_sequential() %>%
  layer_dense(units=FLAGS$nodes, activation=FLAGS$activation, input_shape=dim(kdd_train_x_ohe)[2]) %>%
  layer_dense(units=FLAGS$nodes, activation=FLAGS$activation) %>% 
  layer_dense(units=5, activation='softmax') # 5 category classifiers

# Compile Model
model %>% compile(optimizer = optimizer_adam(lr=FLAGS$learning_rate), 
                  loss = 'categorical_crossentropy',
                  metrics = c('accuracy'))

# Train Model
model %>% fit(as.matrix(kdd_train_x_ohe), as.matrix(kdd_train_y_ohe), 
              batch_size=FLAGS$batch_size,
              epochs=FLAGS$epochs, 
              validation_data=list(as.matrix(kdd_val_x_ohe), as.matrix(kdd_val_y_ohe))
              )