# Parameters for dataset
FLAGS <-flags(flag_numeric("nodes", 128),
              flag_numeric("batch_size", 100),
              flag_string("activation", "relu"),
              flag_numeric("learning_rate", 0.01),
              flag_numeric("epochs", 30)
              )

# Model
model <- keras_model_sequential() %>%
  layer_dense(units=FLAGS$nodes, activation=FLAGS$activation, input_shape=c(10000)) %>%
  layer_dense(units=FLAGS$nodes, activation=FLAGS$activation) %>% # 128 neurons
  layer_dense(units=46, activation='softmax') # 46 category classifiers

# Compile Model
model %>% compile(optimizer = optimizer_adam(lr=FLAGS$learning_rate), 
                  loss = 'categorical_crossentropy',
                  metrics = c('accuracy'))

# Train Model
model %>% fit(train_data, 
              train_labels, 
              epochs=FLAGS$epochs, 
              batch_size=FLAGS$batch_size, 
              validation_data=list(val_data, val_labels)
              )