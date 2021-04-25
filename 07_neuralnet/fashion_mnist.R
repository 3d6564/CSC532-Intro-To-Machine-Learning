# Parameters for fashion_mnist dataset
FLAGS <-flags(flag_numeric("nodes", 128),
              flag_numeric("batch_size", 100),
              flag_string("activation", "relu"),
              flag_numeric("learning_rate", 0.01),
              flag_numeric("epochs", 30)
              )

# Model
model <- keras_model_sequential() %>%
  layer_flatten(input_shape=c(28, 28)) %>%
  layer_dense(units=FLAGS$nodes, activation=FLAGS$activation) %>%
  layer_dense(units=10, activation='softmax')

# Compile Model
model %>% compile(optimizer = optimizer_adam(lr=FLAGS$learning_rate), 
                  loss = 'sparse_categorical_crossentropy',
                  metrics = c('accuracy'))

# Train Model
model %>% fit(fashion_train$x, 
              fashion_train$y, 
              epochs=FLAGS$epochs, 
              batch_size=FLAGS$batch_size, 
              validation_data=list(fashion_val$x, fashion_val$y)
              )