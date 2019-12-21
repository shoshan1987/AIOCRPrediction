ID <- c(300645389,305187080)
# Function using keras library
library(keras)

# Train Model
TrainModel <- function(X, y)
{
  
  # Define neural network implementing logistic regression 
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 10, activation = 'linear', input_shape = c(784)) %>% 
    layer_dense(units = 10, activation = 'softmax')
  
  # Compile model 
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_sgd(), # stochastic gradient descent
    metrics = c('accuracy')
  )
  # Fit model
  history <- model %>% fit(
    X, to_categorical(y, 10), 
    epochs = 10, batch_size = 128, 
    validation_split = 0.2
  )
  return(model)
}

# predict labels for test set using learned parameters w   
PredictModel <- function(X, model, prob_flag = FALSE)
{
  if(prob_flag)
  {
    y_hat <- model %>% predict_proba(X)
  }
  else
  {
    y_hat <- model %>% predict_classes(X)
  }  
  return(y_hat)
}