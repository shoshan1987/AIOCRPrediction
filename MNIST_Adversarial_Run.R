# Relevant libraries in R  
library(R.utils)
library(keras)

rm(list=ls()) # make the memory bigger 

setwd("C:/Users/shoshan1987/Desktop/RonRon/")
source('MNIST_functions_keras-multinomial.R')
source('Adversarial_Image_Generator_RonRon.R') 
# loading data and model 
load(file='AdversarialTest.RData') 
m.image <- load_model_hdf5('SoftMaxNetwork.h5') 
epsilon <- 0.05  
m_test <- length(A$y)

# loop, then adversarial examples and accuracy: 
A$adv_accuracy <- matrix(0, m_test, 1)
A$orig_accuracy <- matrix(0, m_test, 1)
for(i in 1:m_test) # length(J))
{
  if((i%%10) == 0) # track progress 
    print(i)
  X_adv <- PerturbExample(A$X[i,], A$y_adv[i], epsilon, m.image) # make perturbation 
  A$adv_accuracy[i] <- PredictModel(X_adv , m.image, 1)[A$y_adv[i]+1]  # record accuracy - predicted probabilty of y_adv for X_adv
  A$orig_accuracy[i] <- PredictModel(t(as.matrix(A$X[i,])) , m.image, 1)[A$y_adv[i]+1]  # record accuracy - predicted probabilty of y_adv for X_adv  
} 

mean(A$orig_accuracy) # mean probability of adversarial label for original images 
mean(A$adv_accuracy) # mean probability of adversarial label for adversarial images  

hist(A$adv_accuracy, 10)