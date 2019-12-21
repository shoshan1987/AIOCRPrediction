ID <- c(300645389,305187080) # students ID here 

PerturbExample <- function(X, y, epsilon, m.image) 
{
  p <- 28 # assumed image length 
  w <- get_weights(m.image)
  grad <- w[[1]][,y+1] / sum(abs(w[[1]][,y+1]))
  X_adv <- X + grad/ sum(abs(grad))*0.019
  X_adv <- t(as.matrix(pmin(1, pmax(0, X_adv))))
  index = 1:28
  for (i in 1:14){
    diff = matrix(0,nrow = 1,ncol = 784)
    prob = PredictModel(as.matrix(X_adv) , m.image, 1)[y+1]
    for (j in index){
      temp <- X_adv
      temp[j] <- temp[j] + 0.01   
      probY = PredictModel(X = as.matrix(temp),model =  m.image,1)[y+1]
      diff[j] = (probY-prob)/0.01
    }
    X_adv <- X_adv + diff / sum(abs(diff))*0.02
    X_adv <- t(as.matrix(pmin(1, pmax(0, X_adv))))
    index = index+28
  }
  d = sum(abs(X_adv+-X))/(p*p)
  X_adv = X + (X_adv-X) * epsilon/d
  X_adv <- t(as.matrix(pmin(1, pmax(0, X_adv))))
  return(X_adv)
}

