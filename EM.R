
# INPUT DATA --------------------------------------------------------------
setwd("/Users/yangh/Desktop/yangh/Courses/PKU/Machine Learning/HW3")
dat <- read.csv("car.csv",sep = ",", header = F)
for(i in 1:7){
  dat[,i] <- as.factor(dat[,i])
}

label.index <- sample(1:nrow(dat), 1000)
label.dat <- dat[label.index,]
unlabel.dat <- dat[-label.index,]
unlabel.dat <- unlabel.dat[,-7]

dat <- label.dat

# Initialization ----------------------------------------------------------
MaxIter <- 100
Threshold <- 0.0001
# number of samples
N <- nrow(dat)
# number of attributes
M <- ncol(dat)-1
# number of labels
K <- as.numeric(levels(as.factor(dat[,7])))
# attribute levels
Q <- apply(dat[,-7], 2, FUN = function(x){as.numeric(levels(as.factor(x)))})
EV <- matrix(data = 0, nrow = N, ncol = length(K))

# 
U <- list(NULL)
# U[[i]][[j]][l]
for(i in 1:N){
  temp <- Q
  for(j in 1:M){
    for(l in 1:length(Q[[j]])){
      temp[[j]][l] <- as.numeric(dat[i,j]==Q[[j]][l])
    }
  }
  U[[i]] <- temp
}
rm(temp)

V <- matrix(data = 0, nrow = N, ncol = length(K))
# V[i,k]
for(i in 1:N){
  for(k in K){
    V[i,k] <- as.numeric(dat[i,7]==k)
  }
}

# Parameters Theta
beta <- vector(mode = "numeric", length = length(K))
# beta[k]
for(k in K){
  beta[k] <- sum(dat[,7]==k)/N
}

alpha <- list(NULL)
# alpha[[k]][[j]][l]
for(k in K){
  alpha[[k]] <- Q
}
for(k in K){
  tmpdat <- dat[which(dat[,7]==k),]
  for(j in 1:M){
    for(l in 1:length(Q[[j]])){
      alpha[[k]][[j]][l] <- sum(tmpdat[,j]==Q[[j]][l])/nrow(tmpdat)
    }
  }
}


# Parameter Initialization ------------------

QFun.now <- 1
QFun.last <- 0


# LOOP --------------------------------------
delta <- 1
loop <- 0
for(it in 1:MaxIter){
  # check convergence
  beta.last <- beta
  if(delta > Threshold){
    loop <- loop+1
    # E step -------------
    # EV[i,k]
    for(i in 1:N){
      for(k in K){
        product <- 1
        for(j in 1:M){
          for(l in 1:length(Q[[j]]))
            product <- product*(alpha[[k]][[j]][l]^U[[i]][[j]][l])
        }
        EV[i,k] <- beta[k]/10^(factor)*product
      }
      EV[i,] <- EV[i,]/sum(EV[i,])
    }
    
    # M step ---------------
    for(k in K){
      beta[k] <- sum(EV[,k])/N
    }
    
    for(k in K){
      for(j in 1:M){
        for(l in 1:length(Q[[j]])){
          temp <- 0
          for(i in 1:N){
            temp <- temp + EV[i,k]*U[[i]][[j]][l]
          }
          alpha[[k]][[j]][l] <- temp/sum(EV[,k])
        }
      }
    }
    rm(temp)
  }
  beta.new <- beta
  delta <- sum((beta.new-beta.last)^2)
}



# Naive Bayesian Clustering
# in unlabeled data
forecast.vote <- matrix(data = 0, nrow = nrow(unlabel.dat), ncol = length(K))
for(i in 1:nrow(forecast.vote)){
  
  for(k in K){
    temp <- 1
    for(j in 1:M){
      l <- as.numeric(unlabel.dat[i,j])
      temp <- temp*alpha[[k]][[j]][l]
    }
    forecast.vote[i,k] <- beta[k]*temp
  }
}
result <- vector(mode = "numeric", length = nrow(unlabel.dat))
result <- apply(forecast.vote, MARGIN = 1, FUN = function(x){which.max(x)})

for(i in 1:4){
  for(j in 4:6){
    test[[i]][[j]][4] <- NA
  }
}
df4 <- as.data.frame(test[[4]])
write.csv(df4, "df4.csv", sep = ",")