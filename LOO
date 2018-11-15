euclideanDistance <- function(u, v){
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance){
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

kNN <- function(xl, z, k){
  
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  
  classes <- orderedXl[1:k, n + 1]
  
  counts <- table(classes)
  
  class <- names(which.max(counts))
  return (class)
}

looFuncOpt <- function(opt_param_min, opt_param_max, opt_param_step, data, class_func){
  count_missclassif = c(rep(0, length( seq(opt_param_min, opt_param_max, opt_param_step) ) ) )

  for(i in c(1:length(data[,1]) ) ){
    xl <- data[-i,] # train data
    z <- data[i,1:2] # object to classify
    
    orderedXl <- sortObjectsByDist(xl, z) # data sorted by distance to z
    
    n <- dim(orderedXl)[2] - 1 
    
    index = 1;
    
    for (opt_param in seq(opt_param_min, opt_param_max, opt_param_step)){
      classes <- orderedXl[1:opt_param, n+1]
      count <- table(classes)
      class <- names(which.max(count))
      
      if(class != data[i,3]){
        count_missclassif[index] = count_missclassif[index] + 1
      }
      index = index + 1
    }  
  }  
  
  return(count_missclassif)
}

looFunc <- function(opt_param_min, opt_param_max, opt_param_step, data, class_func){
  for (opt_param in seq(opt_param_min, opt_param_max, opt_param_step)){
    count = 0
    for (i in c(1:length(data[,1]) ) ){
      xl = data[-i,]
      class <- class_func(xl, data[i,1:2], k=opt_param)
      if(data[i,3] != class){
        count <- count + 1
      }
    }
    loo <- c(loo, count)
  }
  
  return(loo)
}

par(mfrow=c(1,1))

iris30 = iris[3:5]

colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")

loo <- c()

loo <- looFuncOpt(1, length(iris30[,1]), 1, iris30, kNN) / length(iris30[,1])

plot(c(1:length(iris30[,1])),
     loo,
     'p',
     col='blue',
     xlab='k',
     ylab='loo')
lines(c(1:length(iris30[,1])), loo, type="l", pch=22, lty=1, col="red")

opt_k = which.min(loo)
print(opt_k)

iris30_test = iris[,3:5]
accuracy = 0

for(i in c(1:length(iris30_test[,1]))){
  z <- iris30_test[i,1:2]
  class <- kNN(iris30, z, opt_k)
  if(class == iris30_test[i,3]){
    accuracy = accuracy + 1
  }
}

print(accuracy/length(iris30_test[,1]))

points(which.min(loo), min(loo), pch=21, bg = 'red', col = 'red')

plot(iris30[, 1:2], pch = 21, bg = colors[iris30$Species], col
    = colors[iris30$Species], asp = 1, main='6NN', xlab = 'petal length', ylab = 'petal width')

for (xtmp in seq(0, 7, by=0.1)){
 for (ytmp in seq(0, 3, by=0.1)){
   z <- c(xtmp,ytmp)
   class <- kNN(iris30, z, opt_k)
   points(z[1], z[2], pch = 1, col = colors[class])
 }
}
