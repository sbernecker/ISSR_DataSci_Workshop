
myvec = rep(0, times = 100)
myvec[71] = 1

haventFoundTheOne = TRUE

counter = 1
while(haventFoundTheOne & counter < 500){
  print(counter)
  if(myvec[counter > 0]){
    cat("The one is in entry", counter)
    haventFoundTheOne = FALSE
  }
  counter = counter + 1
}


myMatrix = matrix(1:400, 20, 20)

#rowSums = function(matrix){
#  vector = rep(0)
#}

Row_Sums <- function(matrix){
  vector <- rep(0, length(matrix[,1]))
  for(i in 1:length(matrix[,1])){
    vector[i] <- sum(matrix[i,]) 
  }
  return(vector)
}
# This is for testing what happens when you create an vector of zeroes, versus nulls, versus an empty vector
Bad_Sums1 <- function(matrix){
  vector <- rep(0, length(matrix[,1]))
  for(i in c(1,3,5)){
    vector[i] <- sum(matrix[i,]) 
  }
  return(vector)
}

#Output
#Bad_Sums1(myMatrix)
# [1] 3820    0 3860    0 3900    0    0    0    0    0    0    0
# [13]    0    0    0    0    0    0    0    0

Bad_Sums2 <- function(matrix){
  vector <- rep(NULL, length(matrix[,1]))
  for(i in c(1,3,5)){
    vector[i] <- sum(matrix[i,]) 
  }
  return(vector)
}
#Output
#Bad_Sums2(myMatrix)
#[1] 3820   NA 3860   NA 3900

Bad_Sums3 <- function(matrix){
  vector <- c()
  for(i in c(1,3,5)){
    vector[i] <- sum(matrix[i,]) 
  }
  return(vector)
}

# Output
# > Bad_Sums3(myMatrix)
# [1] 3820   NA 3860   NA 3900






