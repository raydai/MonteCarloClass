#ref:https://stackoverflow.com/questions/24845909/generate-n-random-integers-that-sum-to-m-in-r

# Generate list (sample size for each group)
rand_vect <- function(group, min=8,max=22,total) {
  vec=runif(n=group, min = min, max = max)
  vec <- round(vec / sum(vec) * total)
  deviation <- total - sum(vec)
  # if the the sum of the total number is large 150, we randomly remove the extra samples in the groups
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(group, 1)] + sign(deviation)
  }
  # check the all the number within the range we specifed.
  while (min(vec)<min){
    N<-length(which(vec==min(vec)))
    vec[which(vec==min(vec))] <- vec[which(vec==min(vec))] + 1
    vec[i] <- vec[i <- sample(which(vec!=min(vec)), N)] - 1
  }
  while (max(vec)>max){
    N<-length(which(vec==max(vec)))
    vec[which(vec==max(vec))] <- vec[which(vec==max(vec))] - 1
    vec[i] <- vec[i <- sample(which(vec!=max(vec)), N)] + 1
  }
  return(vec)
}
listx<-rand_vect(group = 10,min = 8,max = 22,total = 150)
paste(min(listx),max(listx),sum(listx),sep=" ")