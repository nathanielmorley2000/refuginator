monteCarlo <- function(entry, decline, duration, nit, summary) {
  it = 1
  score = 0
  count = 0
  for(it in 1:nit){
    vec = floor(runif(nrow(summary), min = 0, max = summary$localities_with_data + 1)) # randomly generate integer for each time bin between 0 and the number of available localities
    indices_greater_than_entry = which(vec >= entry)
    if(length(indices_greater_than_entry) > 1){
      for(i in 1:length(indices_greater_than_entry)){
        count = 0
        if(i + 1 <= length(indices_greater_than_entry)){
          index_lower = indices_greater_than_entry[i]
          index_higher = indices_greater_than_entry[i + 1]
          for(j in index_lower:index_higher){
            if(vec[j] <= decline){
              count <- count + 1
            }
          }
          if(count >= duration) {
            score = score + 1
            break
          }
        }
      }
    }
  }
  return(score)
}