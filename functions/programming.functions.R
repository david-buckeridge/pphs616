# A function to return the mean value of the entries in the values.col from a
#  data.frame, where the category.col is equal to category. 
extract.mean.value <- function(data.frame, category, category.col, values.col) {
  mean(data.frame[data.frame[,category.col]==category, values.col])
} # extract.mean.value

# A function to update a results.frame by overwriting it with a new version where
#  the entry in the values.col is set to the mean of the values in values.frame,
#  values.col where category.col is equal to category.
update.results.frame <- function(results.frame, values.frame, category, category.col, values.col) {
  mean.value = extract.mean.value(values.frame, category, category.col, values.col)
  results.frame[results.frame[,category.col]==category, values.col] = mean.value
  return(results.frame)
} # update.results.frame