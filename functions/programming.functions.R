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


# A function to retrieve from a data.frame (data) the values from the specified 
#   column(s) (value.col), which correspond the specified country (category) 
#   in the country column (category.col).
get.values <- function(data, category.col, category, value.cols) {
  # build the empty results data object with columns corresponding to value.cols
  
  if (length(value.cols) > 0) {
    n.value = nrow(data[data[,category.col] ==  category,])
    values = data.frame()
    for (value.col in value.cols) {
      values[,value.col] = numeric(n.value)
    } # for
    
  } else {
    stop("no value.col identified")
  }
  
  
  for (value.col in value.cols) {
    values[,value.col] = get.value(data, category.col, category, value.col)
  } # for
  
  return(values)
  
} # get.values


# A function to retreive from a data.frame (data) the values from the specified
#   column (value.col)
get.value <- function(data, category.col, category, value.col) {
  # ensure that we have only value.col to process
  if (length(value.col) != 1) stop("exactly one value.col only")
  
  value =  data[data[,category.col] == category, value.col]
  
  return(value)
} # get.values.entry