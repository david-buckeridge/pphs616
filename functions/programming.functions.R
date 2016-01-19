extract.mean.value <- function(data.frame, category, category.col, values.col) {
  mean(data.frame[data.frame[,category.col]==category, values.col])
} # extract.mean.value

update.results.frame <- function(results.frame, values.frame, category, category.col, values.col) {
  mean.value = extract.mean.value(values.frame, category, category.col, values.col)
  results.frame[results.frame[,category.col]==category, values.col] = mean.value
  return(results.frame)
} # update.results.frame