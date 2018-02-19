# load the runs for the indicated ids without having to rely on directory structure
#  input: directory is the realtive path to the datafiles, runids is a vector
#         of runids to load.
#  output: a list of data.frames contain data for inidcated runs
load.runs <- function(directory, runids, os="mac") {
  
  if (os == "mac") seperator = "/"
  else seperator = "\\"
  
  filenames = list.files(directory, pattern="*.csv", full.names=TRUE)
  
  # split on seperator
  tokens = strsplit(filenames, seperator)
  
  if (length(tokens) > 1) {
    n.tokens = length(tokens[[1]])
  } else {
    stop("load.runs: Cannot access filenames for runs - check directory path.")
  } # check token size
  
  # lapply function to extract last token and remove last four characters (.csv)
  filenames.tocheck = lapply(tokens, function (token) {substring(token[[n.tokens]], 1, nchar(token[[n.tokens]])-4)})
  # extract the filenames matching the runids
  filenames.toread = filenames[filenames.tocheck %in% runids]
  
  return(lapply(filenames.toread, read.csv))
} # load.runs