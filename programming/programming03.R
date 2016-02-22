
## 1. Load Data
data.url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv"
gapminder = read.csv(data.url)

### ---- Subsetting -----

## 2. Rows - Find rows for Canada

# Use numeric subscript
# Logical test in parentheses returns logical vector
#  with one entry for each row of the gapminder data
#  frame. The enclosing which() function returns the 
#  vector of row numbers where the logical vector
#  has a TRUE value.
rows = which(gapminder$country == "Canada")
# The vector of row numbers is used as a numerical 
#  subscript to select only those rows, and all 
#  columns (indicated by placing nothing after
#  comma).
cdn.n = gapminder[rows,]

# Use character subscript
# Character subscripts operate on the names of rows
#  or columns. Row names are not as commonly used as
#  column names, but you can use row names to 
#  select a subset of rows.
# Current row names of cdn.n data frame
row.names(cdn.n)
# Use these row names to subscript the data frame
gapminder[row.names(cdn.n),]
# Use the row numbers to subscript the data
#  frame. Note that this is numerical subscripting,
#  as it operates on the integer row indices, not
#  the character row names.
gapminder[241:252,]

# Use logical subscript
cdn.l = gapminder[gapminder$country == "Canada",]


## 3. Columns - Find countries with life expectancy > 80
# Character subscripting is used to select only the column
#  for country. Logical subscriptin is used to identify
#  rows with a life expectancy > 80. The unique() function
#  removes duplicates as some countries have had life
#  expectancy > 80 for multiple years in the data frame.
le.80.subscript = unique(gapminder[gapminder$lifeExp > 80, "country"])


## 4. Use the subset function
# The column subscript does not need to be in quotation marks. This
#  function returns the same class it is passed, so here it returns
#  a data frame.
le.80.subset = unique(subset(gapminder, lifeExp > 80, select=c(country)))


### ---- For Loops -----

## 5. Find countries with LE > 80 by iterating over countries
# Create a vector of the countries over which to iterate
countries = unique(gapminder$country)
# Create an empty vector to hold the names of countries that
#  have a life expectancy > 80. Note that at this point, it 
#  is not clear how many countries will be identified, so the
#  vector is given the largest possible number, the total 
#  number of countries.
le.80.for.country = rep(NA, length(countries))
# Create an indexing variable that will keep track of the 
#  number of countries identified to ensure that the next
#  one identified is place in the correct location of the
#  vector.
insert = 1

for (country in countries) {
  # Obtain the vector of life expetancy for the current country
  le.values = gapminder$lifeExp[gapminder$country == country]
  # Check if at least one value is greater than 80
  if (max(le.values) > 80) {
    # Insert the name of the country in the vector at the 
    #  current location
    le.80.for.country[insert] = country
    # Increment the counter to point at the next location in
    #  the vector
    insert = insert + 1
  } # if
} # for - country

# Trim the end of the vector so that it only contains the entries
#  with country names.
le.80.for.country = le.80.for.country[!is.na(le.80.for.country)]


## 6. Find countries with LE > 80 by iterating over values for LE
# Create an empty vector to hold the names of countries. The
#  iteration will be over rows, so the vector is made the maximum
#  possible size, if every row has a LE > 80
le.80.for.value = rep(NA, nrow(gapminder))

# As opposed to using a list of countries, here, a numerical 
#  vector is used with values corresponding to the row numbers
#  of the data frame
for (row in 1:nrow(gapminder)) {
  # Check if the life expectancy of the current row is > 80
  if (gapminder[row,"lifeExp"] > 80) {
    # Insert the name of the country in the current row of the 
    #  data frame into the current row of the result vector. Note  
    #  that because a numerical vector is being used as a counting  
    #  variable to control the for loop, the counting variable can  
    #  also be used to access the current row of the result vector,
    #  which has the same length as the the number of rows in
    #  the data frame.
    le.80.for.value[row] = as.character(gapminder$country[row])
  }
} # for - row

# Remove the NA values from the results vector and select a
#  unique set of country names.
le.80.for.value = unique(le.80.for.value[!is.na(le.80.for.value)])



### ---- Vectorization -----
# A small function to identify rows in the data frame with 
#  a life expectancy (in column five) greater than 80. This function
#  returns a logical value.
gt.80 <- function(x) {return(x[5] > 80)}
# Use apply to have the function process each row of the data frame,
#  returning a vector of logical values, indicating which rows have
#  life exptancy > 80.
gt.80.apply.true = apply(gapminder, 1, gt.80)

# In-line version, where the function is created in the call to
#  apply and does not remain as a named object in memory after
#  the function returns.
gt.80.apply.true = apply(gapminder, 1, function(x) {x[5] > 80})

# The logical vector is used to subset the column of country
#  names from the data frame. The unique() function is used to
#  remove duplicates.
gt.80.apply = unique(gapminder$country[gt.80.apply.true])


