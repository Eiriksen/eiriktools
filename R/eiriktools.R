#' Converts anything to a numeric
#' @export
numextract <- function(string){
  require(stringr)
  as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
}


#' Converts anything to an integer
#' @export
intextract <- function(string){
  require(stringr)
  as.integer(str_extract(string, "\\-*\\d+\\.*\\d*"))
}


#' Stateswitch
#' @export
stateSwitch = function(x) {
  if (sum(x == T) == 0) return(x)
  if (sum(x != T) == 0) return(x)

  first_true <- which.max(x)
  output <- c(rep(F,first_true-1),rep(T,length(x)-first_true+1))
  output
}

#' Stateswitch_last
#' @export
stateSwitch_last = function(x) {
  if (sum(x == T) == 0) return(rep(F,length(x)))
  if (sum(x != T) == 0) return(rep(T,length(x)))

  last_false <- tail(which(x==FALSE),1)
  output <- c(rep(F,last_false),rep(T,length(x)-last_false))
  output
}



#' @title Standard error of the mean
#' @description  Ignores NA values
#' @export
sem <- function(x) sqrt(var(x, na.rm=T)/(sum(!is.na(x))))

#' @title Mean and standard error
#' @description Reports a character string with the mean and  standard error
#' @export
meanSem = function(var,a=2,b=2){ return(paste(  round(mean(var,na.rm=T),a),round(sem(var),b),sep="+-")) }


#' @title Mean and standard deviation
#' @description Reports a character string with the mean and  standard error
#' @export
meanSD = function(var,a=2,b=2){ return(paste(  round(mean(var,na.rm=T),a),round(sd(var),b),sep="+-")) }


#' @title List dates between two dates
#' @description Function for gaining a list of all dates between two dates. Sollution to this problem by user "yifyan" at stackoverflow.com  https://stackoverflow.com/questions/14450384/create-a-vector-of-all-days-between-two-dates. date_a and date_b must be lubridate-date-objects
#' @export
datesBetween = function(date_a,date_b) {
  require(lubridate)

  n_days <- interval(date_a,date_b)/days(1)
  dates = date_a + days(0:n_days)
  return(dates)
}


# Calculate the number of siblings for each fish
#' @export
siblings_count = function(df)
{
  sibs <- df %>% apply(MARGIN=1,FUN=function(x){
    tfam = x[["ID_family"]]
    ttank= x[["tank"]]
    sibs = df %>% filter(ID_family == tfam & ttank == tank) %>% nrow()
    sibs
  })

  df$sibs = sibs
  df
}


#' @export
invert = function(x)
{
  (max(x)-x)+1
}


# Credit: Kevin Ushey https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows
# Ex:
#
# mtcars %>% mutate_when(
#   mpg > 22,    list(cyl = 100),
#   disp == 160, list(cyl = 200)
# )

#' @export
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}



#' Converts messy names and ID's to tidy clean ones.
#'
#' For sorting out a vector with long and complicated identifiers or row names, where the true ID of a row is hidden in a string.\cr
#' E.g: Make "dirty" ID's like "A0006_3911_BT-F1_GTCGTCTA_run20190930N" turn into "clean" ID's like 3991_BT
#' @param vector A vector of "dirty" IDs
#' @param identifier ID's need to be formated with a number and following identifier, e.g "34_individuals2019" where "_individuals2019" is the identifier. Any entries not matching this format will be removed.
#' @param identifier_left Wether the identifier is on the left hand (T) or right-hand (R) side of the number
#' @param numLength if you want leading zeroes, use this parameter to specify the length of the number, e.g "8" for 00000342
#' @param prefix if you want a prefix in the new cleaned ID. Ex: "individuals2019_" will give you "individuals2019_0034". If not specified, the old identifier will be used instead. Set to NA if you only want the number.
#' @param remove_NA if you want to remove any entries that don't follow your pattern (otherwise, they'll turn to NA)
#' @export
clean_ID = function(vector,identifier="", identifier_left=F, numLength=4, prefix, remove_NA=F,numeric=F) {
  require(tidyverse)
  require(stringr)

  # SET THE REGULAR EXPRESSION
  if (!identifier_left) regExpr = paste("[0-9]{1,50}",identifier,sep="")
  else                  regExpr = paste(identifier,"[0-9]{1,50}",sep="")

  # Extract the ID's from the dirty ID's
  ID_dirty = vector
  ID_clean = ID_dirty %>% str_extract(regExpr)

  # Remove the old identifier (for now)
  ID_clean = ID_clean %>% sub(identifier,"",.)

  # Remove NA values
  if (remove_NA) ID_clean = ID_clean[!is.na(ID_clean)]

  # Add leading zeroes
  if (numLength!=0) ID_clean[!is.na(ID_clean)] = ID_clean[!is.na(ID_clean)] %>% as.numeric() %>% sprintf(paste("%0",numLength,"d",sep=""),.)

  # Make the ID completely numeric
  if (numeric) ID_clean = as.numeric(ID_clean)

  # Add the new prefix
  if (exists("prefix")){
    if (is.na(prefix))       return(ID_clean)
    else                     ID_clean[!is.na(ID_clean)] = paste(prefix, ID_clean[!is.na(ID_clean)], sep="")
  }
  else if (identifier_left)  ID_clean[!is.na(ID_clean)] = paste(ID_clean[!is.na(ID_clean)], identifier, sep="")
  else if (!identifier_left) ID_clean[!is.na(ID_clean)] = paste(identifier, ID_clean[!is.na(ID_clean)], sep="")

  return(ID_clean)
}


#' In a dataframe, converts messy names and ID's to tidy clean ones.
#'
#' For sorting out column with long and complicated identifiers or row names, where the true ID of a row is hidden in a string.\cr
#' E.g: Make "dirty" ID's like "A0006_3911_BT-F1_GTCGTCTA_run20190930N" turn into "clean" ID's like 3991_BT
#' @param df The data frame
#' @param column The name of a column containing dirty IDs
#' @param identifier ID's need to be formated with a number and following identifier, e.g "34_individuals2019" where "_individuals2019" is the identifier. Any entries not matching this format will be removed.
#' @param identifier_left Wether the identifier is on the left hand (T) or right-hand (R) side of the number
#' @param numLength if you want leading zeroes, use this parameter to specify the length of the number, e.g "8" for 00000342
#' @param prefix if you want a prefix in the new cleaned ID. Ex: "individuals2019_" will give you "individuals2019_0034"
#' @param remove_NA if you want to remove any rows that don't follow your pattern (otherwise, they'll turn to NA). Default is True.
#' @export
clean_ID_df = function(df, column_name="ID", identifier="", identifier_left=F, numLength=F, prefix="", remove_NA=T, keep_name=F, numeric=F){
  require(tidyverse)
  require(stringr)

  # Ectract the dirty ID's
  ID_dirty = unlist(df[column_name])

  # Clean the ID
  ID_clean = clean_ID(ID_dirty, identifier, identifier_left, numLength, prefix,numeric=numeric)

  # Insert the cleaned ID's into the column
  df[column_name] = ID_clean

  # Remove NA values
  if (remove_NA) df = df %>% na_removeRow(column_name)

  # Rename the old ID column
  # Check what name to use
  if (keep_name == F) column_name_new = "ID"
  else if (keep_name == T) column_name_new = column_name
  else column_name_new = keep_name
  # Rename the column to "ID"
  df = df %>% rename(!! column_name_new := !! column_name)

  return(df)
}


#' @export
duplicates_cut = function(df, by, na.keep=T)
{
  if (na.keep)
  {
    keep = df %>% filter(is.na(.data[[by]]))
    df   = df %>% filter(!is.na(.data[[by]]))
  }
  else
  {
    keep = data.frame()
  }

  df <- df %>% group_by(.data[[by]]) %>% summarise_all(funs(mergeDuplicates_last))

  df <- rbind(df,keep)

  df

}

#' @export
duplicates_find = function(df, by, na.omit=T)
{
  if (na.omit)
  {
    df[!is.na(df[[by]]) & df[[by]] %>% duplicated(),][[by]]
  }
  else
  {
    df[df[[by]] %>% duplicated(),][[by]]
  }

}


#' Removes rows with NA in a given column
#'
#' Removes NA rows (in a given column) from a dataset
#' @export
na_removeRow = function(dataset,columns){
  for (column in columns)
  {
    dataset = dataset[which(!is.na(dataset[column])),]
  }
  return(dataset)
}

#' funkyTranspose
#' https://stackoverflow.com/questions/6645524/what-is-the-best-way-to-transpose-a-data-frame-in-r-and-to-set-one-of-the-column
#' Credits: mortonjt and nzcoops
funkyTranspose = function(df){
  # Transpose table YOU WANT
  df_t <- t(df[,2:ncol(df)])
  # Set the column headings from the first column in the original table
  colnames(df_t) <- t(df[,1])
  return(df_t)
}


#' @export
duplicates_cut_adv = function(df, lim_coeff=15)
{
  duplicates = df %>% duplicates_find(by="pit")
  for (dupl in duplicates){
    print(glue("Pit: {dupl}"))
    weights = df[df$pit==dupl,]["weight"] %>% na_removeRow("weight")
    print(glue("Weights {paste(weights, collapse='  ')}"))
    tanks   = df[df$pit==dupl,]["tank"]   %>% na_removeRow("tank")
    print(glue("Tanks: {paste(tanks, collapse='  ')}"))
    mords=df[df$pit==dupl,]["measOrder"] %>% na_removeRow("measOrder")
    print(glue("Mords: {paste(mords, collapse='  ')}"))

    coeff = sd(weights) / mean(weights) * 100
    if (is.na(coeff)) next

    if (length(unique(tanks))==1 & coeff < 15)
    {
      # delete all entries from the dataset except the first
      mords_delete = mords[2:length(mords)]
      print(glue("Deleted {paste(mords_delete,collapse=', ')} in tank {unique(tanks)}"))
      df = df %>% filter(!measOrder %in% mords_delete)
    }
  }
  return(df)
}
