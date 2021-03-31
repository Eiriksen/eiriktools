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

