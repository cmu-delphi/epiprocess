#' @export
#' @noRd
Min = function(x) min(x, na.rm = TRUE)

#' @export
#' @noRd
Max = function(x) max(x, na.rm = TRUE)

#' @export
#' @noRd
Sum = function(x) sum(x, na.rm = TRUE)

#' @export
#' @noRd
Mean = function(x) mean(x, na.rm = TRUE)

#' @export
#' @noRd
Median = function(x) median(x, na.rm = TRUE)

##########

Start = function(x) x[1]
End = function(x) x[length(x)]
MiddleL = function(x, floor = TRUE) x[floor(length(x)/2)]
MiddleR = function(x, floor = TRUE) x[ceiling(length(x)/2)]
ExtendL = function(x) c(Start(x), x)
ExtendR = function(x) c(x, End(x))

##########

#' @export
#' @noRd
quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

##########

guess_geo_type = function(geo_value) {
  if (is.character(geo_value)) {
    # Convert geo values to lowercase
    geo_value = tolower(geo_value)
      
    # If all geo values are state abbreviations, then use "state" 
    state_values = c(tolower(state.abb), "as", "dc", "gu", "mp", "pr", "vi")
    if (all(geo_value %in% state_values)) return("state")

    # Else if all geo values are 2 letters, then use "nation"
    else if (all(grepl("[a-z]{2}", geo_value))) return("nation")

    # Else if all geo values are 5 numbers, then use "county"
    else if (all(grepl("[0-9]{5}", geo_value))) return("county")
  }

  else if (is.numeric(geo_value)) {
    # Convert geo values to integers
    geo_value = as.integer(geo_value)

    # If the max geo value is at most 10, then use "hhs"
    if (max(geo_value) <= 10) return("hhs")
      
    # Else if the max geo value is at most 457, then use "hrr"
    if (max(geo_value) <= 457) return("hrr")
  }

  # If we got here then we failed
  return("custom")
}

guess_time_type = function(time_value) {
  # Convert character time values to Date or POSIXct
  if (is.character(time_value)) {
    if (nchar(time_value[1]) <= "10") {
      new_time_value = tryCatch({ as.Date(time_value) },
                                error = function(e) NULL)
    }
    else {
      new_time_value = tryCatch({ as.POSIXct(time_value) },
                                error = function(e) NULL)
    }
    if (!is.null(new_time_value)) time_value = new_time_value
  }
    
  # Now, if a POSIXct class, then use "day-time"
  if (inherits(time_value, "POSIXct")) return("day-time")

  # Else, if a Date class, then use "week" or "day" depending on gaps 
  else if (inherits(time_value, "Date")) {
    return(ifelse(all(diff(sort(time_value)) == -7), "week", "day"))
  }

  # Else, check whether it's one of the tsibble classes
  else if (inherits(time_value, "yearweek")) return("yearweek")
  else if (inherits(time_value, "yearmonth")) return("yearmonth")
  else if (inherits(time_value, "yearquarter")) return("yearquarter")

  # Else, if it's an integer that's at least 1582, then use "year"
  if (is.numeric(time_value) &&
      all(time_value == as.integer(time_value)) &&
      all(time_value >= 1582)) {
    return("year")
  }
      
  # If we got here then we failed
  return("custom")
}

##########

# Create an auto-named list
enlist = function(...) {
  x = list(...)
  n = as.character(sys.call())[-1]
  if (!is.null(n0 <- names(x))) {
    n[n0 != ""] = n0[n0 != ""]
  }
  names(x) = n
  return(x) 
}

# Variable assignment from a list. NOT USED. Something is broken, this doesn't 
# seem to work completely as expected: the variables it define don't propogate
# down to child environments  
list2var = function(x) {
  list2env(x, envir = parent.frame())
}
