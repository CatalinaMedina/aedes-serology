library(stringr) # for extracting elements of column names
library(lubridate) # for days() function


process_modis_data <- function(
    df_og,            # modis data
    sat_var_names,    # vector of names of satellite variables
    sat_var_col_start # column number which contains the first satellite index (columns expected to be ordered as non satellite variables followed by satellite variables)
  ){
  
  df_og <- data.frame(df_og) 
  num_vars <- length(sat_var_names)
  
  sat_cols <- sat_var_col_start:ncol(df_og) # identifies satellite columns
  n_sat_cols <- length(sat_cols)
  non_sat_cols <- 1:(sat_var_col_start - 1) # identifies non satellite cols
  
  
  new_df <- data.frame(matrix( # data frame to be returned
    NA, 
    nrow = nrow(df_og) * n_sat_cols, # each individual gets n_sat_cols rows (one for each measurement) 
    ncol = length(non_sat_cols) +  3 # columns = c(non satellite columns, "date", "var_type", "sat_var_value")
  ))
  
  colnames(new_df) <- c(
    colnames(df_og)[non_sat_cols], 
    "date", "var_type", "sat_var_value"
  )
  
  
  col_names <- colnames(df_og[, sat_cols]) # extract names of satellite columns
  
  var_type <- str_sub(col_names, start = 1, end = 3) # first 3 characters identified as satellite variable type
  new_df$var_type <- rep(var_type, nrow(df_og)) # store satellite variable type
  
  new_df$date <- as.Date(new_df$date)
  year_seq <- paste0("20", str_sub(col_names, start = 5, end = 6)) # fifth and six characters identified as year
  days_seq <- str_sub(col_names, start = 8, end = 10) # 8th-10th characters identified as number of days into year
  date_seq <- as.Date(paste0(year_seq, "-01-01")) + days(as.numeric(days_seq) - 1) # "2020-01-01" + number of days into year - 1; (1 since "2020-01-01" starts on day 1)
  new_df$date <- rep(date_seq, nrow(df_og)) # store date of satellite variable measurement
  
  
  new_row <- 1 # keeps track of row in new data frame
  
  for(i in 1:nrow(df_og)) { # loops over all rows (individuals) from original data frame
    rows_for_id <- new_row:(new_row + n_sat_cols - 1) # identifies all rows for individual i in new data frame
    
    new_df[rows_for_id, non_sat_cols] <- as.data.frame(lapply(
      df_og[i, non_sat_cols], 
      rep, 
      n_sat_cols
    )) # non_sat_cols info for individual i into all of their rows in new data frame
    
    new_df[rows_for_id, "sat_var_value"] <- as.numeric(df_og[i, sat_cols]) # extract satellite variables' values for individual i
    
    new_row <- new_row + n_sat_cols # moves index for rows in new data frame
  }
  
  new_df 
}


# example of satellite variable matching by date in aedes-serology-investigation.Rmd file in "read-process-data" chunk