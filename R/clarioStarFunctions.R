#' Parser for ClarioStar plate reader data
#'
#' @param data_csv path to csv file from Tecan Spark plate reader
#' @param layout_csv path to csv file containing plate layout information
#' @param n_measurements number of fluorescent or OD measurements (ex. 3 different monochromatic measurements)
#'
#' @return a data.frame containing the parsed plate reader data
#' @export
#'

clario_parse <- function(data_csv, layout_csv, n_measurements) {
  
  if(stringr::str_ends(data_csv, ".xlsx") | stringr::str_ends(data_csv, ".xls")){
    data <- as.data.frame(readxl::read_excel(path = data_csv,
                                             col_names = F,
                                             col_types = "text"))
    data_csv <- gsub(".xlsx", ".csv", data_csv)
  } else if(stringr::str_ends(data_csv, ".csv")){
    data <- utils::read.table(data_csv, sep = ",",
                              na.strings = c(""),
                              header = F,
                              stringsAsFactors = F)
  } else {
    stop("data_csv is must be a .csv, .xls or .xlsx file.")
  }
  
  plate_layout <- utils::read.csv(layout_csv)
  block_starts <-grep('Cycle',data[,1]) # get coordinates of each cycle start in the file
  
  parsed_data<-data.frame()
  for (i in 1:length(block_starts)){
    # get current block start and end points
    block_start_idx <- block_starts[[i]] 
    block_end_idx <- block_start_idx+block_starts[2]
    
    # grab the data only for that measurement
    new_block <- data[(block_start_idx + 1):(block_end_idx-2),]
    parsed_block_data <- data.frame(matrix(ncol=n_measurements, nrow=96))
    
    # get timestamp for that measurement 
    timestamp <- gsub('.*\\(','',data[block_starts[i],1])
    timestamp <- gsub('\\)','',timestamp)
    timestamp_type_h <- grepl('h',timestamp,fixed=T)
    if (timestamp_type_h == T) {
      hours <- as.numeric(sub(" h.*", "", timestamp))
      minutes <- as.numeric(regmatches(timestamp, regexec("h \\s*(.*?)\\s* min", timestamp))[[1]][2])
      seconds <- as.numeric(regmatches(timestamp, regexec("min \\s*(.*?)\\s* s", timestamp))[[1]][2])
    } else {
      hours <- as.numeric(sub(" h.*", "", timestamp))
      minutes <- hours <- as.numeric(sub(" min.*", "", timestamp))
      seconds <- as.numeric(regmatches(timestamp, regexec("min \\s*(.*?)\\s* s", timestamp))[[1]][2])
    }
    
    measurement_names <-c()
    for (j in 0:(n_measurements-1)){
      #get the individual measurement names & add them to the measurement names list
      ind_measur_name <- new_block[j*11+1, 2] 
      measurement_names <- c(measurement_names, ind_measur_name) 
      
      #get the individual measurements, & parse the data for this block into a linear format compatible with the layout
      measurements <-c()
      for (row in 1:8){
        measurements<-c(measurements,as.numeric(new_block[j*11+2+row,-1]))
      } 
      parsed_block_data[j+1]<-measurements
    }
    
    colnames(parsed_block_data) <- measurement_names #rename the columns to the appropriate measurement names
    
    parsed_block_data$time_hours <-hours #add timestamp data
    parsed_block_data$time_minutes <-minutes
    parsed_block_data$time_seconds <-seconds
    
    joined_block <-cbind(plate_layout,parsed_block_data) #join to the provided plate layout
    parsed_data<-rbind(parsed_data,joined_block)
  }

    # write parsed data to csv ------------------------------------------------
  out_name <- gsub(".csv", "_parsed.csv", data_csv)
  utils::write.csv(x = parsed_data, file = out_name, row.names = FALSE)
}