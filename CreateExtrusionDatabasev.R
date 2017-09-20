#This file contains the functions to build up the extrusion database
#Can use ParsingFunctionsv7.09
require(xlsx)
require(readxl)
require(purrr)

source("C:/Users/correaf/Documents/ProgrammingAtWork/ExtrusionDatabase/ParsingFunctions/ParsingFunctionsv8.07.R")


createDatabase <- function(directory){
  #creates the database of the parts and returns the dataframe.
  setwd(directory) # sets the working directory to the directory. This way the files can be 
  #accessed by just their respective filenames and not a full workingpath
  file_list <- list.files(directory, pattern = "\\.xls") #lists all the files in the directory
  #by searching the pattern '.xls', '.xlsx' is also included.
  
  
  
  ##### Create empty dataframes ####
  #create the empty dataframe to store the single extrusion values
  single_output_names <- c("Part Number", "PPS Number", "Die Size (in)", "Die Land Length (in)",
                           "Tip Size (in)","Tip Land Length (in)", "Screw Print",
                    "Feedthroat Temperature (F)", "Barrel Zone 1 Temperature (F)", 
                    "Barrel Zone 2 Temperature (F)", "Barrel Zone 3 Temperature (F)", 
                    "Clamp Temperature (F)","Adapter Temperature (F)", "Die 1 Temperature (F)", 
                    "Die 2 Temperature (F)", "Inner Diameter (in)", "Outer Diameter (in)", 
                    "Wall Thickness (in)", "Out-of-Roundness (in)", "Concentricity (in)", "Length (in)", 
                    "Perpendicularity (in)")
  database <- data.frame(matrix(ncol = length(single_output_names), nrow = 0), 
                         stringsAsFactors = FALSE) # seventeen outputs
  colnames(database) <- single_output_names #assigns column names
  
  #creates an empty data frame to store the tapered extrusion values
  tapered_output_names <- c("Part Number", "PPS Number","Die Size (in)", "Die Land Length (in)",
                            "Tip Size (in)","Tip Land Length (in)", "Screw Print",
                             "Feedthroat Temperature (F)", "Barrel Zone 1 Temperature (F)", 
                             "Barrel Zone 2 Temperature (F)", "Barrel Zone 3 Temperature (F)", 
                             "Clamp Temperature (F)","Adapter Temperature (F)", "Die 1 Temperature (F)", 
                             "Die 2 Temperature (F)",
                             "Proximal Inner Diameter (in)", "Proximal Outer Diameter (in)", 
                             "Proximal Wall Thickness (in)", "Proximal Out-of-Roundness (in)", 
                             "Proximal Concentricity (in)", "Proximal Perpendicularity (in)",
                             "Distal Inner Diameter (in)", "Distal Outer Diameter (in)", 
                             "Distal Wall Thickness (in)", "Distal Out-of-Roundness (in)", 
                             "Distal Concentricity (in)", "Distal Perpendicularity (in)",
                             "Proximal Length (in)", "Distal Length (in)", "Transition Length (in)", 
                             "Total Length (in)", "Proximal and Transition Length (in)")
  tapered_database <- data.frame(matrix(ncol = length(tapered_output_names), nrow = 0),
                                 stringsAsFactors = FALSE) #twenty-six outputs
  colnames(tapered_database) <- tapered_output_names #assign column names
  
  #creates an empty data frame to store the multi extrusion values
  multi_output_names <- c()
  multi_database <- data.frame(matrix(ncol = length(multi_output_names), nrow = 0),
                               stringsAsFactors = FALSE) #twenty-six outputs
  
  
  #### Debugging Trackers ####
  
  braiding_PPS_count <- 0 #counts how many braiding PPS documents were encountered
  single_PPS_count <- 0 #counts how many single extrusion PPS documents were encountered
  multiext_PPS_count <- 0 #counts how many multi-extrusion PPS documents were encountered
  taperedext_PPS_count <- 0 #counts how many tapered extrusion PPS documents were encountered
  miscellaneous_PPS_count <- 0 #counts PPS documents encountered that are not of the other three types
  missed_PPS_count <- 0 #means that the PPS typed was single, but it failed criteria to be analyzed
  
  
  #for debugging purpose
  braiding_df <- data.frame(matrix(nrow = 100, ncol = 2), stringsAsFactors = FALSE)
  single_df <- data.frame(matrix(nrow = 700, ncol = 2),stringsAsFactors = FALSE)
  multiext_df <- data.frame(matrix(nrow = 150, ncol = 2),stringsAsFactors = FALSE)
  tapered_df <- data.frame(matrix(nrow = 150, ncol = 2), stringsAsFactors = FALSE)
  miscellaneous_df <- data.frame(matrix(nrow = 50, ncol = 2), stringsAsFactors = FALSE)
  missed_df <- data.frame(matrix(nrow = 50, ncol = 3), stringsAsFactors = FALSE)
  
  #for the try and catch block in importPPS
  try_count <- 0
  error_count <- 0
  warning_count <- 0
  

  directory_count <- 1 #count to run through the file_list
  
  #### Analysis of PPS Documents ####
  
  while (directory_count < (length(file_list) + 1)){
    #while loop that goes through each excel file and pulls out the necessary data
    file_name <- file_list[directory_count]
    print(paste0("Running File: ", file_name))
    
    sheet_names <- excel_sheets(file_name) #assigns sheet names
    PPS_number <- parsePPSNumber(file_name) #parses the PPS file name for just
    #the number
    
    sheet_count <- 1

    while (sheet_count < (length(sheet_names) + 1)){
      #runs through each sheet/tab in the excel file to get the data
      
      print(paste0("Running Sheet: ", sheet_names[sheet_count]))
      sheet_name <- sheet_names[sheet_count]
      
      
      #Conditions to break out of the sheet
      if (identical(sheet_name, "History Sheet") == TRUE){
        #breaks out if for whatever unknown, unfathomable reason, the creator of the PPS document
        #has chosen in absurdity to have a History Sheet
        break
      }
      
      
      importPPS_list <- importPPS(file_name, sheet_name, try_count, error_count, warning_count)
      PPS <- importPPS_list$'PPS'
      try_count <- importPPS_list$'Try'
      error_count <- importPPS_list$'Error'
      warning_count <- importPPS_list$'Warning'
      
      if (is.null(PPS)){
        #NA is the code for the scenario that the sheet was null in importPPS
        break
      }
      
      #make the PPS smaller if there are a lot of empty rows and columns
      if (length(nrow(PPS)) != 0 && nrow(PPS) > 200){
        PPS <- PPS[1:200,]
      } #end if
      if (length(ncol(PPS)) != 0 &&ncol(PPS) > 100){
        PPS <- PPS[,1:100]
      }#end if
      
      
      PPS_type <- determinePPSType(PPS)
      ###############print(paste0("The PPS Type is: ", PPS_type))
      
      if (PPS_type == -1){
        braiding_PPS_count <- braiding_PPS_count + 1
        braiding_df[braiding_PPS_count, 1] <- file_name
        braiding_df[braiding_PPS_count, 2] <- sheet_name
      } #end if
      if (PPS_type == 0){
        miscellaneous_PPS_count <- miscellaneous_PPS_count + 1
        miscellaneous_df[miscellaneous_PPS_count,1] <- file_name
        miscellaneous_df[miscellaneous_PPS_count,2] <- sheet_name
      } #end if

      if (PPS_type == 1){ # if extrusion
        
        extrusion_type <- getExtrusionType(PPS)
        
        if (extrusion_type == 'multi'){
          multiext_PPS_count <- multiext_PPS_count + 1
          multiext_df[multiext_PPS_count,1] <- file_name
          multiext_df[multiext_PPS_count,2] <- sheet_name
        }
        else if (extrusion_type == 'tapered'){
          taperedext_PPS_count <- taperedext_PPS_count + 1
          tapered_df[taperedext_PPS_count,1] <- file_name
          tapered_df[taperedext_PPS_count,2] <- sheet_name
          
          part_number <- parsePartNumber(sheet_name) #parses the excel tab for just the part number
          
          
          #missing_analysis <- checkedMissedPPS(tapered_PPS_data)
          
          #if(missing_analysis$missing == TRUE){
          #  missed_PPS_count <- missed_PPS_count + 1
          #  missed_df[missed_PPS_count,1] <- file_name
          #  missed_df[missed_PPS_count,2] <- sheet_name
          #  missed_df[missed_PPS_count,3] <- missing_analysis$reason
          #}
        
        } #end taper if
        else if (extrusion_type == 'single'){
          #cleans up the single extrusion
          
          single_PPS_count <- single_PPS_count + 1
          single_df[single_PPS_count,1] <- file_name
          single_df[single_PPS_count,2] <- sheet_name
          
          part_number <- parsePartNumber(sheet_names[sheet_count]) #parses the excel tab for just the
          #part number
          
          single_PPS_data <- assembleSingleData(part_number, PPS_number,
                                          PPS, single_output_names)
          
          missing_analysis <- checkedMissedPPS(single_PPS_data)
          
          if(missing_analysis$missing == TRUE){
            missed_PPS_count <- missed_PPS_count + 1
            missed_df[missed_PPS_count,1] <- file_name
            missed_df[missed_PPS_count,2] <- sheet_name
            missed_df[missed_PPS_count,3] <- missing_analysis$reason
          }
          else{
            single_PPS_dataframe <- as.data.frame(single_PPS_data, stringsAsFactors = FALSE,
                                                  col.names = single_output_names)
            database <- rbind(database, single_PPS_dataframe)
          }
        } #end single if
        
        
        ###########datalist[[directory_count]] <- single_PPS_data
        
        ###########print("Rbind was completed.")
      } #end if for single extrusion
      
      print(paste0("Completed the analysis of the sheet. Number: ", sheet_count, ", Sheet Name: ", sheet_name))
      
      sheet_count <- sheet_count + 1
    }#end while
    
    print(paste0("Completed the analysis of the file Number: ", directory_count))
    
    directory_count <- directory_count + 1 #update the count
    
  }# end while
  
  
  #### Printing and Assigning ####
  
  print(paste0("This is the number of single extrusion PPS Documents: ", single_PPS_count))
  print(paste0("This is the number of multi-extrusion PPS Documents: ", multiext_PPS_count))
  print(paste0("This is the number of tapered extrusion PPS Documents: ", taperedext_PPS_count))
  print(paste0("This is the number of braiding PPS Documents: ", braiding_PPS_count))
  print(paste0("This is the number of Miscellaneous PPS Documents: ", miscellaneous_PPS_count))
  print(paste0("This is the number of Missed PPS Documents: ", missed_PPS_count))
  
  print(paste0("The number of PPS Documents Analyzed in the Try Block is: ", try_count))
  print(paste0("The number of PPS Documents Analyzed in the Error Block is: ", error_count))
  print(paste0("The number of PPS Documents Analyzed in the Warning Block is: ", warning_count))
  
  braiding_unique <- braiding_df[!duplicated(braiding_df[,1]),]
  single_unique <- single_df[!duplicated(single_df[,1]),]
  multi_unique <- multiext_df[!duplicated(multiext_df[,1]),]
  tapered_unique <- tapered_df[!duplicated(tapered_df[,1]),]
  miscellaneous_unique <- miscellaneous_df[!duplicated((miscellaneous_df[,1])),]
  
  
  assign("Tapered Database", tapered_database, .GlobalEnv)
  
  assign("Braiding PPS Unique", braiding_unique, .GlobalEnv)
  assign("Braiding PPS", braiding_df, .GlobalEnv)
  assign("Single PPS Unique", single_unique, .GlobalEnv)
  assign("Multi PPS", multiext_df, .GlobalEnv)
  assign("Tapered PPS", tapered_df, .GlobalEnv)
  assign("Miscellaneous PPS Unique", miscellaneous_unique, .GlobalEnv)
  assign("Missed PPS", missed_df, .GlobalEnv)
  
  return (database)
  
}#end createDatabase

checkedMissedPPS <- function(PPS){
  #this code checks if any of the temperatues are missing or are NA. Ifthey are, it returns TRUE
  
  output <- list() #will store two values: missing and reason.
  output$missing <- FALSE
  
  if(length(PPS[["Feedthroat Temperature (F)"]]) == 0 &&
     length(PPS[["Barrel Zone 1 Temperature (F)"]]) == 0 &&
     length(PPS[["Clamp Temperature (F)"]]) == 0){
    output$missing <- TRUE
    output$reason <- "Lengths of Zero"
  }
  else if(is.na(PPS[["Feedthroat Temperature (F)"]]) &&
          is.na(PPS[["Barrel Zone 1 Temperature (F)"]]) &&
          is.na(PPS[["Clamp Temperature (F)"]])){
    output$missing <- TRUE
    output$reason <- "Temps are NA"
  }
  else if(PPS[["Feedthroat Temperature (F)"]] == "Temperature was not found" &&
          PPS[["Barrel Zone 1 Temperature (F)"]] == "Barrel temperature is NA or ''" &&
          PPS[["Clamp Temperature (F)"]] == "Temperature was not found"){
    output$missing <- TRUE
    output$reason <- "Temps not found"
  }

  
  return(output)
  
}#end checkedMissedPPS

importPPS <- function(file_name, sheet_name, try_count, error_count, warning_count){
  #this reads the excel file of the PPS. It tries read_excel and if an error is generated, it then
  #attemps read.xlsx.
  #the counts show which code anlyzed the PPS.
  
  output <- list()
  output$'PPS' <- NULL
  output$'Try' <- try_count
  output$'Error' <- error_count
  output$'Warning' <- warning_count
  
  PPS <- tryCatch({
    if (sheet_name == "90068180-01" ||
             sheet_name == "90978444-02" ||
             sheet_name == "90978444-03" ||
             sheet_name == "90807913-05" ||
             sheet_name == "91073346-06" ||
             sheet_name == "90975692-02"
    ){
      #this first sheet is part of file "90068481 AJ.xls" needs to start at row 2
      #the next two sheets are part of file "90991622 AG Final.xlsx"
      #the fourth sheet is part of file "90808666 Ver AF.xls"
      #the fifth sheet is part of file "91123505_Rev03.xlsx"
      #the sixth sheet is part of file "91123509_Rev03.xlsx"
      PPS <- read_excel(file_name, sheet = sheet_name, col_names = FALSE, skip = 1)
    }
    else if (sheet_name == "09674-001" ||
             sheet_name == "30373-04" ||
             sheet_name == "05446-001" ||
             sheet_name == "91073346-01 " ||
             sheet_name == "91073346-02" ||
             sheet_name == "91073346-03" ||
             sheet_name == "91073346-04" ||
             sheet_name == "91073346-05" ||
             sheet_name == "91079820-01" ||
             sheet_name == "91079820-02" ||
             sheet_name == "91079820-03" ||
             sheet_name == "90001840 ver BF.xlsx"
    ){
      #this first sheet is part of file "90942817.xlsx" needs to start at row 3
      #the second sheet is part of file "90484275.xls"
      #the third sheet is part of file "90942811.xlsx"
      #the fourth to eight sheets are part of file "91123505_Rev03.xlsx"
      #the ninth to eleventh sheets are part of file "91124188 Rev AC - PPS 91079820.xls"
      PPS <- read_excel(file_name, sheet = sheet_name, col_names = FALSE, skip = 2)
    }
    else {
      PPS <- read_excel(file_name, sheet = sheet_name, col_names = FALSE)
    }
    
    output$'PPS' <- PPS
    output$'Try' <- output$'Try' + 1
    
  }, #end try portion
  
  error = function(err){ #this is the error handling if col_names doesn't match the length
    #of column types

    if (sheet_name == "90068180-01" ||
             sheet_name == "90978444-02" ||
             sheet_name == "90978444-03" ||
             sheet_name == "90807913-05" ||
             sheet_name == "91073346-06" ||
             sheet_name == "90975692-02"
    ){
      #this first sheet is part of file "90068481 AJ.xls" needs to start at row 2
      #the next two sheets are part of file "90991622 AG Final.xlsx"
      #the fourth sheet is part of file "90808666 Ver AF.xls"
      #the fifth sheet is part of file "91123505_Rev03.xlsx"
      #the sixth sheet is part of file "91123509_Rev03.xlsx"
      PPS <- read.xlsx(file_name, sheetName = sheet_name, header = FALSE,
                       stringsAsFactors = FALSE, startRow = 2)
    }
    else if (sheet_name == "09674-001" ||
             sheet_name == "30373-04" ||
             sheet_name == "05446-001" ||
             sheet_name == "91073346-01 " ||
             sheet_name == "91073346-02" ||
             sheet_name == "91073346-03" ||
             sheet_name == "91073346-04" ||
             sheet_name == "91073346-05" ||
             sheet_name == "91079820-01" ||
             sheet_name == "91079820-02" ||
             sheet_name == "91079820-03" ||
             sheet_name == "90001840 ver BF.xlsx"
    ){
      #this first sheet is part of file "90942817.xlsx" needs to start at row 3
      #the second sheet is part of file "90484275.xls"
      #the third sheet is part of file "90942811.xlsx"
      #the fourth to eight sheets are part of file "91123505_Rev03.xlsx"
      #the ninth to eleventh sheets are part of file "91124188 Rev AC - PPS 91079820.xls"
      PPS <- read.xlsx(file_name, sheetName = sheet_name, header = FALSE,
                       stringsAsFactors = FALSE, startRow = 3)
    }
    else {
      PPS <- read.xlsx(file_name, sheetName = sheet_name, header = FALSE,
                       stringsAsFactors = FALSE)
    }
    
    output$'PPS' <- PPS
    output$'Error' <- output$'Error' + 1
    
  }, #end error portion
  
  warning = function(war){ #just in case
    
    if (sheet_name == "90068180-01" ||
        sheet_name == "90978444-02" ||
        sheet_name == "90978444-03" ||
        sheet_name == "90807913-05" ||
        sheet_name == "91073346-06" ||
        sheet_name == "90975692-02"
    ){
      #this first sheet is part of file "90068481 AJ.xls" needs to start at row 2
      #the next two sheets are part of file "90991622 AG Final.xlsx"
      #the fourth sheet is part of file "90808666 Ver AF.xls"
      #the fifth sheet is part of file "91123505_Rev03.xlsx"
      #the sixth sheet is part of file "91123509_Rev03.xlsx"
      PPS <- read.xlsx(file_name, sheetName = sheet_name, header = FALSE,
                       stringsAsFactors = FALSE, startRow = 2)
    }
    else if (sheet_name == "09674-001" ||
             sheet_name == "30373-04" ||
             sheet_name == "05446-001" ||
             sheet_name == "91073346-01 " ||
             sheet_name == "91073346-02" ||
             sheet_name == "91073346-03" ||
             sheet_name == "91073346-04" ||
             sheet_name == "91073346-05" ||
             sheet_name == "91079820-01" ||
             sheet_name == "91079820-02" ||
             sheet_name == "91079820-03" ||
             sheet_name == "90001840 ver BF.xlsx"
    ){
      #this first sheet is part of file "90942817.xlsx" needs to start at row 3
      #the second sheet is part of file "90484275.xls"
      #the third sheet is part of file "90942811.xlsx"
      #the fourth to eight sheets are part of file "91123505_Rev03.xlsx"
      #the ninth to eleventh sheets are part of file "91124188 Rev AC - PPS 91079820.xls"
      PPS <- read.xlsx(file_name, sheetName = sheet_name, header = FALSE,
                       stringsAsFactors = FALSE, startRow = 3)
    }
    else {
      PPS <- read.xlsx(file_name, sheetName = sheet_name, header = FALSE,
                       stringsAsFactors = FALSE)
    }
    
    output$'PPS' <- PPS
    output$'Warning' <- output$'Warning' + 1
    
  } #end warning portion
  
  ) #end tryCatch
  
  return(output)
}

determinePPSType <- function(data){
  #determines if the PPS being examined is a braiding PPS, single extrusion PPS, or co-extrusion PPS.
  #it will return, braid (-1), single (1), or coext (2), respectively. If it is neither, 
  #it returns miscellaneous (0).

  numberOfColumns <- ncol(data)
  
  #first determine if it is braiding
  if (numberOfColumns < 8){
    #if there are less than 8 columns, it must be a braiding PPS, so it returns 'braid'
    return (-1)
  }
  
  extruder_found <- c("Extruder")
  waterbath_found <- c("waterbath", "water bath")
  screwspeed_found <- c("screw speed", "screw speed")
  col_count <- 1
  while  (col_count < numberOfColumns + 1){
    #checks for matches in the columns of the above strings
    if (length(grep(extruder_found, data[,col_count], ignore.case = TRUE)) != 0){
      return(1)
    }# end if
    
    string_count <- 1
    while (string_count < 3){
      if (length(grep(waterbath_found[string_count], data[,col_count], ignore.case = TRUE)) != 0){
        return(1)
      }
      if (length(grep(screwspeed_found[string_count], data[,col_count], ignore.case = TRUE)) != 0){
        return(1)
      }
      string_count <- string_count + 1
    }# end while
    
    col_count <- col_count + 1
  }#end while
  
  
  #if neither type was found
  return (0)
  
}# end determinePPSType

assembleSingleData <- function(PN, PPS_number, PPS, output_names){
  #assembles all the information fed into a format to be placed into the database
  
  PPS_data <- singleExtrusionParameters(PPS, output_names[3:length(output_names)])
  vector_of_data <- matrix(c(PN,
                     PPS_number,
                     unlist(PPS_data)), nrow = 1, ncol = (2 + length(PPS_data)))
  names(vector_of_data) <- output_names
  return(vector_of_data)
  
} #end assembleSingleData

assembleTaperedData <- function(PN, PPS_number, PPS, output_names){
  #assembles all the information fed into a format to be placed into the database
  
  PPS_data <- taperedExtrusionParameters(PPS, output_names[3:length(output_names)])
  vector_of_data <- matrix(c(PN,
                             PPS_number,
                             unlist(PPS_data)), nrow = 1, ncol = 32)
  
  names(vector_of_data) <- output_names
  return(vector_of_data)
  
} #end assembleSingleData

parsePartNumber <- function(part_number){
  #parses the part number to remove an extraneous words from the excel tab name
  no_spaces <- strsplit(part_number, " ")
  no_blanks <- unlist(no_spaces)[!no_spaces[[1]] == ""] #creates a list of no blanks
  
  count <- 1
  while (count < length(no_blanks) + 1){
    #runs through the list of strings in no_blanks and checks for a part number
    if (grepl("^[[:digit:]]", no_blanks[count]) == TRUE){
      #checks if the string is just an integer
      return(no_blanks[count])
    } #end if
    count <- count + 1
  }#end while
  
  return(part_number)
  
}# end parsePartNumber

parsePPSNumber <- function(PPS_name){
  #this will parse the worksheet name to get the PPS number.
  
  remove_underscore <- gsub(pattern = "_", replacement = " ", PPS_name, ignore.case = TRUE)
  remove_PPS <- gsub(pattern = "PPS", replacement = "", remove_underscore, ignore.case = TRUE)
  no_spaces <- strsplit(remove_PPS, " ")
  no_blanks <- unlist(no_spaces)[!no_spaces[[1]] == ""]
  first_entry <- no_blanks[1]
  no_file_type <- strsplit(first_entry, "[.]")[[1]][1]
  no_rev <- strsplit(no_file_type, "rev")[[1]][1]
  
  return(no_rev)
  
} #end parsePPSNumber





##Silent read_excel
excel_sheets <- function(path) {
  quiet_excel_sheets <- purrr::quietly(readxl::excel_sheets)
  out <- quiet_excel_sheets(path)
  if(length(c(out[["warnings"]], out[["messages"]])) == 0)
    return(out[["result"]])
  else readxl::excel_sheets(path)
} #end excel_sheets

read_excel <-  function(...) {
  quiet_read <- purrr::quietly(readxl::read_excel)
  out <- quiet_read(...)
  if(length(c(out[["warnings"]], out[["messages"]])) == 0)
    return(out[["result"]])
  else readxl::read_excel(...)
} #end read_excel

