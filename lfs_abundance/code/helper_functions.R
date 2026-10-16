#Design-based function that takes a data frame with explicit zeros for all species
Design_Based <- function(Data_In = NA,
                         ID_Column = "SampleID",
                         Studies=c("SLS"),
                         Study_Column = "Source",
                         Date_Column = "Date",
                         Catch_Column = "Catch",
                         Volume_Column = "Tow_volume",
                         Grouping_Columns = c("Source","SubRegion"),#SubRegion needs to be here
                         Time_Columns = c("WY","Survey","survDate"),
                         Final_Grouping = c("survDate","WY","Source","Survey","SubRegion","Region"), #This can be at coarser spatial scale than grouping columns, defaults to Grouping_Columns if NA
                         Volume_Object = habVols,
                         AugSepAdj = F,
                         Data_Only = F)#This is mostly for troubleshooting, 
#Default is recommneded. 
{
  
  #Make starting grouping final grouping if NA
  if(any(is.na(Final_Grouping))==T){
    Final_Grouping <- paste(c(Grouping_Columns,Time_Columns))}
  
  #Select only the columns needed for DB calculations and grouping
  #The use of 'unique' ensures that if Study is a grouping factor that
  #it is only attempted to be selected once
  
  if(AugSepAdj==T){
    data1 <- Data_In%>%select_at(unique(c(ID_Column,Study_Column,Date_Column,
                                          Catch_Column,Volume_Column,
                                          Grouping_Columns,Time_Columns)))
    
    names(data1) <- unique(c("tow_id","Source","Date",
                             "Catch","Volume",paste(Grouping_Columns),paste(Time_Columns)))%>%
      mutate(WY = if_else(Month%in%c("Aug","Sep",WY+1,WY)))
    
  }else{
    data1 <- Data_In%>%select_at(unique(c(ID_Column,Study_Column,Date_Column,
                                          Catch_Column,Volume_Column,
                                          Grouping_Columns,Time_Columns)))
    
    names(data1) <- unique(c("tow_id","Source","Date",
                             "Catch","Volume",paste(Grouping_Columns),paste(Time_Columns)))
    
  }
  
  #print(names(data1))
  #This section calculates the monthly average CPUV and variance at the 
  #initial grouping level of aggregation
  #using simplified versions of the functions in Polansky et al. 2019  
  
  data2 <- data1%>%
    filter(Source%in%Studies)%>%
    #add in subregion volumes
    left_join(Volume_Object,by="SubRegion")%>%
    #calculate grouping level values
    group_by_at(c(paste(Grouping_Columns),paste(Time_Columns)))%>%
    #Calculate stratum effort and total catch
    mutate(N_Tows = n(),
           Total_Catch = sum(Catch,na.rm=T))%>%
    ungroup()%>%arrange(Source,tow_id)%>%
    select(-Total_Catch)
  
  #print(names(data2))
  if(Data_Only==T){return(data2)}else{
    
    output1 <- data2 %>% 
      group_by_at(c(paste(Grouping_Columns),paste(Time_Columns)))%>%
      mutate(HabVols=sum(unique(volume_0to15)),#Aggregate habitat volumes to appropriate level
             StrataC = sum(Catch), #Calculate total stratum catch
             StrataV = sum(Volume),#Calculate total stratum sampled volume
             CPUV = StrataC/StrataV,#Calculate stratum CPUV
             devSquared = (Catch-CPUV*Volume)^2,#Calculate individual tow deviances
             #from stratum CPUV
             s2 = sum(devSquared)/(n()-1),
             Var = ((1/mean(Volume)^2) * (s2/n()))*volume_0to15^2#Calculate and expand
             #Stratum variance
      )%>%
      ungroup()%>%
      #Replace infinite variance with NA
      mutate(Var = na_if(Var,Inf))%>%
      #Summarize to one row per month/year/Subregion
      distinct(across(c(paste(Grouping_Columns),paste(Time_Columns))),.keep_all=T)
    
    output_final <- 
      output1%>%
      #Expand CPUV by habitat volume, calculate total variance and SE
      group_by_at(Final_Grouping)%>%
      mutate(Abundance = sum(CPUV*volume_0to15,na.rm=T),#Final abundance estimate
             TotalVar = sum(Var,na.rm=T),#Final variance estimate
             SE = sqrt(TotalVar),
             N_Tows=sum(N_Tows))%>%
      ungroup()%>%
      
      #Summarize to final grouping level
      distinct(across(c(all_of(Final_Grouping))),.keep_all=T)%>%
      
      ##Calculate log-normal CI as in Polansky et al 2019
      ## Assuming the variable is log-normal (and not using a bias correction).
      mutate(cv = SE/Abundance,
             cv = if_else(is.nan(cv),0,cv),
             mu.hat = log(Abundance/sqrt(1+cv^2)),
             sigma.est = sqrt(log(1 + cv^2)),
             lower = exp(mu.hat+qnorm(.05/2)*sigma.est),#Lower 95% CI bound
             upper = exp(mu.hat+qnorm(1-.05/2)*sigma.est)#Upper 95% CI bound
      )%>%
      select(all_of(Final_Grouping),"Abundance","cv","lower","upper","N_Tows")
    
  }
  return(output_final)
}


#===============================================================================
#WIDE FUNCTION
#=============================================================================
#This function makes wide-format longfin data with the option to specify length/age cuts
#Generate a data frame of Bay Study age-at-length classifications


Age_Cuts_Simple <- data.frame(Month = 1:12,
                              Age0Cut = 75,
                              Age1Cut = 100)%>%
  mutate(Month=as.factor(month.abb[Month]))


Make_Wide <- function(Data,
                      length.df = Age_Cuts_Simple,
                      groupVars = c("SampleID","AgeByLength")
){
  
  Data%>%
    #Assign ages based on Bay Study age-at-length designations
    left_join(length.df,by=c("Month"))%>%
    #Make an Age-by-length variable by comparing lengths to the cutoff values
    mutate(AgeByLength = if_else(Length<=Age0Cut,"Age-0","Age-1"),
           AgeByLength = if_else(Length>Age1Cut,"Age-2+",AgeByLength),
           WY = get_waterYear(Date))%>%
    group_by_at(paste(groupVars))%>%
    summarise(Catch = round(sum(Count)))%>%
    ungroup()%>%
    #Pivot catch wider by age
    pivot_wider(names_from=AgeByLength,
                values_from = Catch)%>% 
    replace(is.na(.), 0)%>%
    return()
}


#A function that identifies the most recent date-suffixed excel file in the 
#working directory and loads it

read_latest_suffixed_xlsx <- function(dir = "./data",
                                      date_regex = "(\\d{1,2}[._-]\\d{1,2}[._-]\\d{2,4})",
                                      verbose = TRUE) {
  # 1) List Excel files (exclude Excel lock files like ~$filename.xlsx)
  files <- list.files(dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
  files <- files[!grepl("^~\\$", basename(files))]
  
  if (length(files) == 0) stop("No .xlsx files found in: ", normalizePath(dir))
  
  # 2) Extract date-like substrings from filenames (e.g., 1.13.26)
  # If multiple date substrings exist in one name, use the last occurrence.
  date_subs_all <- str_extract_all(basename(files), date_regex)
  date_subs <- vapply(date_subs_all,
                      function(x) if (length(x)) tail(x, 1) else NA_character_,
                      FUN.VALUE = character(1))
  
  # 3) Normalize separators and parse the date (try mdy first, then dmy)
  date_clean <- str_replace_all(date_subs, "[._-]", "/")
  
  parsed_mdy <- suppressWarnings(mdy(date_clean, quiet = TRUE))
  parsed_dmy <- suppressWarnings(dmy(date_clean, quiet = TRUE))
  parsed_dates <- coalesce(parsed_mdy, parsed_dmy)  # choose whichever parsed successfully
  
  # 4) Decide the "latest" file
  if (all(is.na(parsed_dates))) {
    # Fallback: use most recent file modification time
    info <- file.info(files)
    chosen_idx <- which.max(info$mtime)
    chosen_file <- files[chosen_idx]
    if (verbose) message("No date suffix found; using most recently modified file:\n  ", chosen_file)
  } else {
    chosen_idx <- which.max(parsed_dates)
    chosen_file <- files[chosen_idx]
    if (verbose) {
      message("Selected by date in filename: ", basename(chosen_file),
              "  (parsed date = ", as.character(parsed_dates[chosen_idx]), ")")
    }
  }
  
  # 5) Read all sheets into a named list
  sheets <- excel_sheets(chosen_file)
  if (length(sheets) == 0) stop("No sheets found in workbook: ", chosen_file)
  
  sheet_list <- setNames(
    lapply(sheets, function(s) read_xlsx(chosen_file, sheet = s)),
    sheets
  )
  
  if (verbose) {
    message("Loaded ", length(sheet_list), " sheet(s) from: ", basename(chosen_file))
    message("Sheets: ", paste(sheets, collapse = ", "))
  }
  
  return(sheet_list)
}

#----------Functions required for updating data from CDFW FTP------------------
# Function to connect from R to Access via the ODBC drivers ---------------

architectureCheck <- function(officeBit = NULL) {
  
  # What architecture of R are you on?
  rBit <- ifelse((.Machine$sizeof.pointer == 4), "x32", "x64")
  
  # Do you have 32 bit or 64 bit office installed
  # Can attempt to read this from the registry itself;
  # if unsuccessful, the user must specify
  if (is.null(officeBit)) {
    if (rBit == "x64") {
      fp <- file.path("SOFTWARE", "Microsoft", "Office",
                      "ClickToRun", "Configuration",
                      fsep = "\\")
      subkey <- "Platform"
    } else {
      fp <- file.path("SOFTWARE", "Microsoft", "Office", "16.0", "Outlook",
                      fsep = "\\")
      subkey <- "Bitness"
    }
    
    officeBit <- tryCatch(readRegistry(fp)[[subkey]],
                          error = function(cond) {
                            ifelse(grepl("not found", cond$message),
                                   stop("Cannot automatically detect the architecture of your Microsoft Office. Please fill in `x32` or `x64` manually in the `officeBit` argument.", call. = F),
                                   stop(cond))
                          })
    officeBit <- ifelse((officeBit != "x64"), "x32", "x64")
  }
  
  # Are they the same?
  if (officeBit != rBit) {
    # First case = in 64bit R but have only 32bit office. Here, will have to use the terminal
    if (rBit == "x64" & officeBit == "x32") {
      # Check to see if a 32 bit R is installed
      if (!file.exists(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe"))) {
        stop("A 32-bit R could not be found on this machine and must be installed.", call. = F)
      }
    }
  }
  
  check <- ifelse(rBit == officeBit, T, F)
  
  list(check = check,
       rBit = rBit,
       officeBit = officeBit)
}

bridgeAccess <- function(file, tables = "check",
                         script = "connectAccess.R",
                         out = tempdir()) {
  
  # Does the file exist? Do you need to be on your company network?
  if (!file.exists(file)) stop("Database file path not found. Did you specify right? Are you on VPN?", call. = F)
  
  # Do you need to specify where the connection script is?
  if (!file.exists(script)) stop("The `connectAccess.R` script cannot be found. Specify full path to the script.", call. = F)
  
  # First, check architecture. If ok then just source the script; if not then invoke system2
  bitCheck <- architectureCheck()
  
  if (isTRUE(bitCheck$check)) {
    Args <<- c(file, out, tables)
    
    source(file = script)
    
    con <- connectAccess(file)
    
    extractTables(con = con,
                  tables = tables,
                  out = out,
                  rBit = bitCheck$rBit,
                  officeBit = bitCheck$officeBit)
  } else {
    file <- shQuote(normalizePath(file, winslash = "\\"))
    script <- shQuote(normalizePath(script, winslash = "\\"))
    tables <- shQuote(tables)
    
    terminalOutput <- system2(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe"),
                              args = c(script,
                                       file, out,
                                       bitCheck, tables))
    
    readRDS(file.path(out, "savedAccessTables.rds"))
  }
}

connectAccess <- function(file,
                          driver = "Microsoft Access Driver (*.mdb, *.accdb)", uid = "", pwd = "", ...) {
  
  file <- normalizePath(file, winslash = "\\")
  
  # Driver and path required to connect from RStudio to Access
  dbString <- paste0("Driver={", driver,
                     "};Dbq=", file,
                     ";Uid=", uid,
                     ";Pwd=", pwd,
                     ";")
  
  tryCatch(DBI::dbConnect(drv = odbc::odbc(), .connection_string = dbString),
           error = function(cond) {
             if (all(stringr::str_detect(cond$message, c("IM002", "ODBC Driver Manager")))) {
               message(cond, "\n")
               message("IM002 and ODBC Driver Manager error generally means a 32-bit R needs to be installed or used.")
             } else {
               message(cond)
             }
           })
  # RODBC::odbcDriverConnect(con, ...)
}

extractTables <- function(con, tables, out, rBit, officeBit) {
  
  # Pulling just the table names
  # tableNames <- RODBC::sqlTables(con, tableType = c("TABLE", "VIEW"))["TABLE_NAME"]
  tableNames <- odbc::dbListTables(conn = con)
  
  # Includes system tables which cannot be read, excluding them below with negate
  # tableNames <- stringr::str_subset(tableNames, "MSys", negate = T)
  if (length(tables) == 1 & all(tables %in% "check")) {
    # If no table names are specified, then simply return the names of the possible databases for the user to pic
    
    # RODBC::odbcClose(con)
    DBI::dbDisconnect(con)
    
    cat("Specify at least one table to pull from: \n")
    
    return(print(tableNames))
  }
  
  # Apply the dbReadTable to each readable table in db
  # returnedTables <- mapply(RODBC::sqlQuery,
  #                          query = paste("SELECT * FROM", tables),
  #                          MoreArgs = list(channel = con),
  #                          SIMPLIFY = F)
  returnedTables <- mapply(DBI::dbReadTable,
                           name = tables,
                           MoreArgs = list(conn = con),
                           SIMPLIFY = F)
  
  # names(returnedTables) <- tables
  
  DBI::dbDisconnect(con)
  # RODBC::odbcClose(con)
  
  if (rBit == "x64" & officeBit == "x32") {
    # Save the table to be read back into R
    saveRDS(returnedTables, file = file.path(out, "savedAccessTables.rds"))
  } else {
    returnedTables
  }
}


