if(!require("tidyverse")) install.packages("tidyverse")
#if(!require("fs")) install.packages("fs")
if(!require("readxl")) install.packages("readxl")

compare_df <- function(df.x, df.y, output_dir, output_file_prefix="compare_file_") {
  
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file =  paste(output_file_prefix,ts,".xlsx", sep="")
  setwd(output_dir)
  wb <- createWorkbook(output_file)
  
  #handle duplication
  #step 1: record
  #step 2: remove duplicated record
  print("Step 1: checking duplicated records in the two sources...")
  duplicated_source1 <- sum(duplicated(df.x))
  duplicated_source2 <- sum(duplicated(df.y))
  duplicated_list <- c()
  duplicated_src <- c()
  if(duplicated_source1 > 0){
    print("Step 2: cleaning up duplicated records in the first data source...")
    duplicated_list <- append(duplicated_list, input_tab1[duplicated(df.x),1] )
    duplicated_src <- append(duplicated_src, rep("x",duplicated_source1))
    input_tab1 <-  input_tab1[!duplicated(df.x),]
  }
  if(duplicated_source2 > 0){
    print("Step 3: cleaning up duplicated records in the second data source...")
    duplicated_list <- append(duplicated_list, input_tab2[duplicated(df.y),1] )
    duplicated_src <- append(duplicated_src, rep("y",duplicated_source2))
    input_tab2 <-  input_tab2[!duplicated(df.y),]
  }
  agg.duplicated_list <- cbind(duplicated_list,duplicated_src)
  duplicated_key.df <- data.frame(agg.duplicated_list)
  colnames(duplicated_key.df ) <- c("duplicated key","source")
  
  # merge two files
  print("Step 4: merging two data sources, preparing for comparison...")
  merged_set <- merge(input_tab1, input_tab2, by=c("key"), all = TRUE)
  
  # processing df (1) - storing merged data
  df <- setNames(
    data.frame(matrix(ncol = ncol(input_tab1), nrow = nrow(merged_set))), 
    colnames(input_tab1)
  )
  # copy a list of key from the merged dataset
  df[1] <- merged_set[1]
  
  # processing df (2) - storing summary of the validation
  print("Step 5: comparing two data sources - missing and mismatch records...")
  stat.df <- setNames(
    data.frame(matrix(ncol = 7, nrow = 0)),
    colnames(c("field","field.x","field.y","mismatch","missing","match","total_count"))
  )
  colnames(stat.df) <- c("field","field.x","field.y","mismatch","missing","match","total_count")
  
  # after merging two inputs, compare field by field
  # key is in column 1. Thus, fields start from column 2.
  for(i in 2:ncol(input_tab1)){
    pos.y = i + ncol(input_tab1) - 1
    source1 <- merged_set[i]
    source2 <- merged_set[pos.y]
    
    #mismatch: return 1 is mismatch, 0 is matched
    df[i] <- ifelse(source1==source2,0,1) 
  }
  
  ## writing output from df(1)
  print("Step 6: writing merged dataset for referece...")
  addWorksheet(wb=wb, sheetName = "Sheet 1")
  writeData(wb, sheet = 1, merged_set)
  
  
  print("Step 7: beginning field by field comparison...")
  # add comparison tab (recursive)
  for(i in 2:ncol(input_tab1)){
    field_name = colnames(input_tab1)[i]
    print(paste("validating field #", i, " '",field_name,"'...", sep=""))
    df2_key <- df[df[i]==1,]
    df_colnames <- c("key","x","y")
    df2 <- setNames(
      #3 columns: key, source1, source2
      #Note: always 3 columns for field by field comparison
      data.frame(matrix(ncol = 3, nrow = nrow(df2_key))), 
      df_colnames
    )
    df2$key <-df2_key$key
    df2$x <- input_tab1[df[i]==1, i]
    df2$y <- input_tab2[df[i]==1, i]
    
    ## writing output from validation of df(1), merged dataset
    addWorksheet(wb=wb, sheetName = paste( "v", i, sep=""))
    writeData(wb, sheet = i, df2)  
    ###print(df2)
    
    field_list = c( colnames(input_tab1)[i], colnames(input_tab2)[i])
    source_list = c("x","y")
    df2.info <- data.frame(cbind(field_list, source_list))
    colnames( df2.info  ) <- c("field","source")
    
    ## writing output from validation of df(1), supplement
    writeData(wb, sheet = i, startCol = ncol(df2)+2 ,df2.info)  
    
    
    stat.mismatch = sum(df[i], na.rm = TRUE)
    stat.missing = sum(is.na(df[i]))
    stat.total = count(df[i])
    stat.match = stat.total -  stat.missing - stat.mismatch
    stat.new.raw<- data.frame(field = paste( "v", i, sep=""),
                              field.x = colnames(input_tab1[i]), 
                              field.y = colnames(input_tab2[i]),
                              mismatch = stat.mismatch, 
                              missing = stat.missing,
                              matched = stat.match,
                              total.count = stat.total)
    colnames(stat.new.raw) <- c("field","field.x","field.y","mismatch","missing","match","total_count")
    stat.df <- rbind(stat.df, stat.new.raw )
  }
  
  ## writing output from df(2)
  print("Step 8: writing validating summary...")
  addWorksheet(wb=wb, sheetName = "Validation Summary")
  writeData(wb, sheet = i+1, stat.df) 
  writeData(wb, sheet = i+1, startCol = ncol(stat.df)+2, duplicated_key.df ) 
  
  print(paste("Step 9: ","saving output as '" , output_file , "'...",sep=""))
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  print("*** validation completed. ***")
  return(1)
}