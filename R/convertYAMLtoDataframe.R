# Helpers function: Convert YAML list into dataframe +++++++++++++++++++++++
# Author: Kai Budde
# Created: 2021/07/02
# Last changed: 2021/07/02
# Version: 0.1.0

# Convert YAML list into dataframe
convertYAMLtoDataframe <- function(df_data_list, variabel_name){
  for(j in 1:length(df_data_list)){
    if(j == 1){
      df_data <- as.data.frame(df_data_list[[j]])
      df_data$Channel <- gsub(pattern = "CHAN", replacement = "", x = names(df_data_list)[j])
    }else{
      df_data_dummy <- as.data.frame(df_data_list[[j]])
      df_data_dummy$Channel <- gsub(pattern = "CHAN", replacement = "", x = names(df_data_list)[j])
      
      df_data <- rbind(df_data, df_data_dummy)
    }
  }
  return(df_data)
}