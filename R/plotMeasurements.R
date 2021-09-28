# Script to plot recorded measurements +++++++++++++++++++++++++++++++++++++
# Author: Kai Budde
# Created: 2021/05/28
# Last changed: 2021/07/02
# Version: 0.1.1

rm(list = ls())
graphics.off()

require(yaml)
require(ggplot2)
source("R/convertYAMLtoDataframe.R")

# Load packages and define parameter values ################################

# Either read zip file or directory with unzipped files
#directory_with_zip_files <- "data/"
#zip_file_with_data <- "Francia_24h_Reihen_Stimulation.zip"

directory_with_files <- "data/04_coldMedium_coldElectrodes/"

#plot_title <- "30', 60', 120' AC stimulation, 130 Hz, 60\u03BCs, current-controlled, 4mL medium, 3 wells"
#plot_title <- "12h AC stimulation, 130 Hz, 60\u03BCs, current-controlled, 4mL medium, 6 wells"
#plot_title <- "24h AC stimulation, 130 Hz, 60\u03BCs, voltage-controlled, 3.5mL medium, 6 wells"
plot_title <- "Exp4 (medium cold, electrodes cold)"

vavg_min <- -1
vavg_max <- 1
vpp_min <- 0
vpp_max <- 10
timedate_breaks = "1 min" #"1 hours"
dir_out <- file.path(getwd(), "data", "output", "measurements")

# Create a directory to which the images will be written ###################
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# Import data from YAML file ###############################################
#yaml_files <- list.files(path = directory_with_yaml_files, pattern = "measurement")
if(exists("zip_file_with_data") || exists("directory_with_zip_file")){
  zipped <- TRUE
  yaml_files <- unzip(zipfile = file.path(directory_with_zip_files, zip_file_with_data), list = TRUE)
  yaml_files <- yaml_files$Name
  yaml_files <- yaml_files[grepl(pattern = "measurement", x = yaml_files, ignore.case = TRUE)]
}else{
  zipped <- FALSE
  yaml_files <- list.files(path = directory_with_files, pattern = "measurement")
}


for(i in 1:length(yaml_files)){
  if(i == 1){
    
    if(zipped){
      df_data_list <- yaml::read_yaml(file = unz(
        description = file.path(directory_with_zip_files, zip_file_with_data),
        filename = yaml_files[i] ))
    }else{
      df_data_list <- yaml::read_yaml(file = file.path(directory_with_files, yaml_files[i]))
    }
    
    df_data <- convertYAMLtoDataframe(df_data_list)

    # Get date and time of measurement
    df_data$date_time <- gsub(pattern = "-measurement.yml", replacement = "", x = yaml_files[i])
    
  }else{
    
    if(zipped){
      df_data_dummy_list <- yaml::read_yaml(file = unz(
        description = file.path(directory_with_zip_files, zip_file_with_data),
        filename = yaml_files[i] ))
    }else{
      df_data_dummy_list <- yaml::read_yaml(file = file.path(directory_with_files, yaml_files[i]))
    }
    
    df_data_dummy <- convertYAMLtoDataframe(df_data_dummy_list)
    
    # Get date and time of measurement
    df_data_dummy$date_time <- gsub(pattern = "-measurement.yml", replacement = "", x = yaml_files[i])
    
    df_data <- rbind(df_data, df_data_dummy)
  }
}

rm(list = c("df_data_dummy", "df_data_list", "df_data_dummy_list", "i"))

df_data$date_time <- as.POSIXct(strptime(df_data$date_time, "%Y%m%d-%H%M%S"))

# Plot VAVG ################################################################
plot_vavg <- ggplot2::ggplot(data = df_data,
                             aes(x = date_time, y = VAVG, group=Channel, color=Channel)) +
  geom_point() +
  coord_cartesian(ylim = c(vavg_min, vavg_max)) +
  scale_x_datetime(date_breaks = timedate_breaks) +
  scale_y_continuous(breaks = seq(vavg_min, vavg_max, by = 0.2),
                     minor_breaks = seq(vavg_min, vavg_max, by = 0.1)) +
  labs(title=plot_title, x = "Date", y = "U_avg/V") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))

# Save files
file_path <- file.path(dir_out, "vavg_measurement.png")
ggsave(plot = plot_vavg, filename = file_path, device = "png", width = 19.2, height = 10.8,  units = "cm")

# Find VPP #################################################################

plot_vpp <- ggplot2::ggplot(data = df_data, aes(x = date_time, y = VPP, group=Channel, color=Channel)) +
  geom_point() +
  coord_cartesian(ylim = c(vpp_min, vpp_max)) +
  scale_x_datetime(date_breaks = timedate_breaks) +
  scale_y_continuous(breaks = seq(vpp_min, vpp_max, by = 2),
                     minor_breaks = seq(vpp_min, vpp_max, by = 1)) +
  labs(title=plot_title, x = "Date", y = "U_p2p/V") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))
        
file_path <- file.path(dir_out, "vp2p_measurement.png")
ggsave(plot = plot_vpp, filename = file_path, device = "png", width = 19.2, height = 10.8,  units = "cm")

