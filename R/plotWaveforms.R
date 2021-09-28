# Script to plot recorded waveforms (imported from yaml files) +++++++++++++
# Author: Kai Budde
# Created: 2021/05/28
# Last changed: 2021/07/02
# Version: 0.1.1

rm(list = ls())
graphics.off()

require(yaml)
require(ggplot2)
require(magick)
require(tidyverse)

# Load packages and define parameter values ################################
directory_with_yaml_files <- "data/04_coldMedium_coldElectrodes/"
time_in_us <- TRUE
pulse_width <- 60 #\mu s
Umax <- 15  #V
plot_title <- "Exp4 (medium cold, electrodes cold)"
channel_pulse <- "CHAN1"
dir_out <- file.path(getwd(), "data", "output", "waveforms")

# Create a directory to which the images will be written ###################
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# Find all waveforms and plot the data #####################################

wave_files <- list.files(path = directory_with_yaml_files, pattern = "wave")
wave_files <- wave_files[grepl(pattern = "yml$", x = wave_files, ignore.case = TRUE)]

for(i in 1:length(wave_files)){
  
  # Import data from YAML file #############################################
  df_data <- yaml::read_yaml(file = file.path(directory_with_yaml_files, wave_files[i]))
  df_data <- as.data.frame(df_data)
  df_data <- tidyr::gather(data = df_data, key = "Channel", value = "U",  -time)
  
  # Convert time to \mu s ##################################################
  if(time_in_us){
    df_data$time <- df_data$time*1e6
  }
  
  # Get time information ###################################################
  date_time_measurement <- gsub(pattern = "-wave.yml", replacement = "", x = wave_files[i])
  
  # Get max and min (smoothed) of pulse ("Chan1")
  lower_bound <- (max(df_data$U[df_data$Channel == channel_pulse]) - mean(df_data$U[df_data$Channel == channel_pulse]))/2
  upper_bound <- (min(df_data$U[df_data$Channel == channel_pulse]) - mean(df_data$U[df_data$Channel == channel_pulse]))/2
  
  max_value <- as.numeric(quantile(df_data$U[df_data$Channel == channel_pulse & df_data$U > lower_bound], 0.95))
  min_value <- as.numeric(quantile(df_data$U[df_data$Channel == channel_pulse & df_data$U < upper_bound], 0.05))
  
  p2p_value <- abs(max_value)+abs(min_value)
  if(is.na(p2p_value)){
    p2p_value <- 0
  }
  p2p_value <- format(round(p2p_value, digits = 2), nsmall = 2)
  p2p_value <- paste("V_p2p=", p2p_value, "V", sep="")

  # Plot complete data from YAML file ######################################
  plot_waveform <- ggplot2::ggplot(data = df_data,
                                   aes(x = time, y = U, color=Channel)) +
    geom_path() +
    geom_hline(yintercept=max_value, linetype="dashed", color = "gray") +
    geom_hline(yintercept=min_value, linetype="dashed", color = "gray") +
    annotate("text", x=20, y=(Umax-2), label=p2p_value) + 
    coord_cartesian(ylim = c(-Umax, Umax)) +
    labs(title=paste(plot_title, "at", date_time_measurement, sep=" "),
         x = "time/\U00B5s", y = "U/V") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust = 0.5),
          plot.title = element_text(hjust = 0.5))

  # Save files
  file_path <- file.path(dir_out, paste0(date_time_measurement, ".png"))
  ggsave(plot = plot_waveform, filename = file_path, device = "png", width = 19.2, height = 10.8,  units = "cm")
}
rm(i)

# Create gif ###############################################################

# (Adapted from http://www.nagraj.net/notes/gifs-in-r/.)

# List file names and read in
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, magick::image_read)

# Join the images together
img_joined <- magick::image_join(img_list)

# Animate at 2 frames per second
img_animated <- magick::image_animate(img_joined, fps = 2)

# save to disk
magick::image_write_gif(image = img_joined, path =  file.path(dir_out, "waveforms.gif"), delay = 0.5)
