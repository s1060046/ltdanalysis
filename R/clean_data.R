#' clean_data function
#'
#' cleans data, takes two points and makes min average, subset data from 20 minutes before the stim as baseline,  calculates slope as % baseline returns clean object
#' @param recording, object generated from read_data
#' @param stim_sweep, sweep number at DHPG stim
#' @return data object or list of objects
#' @import dplyr
#' @export


clean_data <- function(recording, stim_sweep){

  if(inherits(recording, "data.frame")){
    stim_time <- suppressWarnings(recording[grep(pattern = stim_sweep, recording$Filename)[1],]$Time_min)
    
    plot_data <- subset(recording, recording$Time_min > stim_time-20)
    start_time <- subset(recording, recording$Time_min == stim_time)
    start_time <- start_time$TimeOfDay
    
    df_summary <- plot_data %>% as_tibble(rownames = "names") %>%
      group_by(ceiling(1:n() / 2)) %>%
      summarise(time = mean(Time_min),
                slope = -mean(`Slope_unit/ms`),
                amp = -mean(PkAmp)) %>%
      select(-1)

    baseline <- mean(subset(df_summary, df_summary$time  < stim_time)$slope)

    df_summary$slope <- df_summary$slope / baseline * 100
    df_summary$time = df_summary$time - stim_time
    df_summary$rig = unique(recording$rig)
    df_summary$stim_time = start_time
    return(df_summary)
  } else{
    stop("input structure wrong check the input data - needs to be an output of read_data function")
  }
}
