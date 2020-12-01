#' QC function
#'
#' plot
#' @param file_path, file path of XLS file
#' @param rig, Rig number
#' @param stim_sweep, sweep number at DHPG stim
#' @param res_file, output file name
#' @param first10vssecond10 first 10 mins - second 10 mins default = 10
#' @param lm_no deviation calculated from lm baseline default = 5
#' @param stability_test test for stability, if error is above it will fail default = 7
#' @return plot
#' @export
#'


QC <- function(file_path, rig, stim_sweep, res_file, first10vssecond10 = 10, lm_no = 7, stability_test = 7){
  if (!dir.exists("./QCdata")){
    dir.create(path = "./QCdata")
  }
  dat <- read_data(file_path = file_path, rig =rig)
  drug_start = dat[1,]$TimeOfDay
  dat_c <- clean_data(recording = dat, stim_sweep = stim_sweep)
  res_file = paste("./QCdata/", res_file, sep = "")
  report <- QCplot(dat_c, res_file, first10vssecond10 = first10vssecond10, lm_no = lm_no, stability_test = stability_test)
  return(cbind(report, drug_start))
}
