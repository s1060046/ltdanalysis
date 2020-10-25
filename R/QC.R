#' QC function
#'
#' plot
#' @param file_path, file path of XLS file
#' @param rig, Rig number
#' @param stim_sweep, sweep number at DHPG sti
#' @param res_file, output file name
#' @return plot
#' @export
#'


QC <- function(file_path, rig, stim_sweep, res_file){
  if (!dir.exists("./QCdata")){
    dir.create(path = "./QCdata")
  }
  dat <- read_data(file_path = file_path, rig =rig)
  dat_c <- clean_data(recording = dat, stim_sweep = stim_sweep)
  res_file = paste("./QCdata/", res_file, sep = "")
  QCplot(dat_c, res_file)
}
