#' read_data function
#'
#' This reads the XLS file generated from WinLTD software
#' @param file_path, file path of XLS file
#' @param rig, Rig number
#' @return data object or list of objects
#' @import readxl
#' @export


read_data <- function(file_path, rig){
  if(!(rig %in% c(1,2,3,4,"2_new"))){
    stop("input rig number")
  }
  if(file.exists(file_path) == FALSE){
    stop("File does not exist")
  }
  data <- readxl::read_excel(file_path)
  if(rig %in% c(3,4,"2_new")){
    data <- subset(data, data$Sx == 0)
    data$rig = rig
    return(data)
  }
  if(rig %in% c(1,2)){
    if(rig==1){
      data_0 <- subset(data, data$AD == 0)
      data_0 <- subset(data_0, data_0$Sx == 0)
      data_0$rig = rig
      return(data_0)
    } else{
      data_1 <- subset(data, data$AD == 1)
      data_1 <- subset(data_1, data_1$Sx == 0)
      data_1$rig = rig
      return(data_1)
    }
  }
}
