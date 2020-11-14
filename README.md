# ltdanalysis

Quality Check and LTD measurment for mGluR LTD experiments performed on WinLTD software for Osterweil lab.

## Setup

Package can be downloaded by 
```{r, echo = TRUE, eval = TRUE, collapse = TRUE}
install.packages("devtools")
devtools::install_github(“s1060046/LTD_analysis/ltdanalysis")
```

## Required files
A collection/single WinLTD ouptut: The slope and peak amplitude measurement of fEPSP is done in WinLTD during each experiment. This script assumes the measurement is correct
A metadata file: This excel spreadsheet must contain a 3 columns plus all the relavent information of the experiment
1. data_name: File name of the WinLTD result (without .XLS extention)
2. Stim_sweep: sweep number at which DHPG was applied.
3. rig: rig number (Due to changes in the set up, rig 2 is now called 2_new 14/11/20)

## Example of batch analysis
load package and set working directory contains all the files and the metadata file

```{r, echo = TRUE, eval = TRUE, collapse = TRUE}
require(ltdanalysis)
setwd("~/Desktop/My projects/LTD/202010QC/")
```
Load in metadata file and blind the file names
```
require(readxl)
meta <- read_excel(path = "202010_Metadata.xlsx")
set.seed(1)
meta$blind <- sample(1:dim(meta)[1], dim(meta)[1], replace = FALSE)
```
use QC function in loop to perfom QC
```
QC(file_path = file_path, stim_sweep = stim_sweep , rig = rig, res_file = res_file)
```
file_path is the path to the WinLTP output file
stim_sweep is the sweep at whcih DHPG was applied
rig is the rig number where the recodring was performed
res_file is the file name of the plots to be generated

first10vssecond10, lm_no, stability_test can also be specified.
these are thresholds for each tests and will generate a pass or fail column.
currently the default is set to first10vssecond10 = 10, lm_no = 7, stability_test = 7

example code for looping through metadata is shown below
```
require(dplyr)
qcreports <- data.frame()
for(trace in meta$data_name){
  message(paste("QCing trace ", trace , sep = "")) #generates output when looping
  dat <- subset(meta, meta$data_name == trace) #subset metadata for the trace
  file_path = paste(dat$data_name, ".XLS", sep = "") #just adds .XLS at the end for file path
  res_file = paste(dat$blind, ".jpg", sep = "") #will generate QC plots with this name <blind id>.jpg
  stim_sweep = dat$Stim_sweep
  rig = dat$rig
  qcreport <- QC(file_path = file_path, stim_sweep = stim_sweep , rig = rig, res_file = res_file) #QC plot and report
  qcreports <- rbind(qcreports, qcreport) #append PC report to qcreports dataframe
}

get_name <- function(x){
  x <- x[1]
  f_name <- strsplit(as.character(x), split = "[/]")[[1]][3]
  return(strsplit(f_name, split = "[.]")[[1]][1])
}

qcreports$Key <- apply(qcreports,1,get_name)
meta$blind <- as.character(meta$blind)
meta <- left_join(meta, qcreports, by = c("blind" = "Key")) #merge metadata and QC reports now meta will contain all the measurements, QC test results and metadata
```
