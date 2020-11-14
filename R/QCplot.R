#' QCplot function
#'
#' plot
#' @param plt_data, data to plot
#' @param res_file, output file name
#' @param first10vssecond10 first 10 mins - second 10 mins default = 10
#' @param lm_no deviation calculated from lm baseline default = 5
#' @param stability_test test for stability, if error is above it will fail default = 7
#' @return generates a plot
#' @import ggplot2
#' @import ggpubr
#' @export
#

QCplot <- function(plt_data, res_file, first10vssecond10 = 10, lm_no = 7, stability_test = 7){
  plt_data <- subset(plt_data, plt_data$time < 70)

  first10 <- subset(plt_data, plt_data$time > -20 & plt_data$time < -10)
  second10 <- subset(plt_data, plt_data$time > -10 & plt_data$time < 0)

  baseline_dat <- subset(plt_data, plt_data$time < 0)
  stim_time = baseline_dat$stim_time[1]
  # SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA
  lm_eqn <- function(df){
    m <- lm(slope ~ time, df);
    eq <- substitute(italic(y) == b %.% italic(x) + a,
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }

  m <- lm(slope ~ time, baseline_dat)
  first = -20*coef(m)[2] + coef(m)[1]
  second = coef(m)[1]
  diff_lm <- second - first
  
  #exponential decay fit
  time_cut <- subset(plt_data, plt_data$time < 15)
  time_cut <- time_cut[time_cut$slope == min(time_cut$slope),]$time
  expo_fit <- subset(plt_data, plt_data$time > time_cut)
  expo_fit$slope <-120 - expo_fit$slope
  c.0 <- min(expo_fit$slope) * 0.5
  model.0 <- lm(log(slope - c.0) ~ time, data=expo_fit)
  start <- list(a=exp(coef(model.0)[1]), b=-coef(model.0)[2], c=c.0)
  nlc <- nls.control(maxiter = 1000)
  
  model <- try(nls(slope ~ a * exp(-b * time) + c, data = expo_fit, start = start, control = nlc), silent = TRUE)
  if(inherits(model, "try-error")){
    expo_plot <- data.frame(time = expo_fit$time,
                            slope = -10)
    error = NA
  }else{
    expo_plot <- data.frame(time = expo_fit$time,
                            slope = 120-predict(model))
    error = expo_fit$slope - predict(model)
    error = sqrt(mean(error^2))
  }
  
  
  
  first10vssecond10_val = mean(first10$slope) - mean(second10$slope)
  
  LTD = subset(plt_data, plt_data$time >55 & plt_data$time<65)
  LTD = 100 - mean(LTD$slope)
  LTD_55_60 = subset(plt_data, plt_data$time >55 & plt_data$time<60)
  LTD_55_60 = 100 - mean(LTD_55_60$slope)
  LTD_50_60 = subset(plt_data, plt_data$time >50 & plt_data$time<65)
  LTD_50_60 = 100 - mean(LTD_50_60$slope)
  
  qcrport <- data.frame(QCID = res_file,
                        first10vssecond10_val = as.numeric(first10vssecond10_val),
                        first10vssecond10_res = if(first10vssecond10_val > first10vssecond10 | 
                                                   first10vssecond10_val < -first10vssecond10){"Fail"}else{"Pass"},
                        lm_baseline_val = as.numeric(diff_lm),
                        lm_baseline_res = if(diff_lm > lm_no |
                                             diff_lm < -lm_no){"Fail"}else{"Pass"},
                        error = error,
                        error_res = if(error > stability_test | is.na(error)){"Fail"}else{"Pass"},
                        LTD = LTD, LTD_55_60 = LTD_55_60, LTD_50_60 = LTD_50_60, mean_amp_baseline = mean(baseline_dat$amp),
                        stim_time = stim_time)
  
  
  firstvssecond <- ggplot(plt_data, aes(time, slope))+
    geom_point() + geom_vline(xintercept = 0, lty = 2) +
    coord_cartesian(ylim = c(0,125)) + geom_segment(aes(x = 55, y = 0, xend = 65, yend = 0)) +
    geom_segment(aes(x = -20, y = mean(first10$slope), xend = -10, yend = mean(first10$slope), colour = "red", linetype = "dashed")) +
    geom_segment(aes(x = -10, y = mean(second10$slope), xend = -0, yend = mean(second10$slope), colour = "red", linetype = "dashed")) +
    geom_text(x = 65, y = 120, label = paste("Diff = ", mean(first10$slope) - mean(second10$slope) ,sep=""), hjust = 1, vjust = 0)+ ggtitle("basline slop")+
    theme(legend.position = "none")+
    ggtitle("First 10 mins vs second 10 mins") +
    xlab("Time (0 = DHPG stim)") + ylab("Slope % baseline")



  baseline <- ggplot(plt_data, aes(time, slope))+
    geom_point() + geom_vline(xintercept = 0, lty = 2) +
    coord_cartesian(ylim = c(0,125)) + geom_segment(aes(x = 55, y = 0, xend = 65, yend = 0)) +
    geom_smooth(data = baseline_dat, method = "lm", formula = "y~x")+
    geom_text(x = 65, y = 120, label = lm_eqn(baseline_dat), parse = TRUE, hjust = 1, vjust = 0)+
    geom_text(x = 65, y = 110, label = paste("diff from lm = ", diff_lm, sep =""), hjust = 1, vjust = 0)+
    ggtitle("basline slop") +
    xlab("Time (0 = DHPG stim)") + ylab("Slope % baseline")

  QC <- ggplot(plt_data, aes(time, slope))+
    geom_point() + geom_vline(xintercept = 0, lty = 2) +
    coord_cartesian(ylim = c(0,125)) + geom_line(data = expo_plot, color = "Red", lty = 2) +
    geom_text(x = 65, y = 120, label = paste("Error = ", error, sep = ""), hjust = 1, vjust = 0)+
    ggtitle("LTD Stability") +
    xlab("Time (0 = DHPG stim)") + ylab("Slope % baseline")


  output <- ggarrange(firstvssecond, baseline, QC,
           ncol = 3)
  ggsave(filename = res_file, plot = output, width = 13.541666667, height = 3.6458333333, units = "in")
  return(qcrport)
}
