#' QCplot function
#'
#' plot
#' @param plt_data, data to plot
#' @param res_file, output file name
#' @param stability stability threshold default = 15%
#' @param first10vssecond10 first 10 mins - second 10 mins default = 10
#' @param lm_no deviation calculated from lm baseline default = 5
#' @param stability_test test for stability, if unstable after this minute test will fail default = 30
#' @return generates a plot
#' @import ggplot2
#' @import ggpubr
#' @export
#

QCplot <- function(plt_data, res_file, stability = 15, first10vssecond10 = 10, lm_no = 5, stability_test = 30){
  plt_data <- subset(plt_data, plt_data$time < 70)

  first10 <- subset(plt_data, plt_data$time > -20 & plt_data$time < -10)
  second10 <- subset(plt_data, plt_data$time > -10 & plt_data$time < 0)

  baseline_dat <- subset(plt_data, plt_data$time < 0)

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

  qcdata <- subset(plt_data, plt_data$time > 0)
  qc_res <- data.frame()
  for(i in 1:dim(qcdata)[1]){
    if(i < (dim(qcdata)[1]-3)){
      qcdat <- qcdata[i:(i+2),]
      diff <- qcdat$slope[1] - qcdat$slope[3]
      exp_data <- data.frame(start = qcdat$time[1],
                             end = qcdat$time[3],
                             diff = diff)
      qc_res <- rbind(qc_res,exp_data)
    }
  }

  #optimise this
  qc_res$test <- (qc_res$diff < -stability | qc_res$diff > stability)

  mark <- subset(qc_res, qc_res$test == TRUE)
  mark$group <- seq_along(mark$start)
  
  first10vssecond10_val = mean(first10$slope) - mean(second10$slope)
  
  LTD = subset(plt_data, plt_data$time >55 & plt_data$time<65)
  LTD = 100 - mean(LTD$slope)
  
  qcrport <- data.frame(QCID = res_file,
                        first10vssecond10_val = as.numeric(first10vssecond10_val),
                        first10vssecond10_res = if(first10vssecond10_val > first10vssecond10 | 
                                                   first10vssecond10_val < -first10vssecond10){"Fail"}else{"Pass"},
                        lm_baseline_val = as.numeric(diff_lm),
                        lm_baseline_res = if(diff_lm > lm_no |
                                             diff_lm < -lm_no){"Fail"}else{"Pass"},
                        stability_res = if(sum(apply(mark, 1, function(x){x[1] > stability_test})) > 0){"Fail"}else{"Pass"},
                        LTD = LTD)
  
  
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
    coord_cartesian(ylim = c(0,125)) + geom_segment(aes(x = 55, y = 0, xend = 65, yend = 0)) +
    geom_rect(data=mark, inherit.aes=FALSE,
              aes(xmin=start, xmax=end, ymin=0
                  ,ymax=130, group=group), color="transparent", fill="orange", alpha=0.3) +
    ggtitle("LTD Stability") +
    xlab("Time (0 = DHPG stim)") + ylab("Slope % baseline")


  output <- ggarrange(firstvssecond, baseline, QC,
           ncol = 3)
  ggsave(filename = res_file, plot = output, width = 13.541666667, height = 3.6458333333, units = "in")
  return(qcrport)
}
