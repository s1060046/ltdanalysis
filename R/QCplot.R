#' QCplot function
#'
#' plot
#' @param plt_data, data to plot
#' @param res_file, output file name
#' @return generates a plot
#' @import ggplot2
#' @import ggpubr
#' @export
#

QCplot <- function(plt_data, res_file){
  plt_data <- subset(plt_data, plt_data$time < 70)

  first5 <- subset(plt_data, plt_data$time > -20 & plt_data$time < -10)
  second5 <- subset(plt_data, plt_data$time > -10 & plt_data$time < 0)

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
  qc_res$test <- (qc_res$diff < -10 | qc_res$diff > 10)

  mark <- subset(qc_res, qc_res$test == TRUE)
  mark$group <- seq_along(mark$start)

  firstvssecond <- ggplot(plt_data, aes(time, slope))+
    geom_point() + geom_vline(xintercept = 0, lty = 2) +
    coord_cartesian(ylim = c(0,125)) + geom_segment(aes(x = 55, y = 0, xend = 65, yend = 0)) +
    geom_segment(aes(x = -20, y = mean(first5$slope), xend = -10, yend = mean(first5$slope), colour = "red", linetype = "dashed")) +
    geom_segment(aes(x = -10, y = mean(second5$slope), xend = -0, yend = mean(second5$slope), colour = "red", linetype = "dashed")) +
    geom_text(x = 65, y = 120, label = paste("Diff = ", mean(first5$slope) - mean(second5$slope) ,sep=""), hjust = 1, vjust = 0)+ ggtitle("basline slop")+
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
}
