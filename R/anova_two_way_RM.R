
anova_two_way_rm <- function(file_input,
                             sheet,
                             outlier_rm) {
  library(lme4)
  library(emmeans)
  data_anova <- data_processed(file_input = file_input,
                               sheet = sheet,
                               outlier_rm = outlier_rm)[["data_stats"]]
  
  subject <- data_anova[,1]
  factor1 <- data_anova[,2]
  factor2 <- data_anova[,3]
  value <- data_anova[,4]
  subject_name <- colnames(data_anova)[1]
  factor1_name <- colnames(data_anova)[2]
  factor2_name <- colnames(data_anova)[3]
  value_name <- colnames(data_anova)[4]
  n_subject <- length(unique(data_anova[,1]))
  n_factor1 <- length(unique(data_anova[,2]))
  n_factor2 <- length(unique(data_anova[,3]))
  
  aov <- aov(value~factor1*factor2+Error(subject/factor1))
  aov1 <- summary(aov)[[1]][[1]]
  aov2 <- summary(aov)[[2]][[1]]
  aov_final <- rbind(aov2[1,], aov1[1,], aov2[2,])
  row.names(aov_final) <- c(factor1_name, factor2_name, paste0(factor1_name, " X ", factor2_name))
  aov_final <- cbind(row.names(aov_final), aov_final)
  colnames(aov_final)[1] <- " "
  colnames(aov_final)[6] <- "P Value"
  rownames(aov_final) <- NULL
  
  
  lme <- lmer(value~factor1*factor2+(1|subject), REML = TRUE)
  lsmean_1 <- lsmeans(lme, pairwise ~ factor2|factor1)
  lsmean_1_1 <- lsmean_1$contrasts
  lsmean_1_1_1 <- summary(lsmean_1_1)
  lsmean_1_1_1_1 <- lsmean_1_1_1[,c("factor1", "contrast","p.value")]
  colnames(lsmean_1_1_1_1)[3] <- "p"
  
  lsmean_2 <- lsmeans(lme, pairwise ~ factor1|factor2)
  
  stats_output <- lsmean_1_1_1_1
  stats_output2 <- lsmean_2$contrasts
  
  p <- lsmean_1_1_1_1
  p$contrast <- factor(p$contrast, levels = unique(p$contrast))
  p$p_symbol <- ""
  p$p_label <- ""
  p$"checkbox_id" <- ""
  p$sig_index <- ""
  rows_sig <- which(p$'p' < 0.05)
  if(length(rows_sig) > 0) {
    sig_symbol <- c("*", "$", "#", "!", "@", "%", "&", "~")
    sig_index <- 1
    for (i in 1:length(unique(p$contrast))) {
      p_i <- which(p$contrast == unique(p$contrast)[i])
      p_sig_i <- intersect(p_i,rows_sig)
      p$sig_index[p_sig_i] <- sig_index
      p$p_symbol[p_sig_i] <- paste00(rep(sig_symbol[sig_index], 1))
      p$p_symbol[p_sig_i][which(p$p[p_sig_i] < 0.01)] <- paste00(rep(sig_symbol[sig_index], 2))
      p$p_symbol[p_sig_i][which(p$p[p_sig_i] < 0.001)] <- paste00(rep(sig_symbol[sig_index], 3))
      p$p_symbol[p_sig_i][which(p$p[p_sig_i] < 0.0001)] <- paste00(rep(sig_symbol[sig_index], 4))
      if (length(p_sig_i) != 0) {
        sig_index = sig_index + 1
      }
    }
    p$sig_index <- as.numeric(p$sig_index)
    for (i in rows_sig) {
      p$p_label[i] <-
        paste0("<input type='checkbox' id='p_label_", i, "' class='p_label' checked>Label<br>")
      p$"checkbox_id"[i] <- paste0("p_label_", i)
    }
  }
  
  p_output <- p[,c(1:5)]
  colnames(p_output)[1] <- factor1_name
  colnames(p_output)[2] <- paste0(factor2_name, " - Comparisons")
  colnames(p_output)[3] <- "P Values"
  colnames(p_output)[4] <- "Symbol"
  colnames(p_output)[5] <- "Label or Not"
    
    
  
  # output
  list(
    p = p,
    n = 3,
    n_factor1 = n_factor1,
    n_factor2 = n_factor2,
    output_table_0 = aov_final,
    output_table = p_output,
    stats_type = "anova_two_way_rm"
  )
}
  