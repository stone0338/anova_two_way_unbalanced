dunnett_test <- function(values, groups) {
  library(reshape2)
  library(multcomp)
  long <- data.frame(groups, values)
  fit <- aov(values ~ groups, long)
  Dunnet <- glht(fit, linfct=mcp(groups="Dunnett"))
  pvalues = summary(Dunnet)$test$pvalues
  num_comparisons = length(pvalues)
  output = pvalues[1]
  for (p_value in pvalues[2:num_comparisons]) {
    output = paste0(output, "&" , p_value)
  }
  output
}
