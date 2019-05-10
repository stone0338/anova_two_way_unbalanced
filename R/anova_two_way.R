
anova_two_way <- function(factor1, factor2, value) {
  # library(jsonlite)
  # data_json = '{"factor1": ["M","M","M","M","M","M","M","M","M","F","F","F","F","F","F","F","F","F"],
  #               "factor2": ["l","l","l","m","m","m","h","h","h","l","l","l","m","m","m","h","h","h"],
  #               "value": [null,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]}'
  # data_list = fromJSON(data_json)
  # data_anova = data.frame(data_list[[1]],data_list[[2]],data_list[[3]])
  # colnames(data_anova) = c("factor1", "factor2", "value")
  # 
  options(contrasts = c("contr.sum", "contr.poly"))
  stats_model_1 <- lm(as.numeric(value) ~ factor1 * factor2)
  anova_1 <- drop1(stats_model_1, .~., test="F")
  # output <- paste0(
  #   "{",
  #     "'F':[",
  #       anova_1[2,5], ",", anova_1[3,5], ",", anova_1[4,5],
  #     "],",
  #     "'P':[",
  #       anova_1[2,6], ",", anova_1[3,6], ",", anova_1[4,6],
  #     "]",
  #   "}"
  # )
  output <- paste0(anova_1[2,5], "&", anova_1[3,5], "&", anova_1[4,5], "&", anova_1[2,6], "&", anova_1[3,6], "&", anova_1[4,6])
  output
  # print(c(anova_1[2,1], anova_1[3,1], anova_1[4,1], anova_1[1,1]))
  # print(c(anova_1[2,5], anova_1[3,5], anova_1[4,5], anova_1[1,5]))
  # print(c(anova_1[2,6], anova_1[3,6], anova_1[4,6], anova_1[1,6]))
}
