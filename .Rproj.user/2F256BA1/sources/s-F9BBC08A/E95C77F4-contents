#' question2:calculation of mean,median and standard deviation
#' @param df The data we want to input, here we use the DRG data
#' @param x  The condition argument, we input "Mean","Standard deviation","Median" to get back corresponding value.
#'
#' @return a dataframe with the corresponding data we choose by x
#' @export
#'
#' @examples
#' lab2_2(df,x='Mean')
stat_q2<-function(df,x) {
  if(x=='Mean') { #we use if and if else to decide what output will be used
    df %>%
      group_by(`DRG Definition`) %>% #group by DRG code
      summarize(mean=mean(`Average Medicare Payments`)) #summarize the mean by every DRG code
  }
  else if(x=='Standard deviation') {
    df %>%
      group_by(`DRG Definition`) %>% #group by DRG code
      summarize(standard.deviation=sd(`Average Medicare Payments`)) #summarize the standard deviation by every DRG code
  }
  else if(x=='Median') {
    df %>%
      group_by(`DRG Definition`) %>% #group by DRG code
      summarize(median=median(`Average Medicare Payments`)) #summarize the median n by every DRG code
  }
}
