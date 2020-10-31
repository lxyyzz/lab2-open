#'question1: boxplot
#' @param df The data we want to input, here we use the DRG data
#' @param ydata Here we choose from " Average Covered Charges "," Average Total Payments","Average Medicare Payments" as the data of y axis
#'
#' @return  a box plot with x axis as the DRG code(only keep the number part) and y axis as the ydata selected from three choices
#' @export
#'
#' @examples
#' lab2(df,ydata='Average Covered Charges')
boxplot_q1<-function(df,ydata) {
  df_1<- substring(df$`DRG Definition`,1,3)#we use sub string to get just the number instead of the whole name of the DRG code which helps us make the plot more clear
  ydata1<-str_to_lower(ydata) #we set the ydata1 to be lower case so that we can use it into the title and yaxis
  ggplot(df,
         aes(x=df_1,
             y=get(ydata)))+ #input the data and define x and y axis
    geom_boxplot(position='dodge')+ #set the plot to be box plot
    xlab('DRG code(only keep the number)')+ #x-label
    ylab(paste0('Distribution of ', ydata1))+ #y-label
    ggtitle(paste0('Boxplot of ' ,ydata1,' by DRG code'))+ #title of the plot
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #we transfer 90 degree to the text of the x-label so that we can see more clearly to the label
}

