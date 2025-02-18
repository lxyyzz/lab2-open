#'question1: boxplot
#' @param data_1 The data we want to input, here we use the DRG data
#' @param ydata Here we choose from " Average Covered Charges "," Average Total Payments","Average Medicare Payments" as the data of y axis
#'
#' @return  a box plot with x axis as the DRG code(only keep the number part) and y axis as the ydata selected from three choices
#' @export
#'
#' @examples
#' boxplot_q1(df,ydata = 'Average Covered Charges')
boxplot_q1<-function(data_1,ydata) {
  df_1<-substring(data_1$'DRG Definition',1,3)#we use sub string to get just the number instead of the whole name of the DRG code which helps us make the plot more clear
  ydata1<-stringr::str_to_lower(ydata) #we set the ydata1 to be lower case so that we can use it into the title and yaxis
  ggplot2::ggplot(data_1,
         ggplot2::aes(x = df_1,
                      y = get(ydata)))+ #input the data and define x and y axis
    ggplot2::geom_boxplot(position='dodge')+ #set the plot to be box plot
    ggplot2::xlab('DRG code(only keep the number)')+ #x-label
    ggplot2::ylab(paste0('Distribution of ', ydata1))+ #y-label
    ggplot2::ggtitle(paste0('Boxplot of ' ,ydata1,' by DRG code'))+ #title of the plot
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) #we transfer 90 degree to the text of the x-label so that we can see more clearly to the label
}

