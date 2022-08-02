library("ggplot2")

ggplot_multiple_ts = function(data,
                              key_name,
                              start_date=NULL){
  if (is.null(start_date) == T){
    data_long = melt(data = data, id.vars = "date")
  }else{
    data_long = melt(data = dataÍ, id.vars = "date") %>% filter(date >= start_date)
  }
  ggplot(data_long,
         aes(date, value, color=variable)) + geom_line()
  
}

ggplot_line_dot <- function(data, line_plot, dot_plot) {
  # assumes that the first column is date
  
  date <- data[, 1]
  
  line_plot_column <- data[, line_plot]
  
  dot_plot_column <- data[, dot_plot]
  
  coeff <- 
    mean(dot_plot_column, na.rm = TRUE) / mean(line_plot_column, na.rm = TRUE)
  
  (graph <- ggplot(data) + 
      geom_line(aes(x = date, y = line_plot_column), color = "#191970") +
      geom_point(aes(x = date, y = dot_plot_column / coeff), color = "red4") + 
      scale_y_continuous(
        # Features of the first axis
        name = line_plot,
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name = dot_plot)) +
      xlab("Data") +
      theme_classic())
}


ggplot_line_dummy <- function(data, 
                            target_name,
                            dummy_name) {
  # assumes that the first column of the dataframe is date
  
  date <- data[, 1]
  
  graph <- ggplot() + 
    geom_line(aes(x = date, y = data[, target_name]), 
              color = "#191970") + 
    xlab("Date") + 
    ylab(target_name) + 
    theme_classic()
  
  only_positive_dummy <- data %>% 
    filter(eval(parse(text=dummy_name)) == 1)
  
  only_negative_dummy <- data %>% 
    filter(eval(parse(text=dummy_name)) == -1)
  
  (graph + 
      geom_point(data = only_positive_dummy,
                 aes(x = only_positive_dummy[, 1], 
                     y = only_positive_dummy[, target_name]),
                 color = "green", shape = 19, size = 2) + 
      geom_point(data = only_negative_dummy,
                 aes(x = only_negative_dummy[, 1], 
                     y = only_negative_dummy[, target_name]),
                 color = "red", shape = 19, size = 2))
}
