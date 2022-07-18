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