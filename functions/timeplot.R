library(tidyverse)
library(rlang)


# default ggplot theme
theme_set(theme_classic())


time_plot <- function(df, time, proxy){
  ggplot(df , aes(x ={{time}}, y = {{proxy}}, color = record, group = scenario)) +
    geom_point() +
    geom_line()+
    scale_x_continuous(trans = "reverse", expand = c(0, 0)) +
    scale_color_discrete("") +
    ylab("Temperature (°C)")
}
