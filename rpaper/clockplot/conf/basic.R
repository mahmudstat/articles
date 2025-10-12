basic_clock <- function(){
  k <- 24 # Hours
  subk <- 24*4
  times <- exp(1i * 2 * pi * (k:1) / k)
  subtimes <- data.frame(SubT = exp(1i * 2 * pi * (subk:1) / subk)) 
  ampm = c(rep(" AM",6), rep(" PM",12), rep(" AM",6))
  dfclock <- tibble(time = times,
                    hour = c(6:12, 1:12, 1:5), # May not be needed
                    label = paste0(c(6:12, 1:5), ampm))
  clock_skeleton <-  dfclock %>% ggplot()+
    geom_path(data = subtimes, aes(Re(SubT), Im(SubT)))+
    # Connect Last two missing points
    geom_line(data = subtimes %>% dplyr::slice(-c(2:95)), aes(Re(SubT), Im(SubT)))+
    theme(aspect.ratio = 1)+
    geom_text(data = dfclock, aes(Re(time)*1.1, Im(time)*1.1, label = label))+
    geom_point(data = subtimes, 
               aes(Re(SubT), Im(SubT)), shape = 19, color = "black", size = 0.6)+
    geom_point(aes(Re(time), Im(time)), color = "black", size = 1.8)+
    labs(x = "", y = "")+
    theme(axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(), 
          axis.text.y=element_blank(),  
          axis.ticks.y=element_blank(),
          legend.position = "none",
          aspect.ratio = 1)#+
  #geom_point(aes(0,0), color = "black", size = 2)
  return(clock_skeleton)
}

clock <- basic_clock() # Working

clock
