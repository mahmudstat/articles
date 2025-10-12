library(tidyverse)
library(clockplot)

clock_chart_qlt(brintcity, time = Arrival, crit = Zone)+
  ggplot2::labs(
    color = "Zone",
    title = "Times of Arrival of Trains in Dhaka Station"
  )

clock_chart_qlt(brintcity, time = Departure, crit = Zone)+
  ggplot2::labs(
    color = "Zone",
    title = "Times of Departure of Trains from Dhaka Station"
  )

# Merge Arrival and Departure 
brintcity %>% pivot_longer(cols = c("Arrival", "Departure"),
                           names_to = "TYPE", 
                           values_to = "Time") %>% 
  clock_chart_qlt(time = Time, crit = TYPE)+
  labs(title = "Departure & Arrival Times of Trains from Dhaka Station")


bdquake %>% ggplot(aes(hms, mag))+
  geom_bar(stat = "identity")+
  labs(x = "Time", 
       y = "Magnitude",
       title = "Recent Earthquakes in Bngladesh")

clock_chart_qnt(
  data = bdquake, time = hms, len = depth,
  Col = mag, high = "red", low = "blue"
) + ggplot2::labs(
  color = "Depth", size = "Magnitude",
  title = "Earthquakes in Bangladesh since 2023"
)+
  theme(legend.position = "right")

# Bar Chart #### 

acdt <- read.csv("https://raw.githubusercontent.com/mahmudstat/open-analysis/main/data/usacc.csv")

acdt %>% ggplot(aes(Time, Humidity...))+
  geom_bar(stat = "identity")+
  labs(x = "Time",
       y = "Humidity",
       title = "USA Accidents Time and Corresponding Humidity")

acdt <- read.csv("https://raw.githubusercontent.com/mahmudstat/open-analysis/main/data/usacc.csv")
clock_chart_qnt(acdt, 
                time = Time, 
                len = Humidity..., 
                Col = Temperature.F.) +
  ggplot2::labs(size = "Humidity", 
                color = "Temperature",
                title = "Times of Accidents in USA")+
  theme(legend.position = "right")



# Methods ####

k <- 24
hour <- exp(1i * 2 * pi * (k:1) / k)
plot(hour, pch = 19, asp = 1, xaxt='n', yaxt='n', xlab = "", ylab = "")

# Without box

k <- 24
hour <- exp(1i * 2 * pi * (k:1) / k)
plot(hour, 
     pch = 19,        # Solid points
     asp = 1,         # Perfect circle aspect ratio
     xaxt = 'n',      # Remove x-axis
     yaxt = 'n',      # Remove y-axis
     xlab = "",       # Remove x-label
     ylab = "",       # Remove y-label
     bty = "n",       # Remove box around plot
     col = "black",   # Point color
     cex = 1.5)       # Point size

# Create a perfect circle
theta <- seq(0, 2*pi, length.out = 100)  # 100 points from 0 to 2π
circle <- exp(1i * theta)

# Rose curve: r = cos(k * theta)
t <- seq(0, 2*pi, length.out = 500)
k <- 5  # Number of petals

z <- cos(k * t) * exp(1i * t)  # Rose curve formula

plot(Re(z), Im(z), type = "l", asp = 1, col = "purple", lwd = 2,
     main = paste("Rose Curve with", k, "petals"),
     xlab = "Real", ylab = "Imaginary")

plot(Re(circle), Im(circle), type = "l", asp = 1, col = "blue", lwd = 2,
     xlab = "Real axis", ylab = "Imaginary axis", main = "Perfect Circle")
grid()

# Archimedean spiral
t <- seq(0, 8*pi, length.out = 500)  # Multiple rotations
z <- (t/5) * exp(1i * t)  # Radius increases with angle

plot(Re(z), Im(z), type = "l", asp = 1, col = "darkorange", lwd = 2,
     main = "Archimedean Spiral", xlab = "Real", ylab = "Imaginary")

k <- 24
times <- exp(1i * 2 * pi * (k:1) / k)
plot(times, xaxt='n', yaxt='n', type = "n", asp = 1)
# ampm = c(rep(" AM",6), rep(" PM",12), rep(" AM",6))
# text(times, labels = paste0(c(6:23, 0:5), ampm))
text(times, labels = c(6:23, 0:5))
# One liner Circle ####

plot(exp(1i * 2 * pi * (24:1) / k), 
     pch = 19, asp = 1, xaxt='n', yaxt = 'n',
     xlab = "", ylab = "")

plot(c(-1, 1), c(-1, 1), type = "n")

# prepare "circle data"
radius = 1
center_x = 0
center_y = 0
theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle

# draw the circle
lines(x = radius * cos(theta) + center_x, y = radius * sin(theta) + center_y)
