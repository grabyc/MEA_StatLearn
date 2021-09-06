ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

##packages <- c("MASS", "caret", "effects", "e1071", "rpart", "Epi")
packages <- c("tidyverse", "knitr", "hrbrthemes", "magrittr")
ipak(packages)

## ej 3.1

# carga de dataset vegetacion 
sitios <- read_csv2("data/VEGETACION.csv") 

sitios_3_1 <- sitios %>%
  rowwise(SITIO) %>%
  summarise(cant_especies=sum(c_across(E1:E84)))

## ej 3.2

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
labels <- sitios_3_1
labels$id <- strtoi(sub('.', '', sitios_3_1$SITIO))

# calculate the ANGLE of the labels
num_bars <- nrow(labels)
angle <-  90 - 360 * (labels$id-0.5) /num_bars     # substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If on the left part of the plot, labels have currently an angle < -90
labels$horiz_just<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
labels$bar_angle<-ifelse(angle < -90, angle+180, angle)


ggplot(sitios_3_1, aes(x=factor(SITIO, levels = SITIO), y=cant_especies)) +       
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-15,45) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) + 
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=labels, aes(x=id, y=cant_especies+5, label=paste(SITIO, " (", cant_especies, ")"), hjust=horiz_just), 
            color="black", fontface="bold",alpha=0.6, size=2.5, angle=labels$bar_angle, inherit.aes = FALSE ) 

