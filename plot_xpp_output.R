#Goal of this script: time series visualization of xppaut data (orbit diagrams)
#Packages needed
library(dplyr)
library(ggplot2)

#Load the datasets
df1 <- read.table("tau2=10_firsthalf.dat")
df2 <- read.table("tau2=10_secondhalf.dat") %>% distinct(V6, .keep_all = TRUE) #It is the case that the "second half" of the dataset, which is obtained without the Poincaré section, 
#has a lot of useless data in it, so it's the files with "second half" in the name that ought to be cleaned up

#Function to perform data cleaning
clean_xpp_data <- function(df1, df2) {
  #Bind both datasets and remove useless columns
  df <- bind_rows(df1, df2) %>% select(-c(1, 4, 5))
  
  #Rename columns
  df <- df %>%
    rename(
      prey = V2,
      predator = V3,
      tau1 = V6
    )
  
  #Save cleaned dataset
  write.table(df, file = "testingdata.dat", append = FALSE, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = " ")
  
  return(df)
}

#Execute clean_xpp_data
df_clean <- clean_xpp_data(df1, df2)

#Data visualization
myplot <- ggplot(df, aes(x = tau1, y = predator)) +
  geom_point(color = "black", alpha = 1, size = 1.5) +
  labs(
    x = expression(tau[1]),
    y = expression(italic(y)(italic(t)))
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 16),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    axis.text = element_text(size = 14, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)   # top, right, bottom, left
  )

#Save plot to output directory
ggsave(
  filename = "orbit_diagram_tau2_10.png",
  plot = myplot,
  width = 7,
  height = 5,
  dpi = 300
)
