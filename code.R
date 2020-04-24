library(tidyverse)
library(ggplot2)
library(data.table)
h <- head
g <- glimpse  
s <- summary  
t <- tail

# pn: a master data frame
pn <- fread("NCHSData13.csv")

# data cleaning        
pn1 <- pn %>% 
        filter(Week %in% 1:12) %>% 
        mutate(Year = as.character(Year))
new_names <- c("Year", 
               "Week", 
               "Percent_of_Deaths_Due_to_Pneumonia_and_Influenza",
               "Expected", 
               "Threshold",
               "All_Deaths", 
               "Pneumonia_Deaths",
               "Influenza_Deaths")
names(pn1) <- new_names
pn2 <- pn1 %>%
        mutate(Pneumonia_Deaths_Percent = round(Pneumonia_Deaths / All_Deaths * 100, digits = 2),
               Influenza_Deaths_Percent = round(Influenza_Deaths / All_Deaths * 100, digits = 2))

# plotting
plotting <- function(df, yy, title, ylabel) {
        ggplot(df, aes(x = Week, 
                       y = yy, 
                       col = Year)) + 
                geom_line(size = 1.5) + 
                ggtitle(title) + 
                ylab(ylabel)
}

plot1 <- plotting(pn2, 
         pn2$Percent_of_Deaths_Due_to_Pneumonia_and_Influenza,
         "Deaths from Peumonia & Influenza",
         "% of All Deaths")
plot2 <- plotting(pn2, 
                  pn2$Pneumonia_Deaths_Percent,
                  "Deaths from Peumonia",
                  "% of All Deaths")
plot3 <- plotting(pn2, 
                  pn2$Influenza_Deaths,
                  "Deaths from Influenza",
                  "% of All Deaths")
