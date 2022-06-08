#pull in Gurobi logfile and generate optimality gap curve

library(tidyverse)
library(magrittr)

rm(list=ls())

#import file
gurobi_logfile <- "my_gurobi_logfile.log" #full path to the logfile you want to read in and visualize


x <- readLines(con= gurobi_logfile, warn = FALSE)
table_starts_at <- grep("Unexpl", x)
table_ends_at <- grep("Cutting",x) - 2 #last line is 2 rows before the Cutting Plane summary
x <- x[table_starts_at:table_ends_at]

#identify H (new incumbent) solutions
h_rows <- grep("H",x)
x <- x[h_rows]

#for each row, extract gap % and duration
gap_pct <- numeric()
duration_s <- numeric()

for(i in 1:length(x)){

#split by spaces
k <- stringr::str_split(x[i],pattern=" ")[[1]]
#which component of the string has %?
m <- which( stringr::str_detect( k, "%") ==TRUE)
gap_pct[i] <- k[m] %>% stringr::str_replace(pattern="%",replacement="") %>% as.numeric() * 0.01

#which component of the storng has "s", indicating seconds since solve start?
m <- which( stringr::str_detect( k,"s") ==TRUE)
duration_s[i] <- k[m] %>% stringr::str_replace(pattern="s",replacement="") %>% as.numeric()

}

mipgap_df <- cbind.data.frame(duration_s , gap_pct,stringsAsFactors=FALSE) 
mipgap_df_100pct <- mipgap_df %>% dplyr::filter(gap_pct <= 1)

mipgap_ggplot <- ggplot2::ggplot(data=mipgap_df_100pct ,aes(x=duration_s/3600,y=gap_pct))+geom_point()+geom_line() + 
  scale_y_log10(labels=scales::percent,breaks= c(seq(0.01,0.2,0.01),seq(0.25,0.75,0.05),seq(0.8,5,0.25)))+
  scale_x_continuous(breaks=seq(0,10000,1))+
  xlab("Solve Time (Hours)")+ylab("% Optimality Gap (Log Scale)")  +
  ggtitle("Solve Time In Hours vs. MIPGap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))