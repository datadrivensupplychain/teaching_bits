#shinydashboard app for teaching statistics concepts

library(tidyverse)
library(magrittr)
library(DT)
library(shinydashboard)
library(rhandsontable)
require(shinybusy)



#clear workspace and run Garbage Collector
rm(list=ls())
gc(T,T,T)
#no scientific notation
options(scipen=999)


			  
#User interface

ui <- dashboardPage(
  
  
  dashboardHeader(title = "Statistics Helpers"),
  dashboardSidebar(
      sidebarMenu(
	  menuItem("Critical Value Calculator", tabName="critical_values_tab"),
	  menuItem("Normal Distribution Calculator", tabName = "standard_normal_tab"),
	  menuItem("T-Distribution Calculator", tabName = "tdist_tab") ,
	  menuItem("Coin Flip Simulation",tabName = "binomial_sim")      
  ) ),
  
  dashboardBody(
  #busy indicator
  shinybusy::add_busy_bar(timeout = 1000, color = "#F00", centered = FALSE, height = "15px"),

	tabItems(

	    tabItem(tabName = "critical_values_tab",
        fluidRow( 
		column(width = 12,
		numericInput("significance",label="Significance",value=0.1,min=0,max=1,step=0.01),
		selectInput("distribution",label="Distribution",choices=c("Std Normal Dist","T Dist","F Dist (Right Tail)"),selected="Std Normal Dist"),
		numericInput("tdist_df",label="T-Distribution Degrees of Freedom", value =1, min=0,max=Inf),
		numericInput("fdist_df1",label="F-Distribution Numerator Degrees of Freedom", value =1, min=1,max=Inf),
		numericInput("fdist_df2",label="F-Distribution Denominator Degrees of Freedom", value =1, min=1,max=Inf),
		selectInput("tails",label="Alternative Hypothesis Type", choices=c("Left Tail","Right Tail", "Two Tail"),selected="Two Tail"),
		
		
		actionButton("calc_critical_values", "Find Critical Values"),
	
		DT::DTOutput("critical_values_breaks")
				

        ))),
		
	    tabItem(tabName = "standard_normal_tab",
        fluidRow( 
		column(width=12,
		actionButton("calc_std_normal", "Calculate Standard Normal"),
		HTML("<br><br>"),
		rHandsontableOutput("standard_normal_zbreaks"),
		DT::DTOutput("standard_normal_output"),
		plotOutput("zdist_ggplot")
		
	
		

        ))),

	    tabItem(tabName = "tdist_tab",
        fluidRow( 
		column(width=12,
		actionButton("calc_tdist","Calculate T-Distribution"),
		HTML("<br><br>"),
		numericInput("tdist_degfreedom",
                       "T-Distribution Degrees of Freedom",
                       min=0,
                       step=1,
                       max=Inf,
                       value=1),
		
		rHandsontableOutput("tdist_breaks"),
		DT::DTOutput("tdist_output"),
		plotOutput("tdist_ggplot")
		
		
		

        ))),
		
		tabItem(tabName = "binomial_sim",
		fluidRow(
		column(width=12,
		numericInput("coincount",
                       "Number of Coins to Flip",
                       min=0,
                       step=1,
                       max=Inf,
                       value=100),
		HTML("<br>"),
		numericInput("repeats_count",
                       "Number of Times to Repeat Experiment",
                       min=0,
                       step=1,
                       max=Inf,
                       value=10000),		
		HTML("<br>"),					   
		numericInput("p_heads",
                       "Probability of Coin Coming Up Heads",
                       min=0,
                       step=0.01,
                       max=1,
                       value=0.5),
					   
		actionButton("run_coinflip_experiment","Run Coin Flip Experiment"),
		HTML("<br>"),
		HTML("<b><font size=5>Longest Runs of Heads</b></font>"),
		DT::DTOutput("longest_heads_run_sim_freqtable"),
		HTML("<br>"),
		plotOutput("longest_heads_run_histogram"),
		HTML("<br>"),
		HTML("<b><font size=5>Longest Runs of Tails</b></font>"),
		DT::DTOutput("longest_tails_run_sim_freqtable"),
		HTML("<br>"),
		plotOutput("longest_tails_run_histogram"),
		HTML("<br>"),
		plotOutput("pct_heads_histogram")


        )))


		


#next three right parentheses close out UI		
	 )))
	  











server <- function(input, output) {

#define default tables of z-scores for breakpoints
zscore_bkpt_df <- cbind.data.frame(zscore_breakpoints = rep(0,10))
tscore_bkpt_df <- cbind.data.frame(tscore_breakpoints = rep(0,10))

  output$standard_normal_zbreaks <- renderRHandsontable(  rhandsontable(zscore_bkpt_df  ) )
  output$tdist_breaks <- renderRHandsontable( rhandsontable(tscore_bkpt_df ) )
  
  #critical values
  observeEvent(input$calc_critical_values, {
  
	#calculate critical values based upon distribution, significance, and alternative hypothesis
    
  #F Distribution
  if(input$distribution=="F Dist (Right Tail)"){critical_values <- qf(1 - input$significance, input$fdist_df1, input$fdist_df2)}
    
  

    #standard normal
	if( (input$distribution == "Std Normal Dist") & (input$tails == "Left Tail")){ critical_values <- qnorm(input$significance , 0, 1) }
	if( (input$distribution == "Std Normal Dist") & (input$tails == "Right Tail")){ critical_values <- -1* qnorm(input$significance , 0, 1) }
	if( (input$distribution == "Std Normal Dist") & (input$tails == "Two Tail")){ critical_values <- c( qnorm(input$significance/2 , 0, 1),-1*qnorm(input$significance/2 , 0, 1)) }
	
	#t distribution
	if( (input$distribution == "T Dist") & (input$tails == "Left Tail")){ critical_values <- qt(input$significance , input$tdist_df) }
	if( (input$distribution == "T Dist") & (input$tails == "Right Tail")){ critical_values <- -1* qt(input$significance , input$tdist_df) }
	if( (input$distribution == "T Dist") & (input$tails == "Two Tail")){ critical_values <- c( qt(input$significance/2 , input$tdist_df),-1*qt(input$significance/2 , input$tdist_df)) }
	
	output$critical_values_breaks <- DT::renderDataTable(as.data.frame(critical_values))
  



} )

  #standard normal
  observeEvent(input$calc_std_normal, {
  
  standard_normal_zbreaks_use <- hot_to_r(input$standard_normal_zbreaks )
  
  sim_dat = data.frame(z = seq(-5,5, length.out = 1001))
  sim_dat$y = dnorm(sim_dat$z, mean = 0, sd=1)
  
  zscores <- standard_normal_zbreaks_use$zscore_breakpoints
  zscores <- sort( setdiff(zscores,0) )
  bkpoints <- sort( c(-Inf, zscores,0, Inf))
  lower <- bkpoints[1:(length(bkpoints) - 1)]
  upper <- bkpoints[2:length(bkpoints)]
  
  pctdata <- pnorm(q = upper) - pnorm(q = lower)
  interval <- paste0(lower, ",", upper)
  
  pctdata_df <- data.frame(interval, lower, upper, pctdata)
  pctdata_df$x_label <- with(pctdata_df, ifelse(is.infinite(lower), upper - 1, .5 * (lower + upper)))
  pctdata_df$x_label <- with(pctdata_df, ifelse(is.infinite(upper), lower + 1, x_label))
  
  sim_dat$standard_normal_sections <- cut(sim_dat$z, breaks = bkpoints)
  
  p1 <- ggplot2::ggplot(sim_dat, aes(z, y)) +
  geom_area(aes(fill = standard_normal_sections)) +
  geom_text(data = pctdata_df, aes(x = x_label, y = 0, label = scales::number(pctdata, .0001)), 
            vjust = 1, size = 20 / .pt, nudge_y = 0.03) +
  scale_x_continuous(breaks = (c(seq(-5, 5, 1), zscores))) +
  theme( axis.text=element_text(size=20, angle =90)) +
  ggtitle("Standard Normal Distribution")
  
    
  output$standard_normal_output <- DT::renderDataTable(pctdata_df %>%
  dplyr::mutate(Percent_of_Data = plyr::round_any(pctdata,0.0001)) %>%
  dplyr::select(Interval = interval, Percent_of_Data ) )
  output$zdist_ggplot <- renderPlot({ p1 }) 
  



} )

  #t distribution
  observeEvent(input$calc_tdist, {
  
    #necessary for all input tables that are editable.
  tdist_breaks_touse <- hot_to_r(input$tdist_breaks )
  degrees_freedom <- input$tdist_degfreedom
  sim_dat = data.frame(t = seq(-5,5, length.out = 1001))
  sim_dat$y = dt(sim_dat$t, df = degrees_freedom)

#fill in t-score bkpts, excluding zero: 0 will always be included
	tscores <- tdist_breaks_touse$tscore_breakpoints
tscores <- sort( setdiff(tscores,0) )
bkpoints <- sort( c(-Inf, tscores,0, Inf))

lower <- bkpoints[1:(length(bkpoints) - 1)]
upper <- bkpoints[2:length(bkpoints)]

pctdata <- pt(q = upper, df = degrees_freedom) - pt(q = lower, df = degrees_freedom)
interval <- paste0(lower, ",", upper)

pctdata_df <- data.frame(interval, lower, upper, pctdata)
pctdata_df$x_label <- with(pctdata_df, ifelse(is.infinite(lower), upper - 1, .5 * (lower + upper)))
pctdata_df$x_label <- with(pctdata_df, ifelse(is.infinite(upper), lower + 1, x_label))

sim_dat$t_sections <- cut(sim_dat$t, breaks = bkpoints)

p1 <- ggplot2::ggplot(sim_dat, aes(t, y)) +
  geom_area(aes(fill = t_sections)) +
  geom_text(data = pctdata_df, aes(x = x_label, y = 0, label = scales::number(pctdata, .0001)), 
            vjust = 1, size = 20 / .pt, nudge_y = 0.03) +
  scale_x_continuous(breaks = (c(seq(-5, 5, 1), tscores))) +
  theme( axis.text=element_text(size=20, angle =90))+
  ggtitle(paste0("t-distribution with ",degrees_freedom," degrees of freedom"))


    
  output$tdist_output <- DT::renderDataTable(pctdata_df %>% 
  dplyr::mutate(Percent_of_Data = plyr::round_any(pctdata,0.0001)) %>%
  dplyr::select(Interval = interval, Percent_of_Data ) )
  output$tdist_ggplot <- renderPlot({ p1 }) 
  



} )

  #coinflip simulation
  observeEvent(input$run_coinflip_experiment, {

#simulation matrix
ht_matrix <- matrix(nrow=input$coincount,ncol = input$repeats_count,data=NA)
for(i in 1:input$repeats_count){
  #size of experiment: 1 coin.  value of 1 "success" = 1 head. value of 0 "success" = 1 tail.
  ht_matrix[,i] <- ifelse(rbinom(n=input$coincount,size=1,prob=input$p_heads)==1,"H","T")
}

heads_p_sim <- colMeans(ht_matrix=='H')
#find longest heads run
longest_heads_run_sim <- numeric()
longest_tails_run_sim <- numeric()

for(i in 1:input$repeats_count){
  
 
  rle_out <- rle(x= ht_matrix[,i])
  df1 <- cbind.data.frame(lengths=rle_out$lengths,values=rle_out$values)
  longest_heads_run_sim[i] <- max(df1$lengths[df1$values=='H'])
  longest_tails_run_sim[i] <- max(df1$lengths[df1$values=='T'])
  
}

#histogram and frequency tables of longest_heads_run_sim and longest_tails_run_sim:
#helps establish probability that coincount flips will yield a string with N heads or tails in a row


longest_heads_run_sim_freqtable <- longest_heads_run_sim %>% table() %>% as.data.frame() %>%
  dplyr::mutate(pct_instances = Freq/sum(Freq)) %>%
  dplyr::mutate(cumpct_instances = cumsum(pct_instances)) %>%
  dplyr::mutate(Pct_Instances = scales::percent(pct_instances),
  Cumulative_Pct_Instances = scales::percent(cumpct_instances)) %>%
  dplyr::select(Longest_Run = '.', Count_Repeats = Freq,Pct_Instances, Cumulative_Pct_Instances,
  pct_instances, cumpct_instances) %>%
  dplyr::mutate(Longest_Run = as.numeric(Longest_Run))
				
longest_tails_run_sim_freqtable <- longest_tails_run_sim %>% table() %>% as.data.frame() %>%
  dplyr::mutate(pct_instances = Freq/sum(Freq)) %>%
  dplyr::mutate(cumpct_instances = cumsum(pct_instances)) %>%
  dplyr::mutate(Pct_Instances = scales::percent(pct_instances,0.01),
  Cumulative_Pct_Instances = scales::percent(cumpct_instances,0.01)) %>%
  dplyr::select(Longest_Run = '.', Count_Repeats = Freq,Pct_Instances, Cumulative_Pct_Instances,
  pct_instances, cumpct_instances)	%>%
  dplyr::mutate(Longest_Run = as.numeric(Longest_Run))	


ev_heads_longestrun <- sum(longest_heads_run_sim_freqtable$Longest_Run*longest_heads_run_sim_freqtable$pct_instances)
ev_tails_longestrun <- sum(longest_tails_run_sim_freqtable$Longest_Run*longest_tails_run_sim_freqtable$pct_instances)

output$longest_heads_run_sim_freqtable <- DT::renderDataTable( 
longest_heads_run_sim_freqtable  %>% 
dplyr::ungroup() %>%
dplyr::select(Longest_Run,Count_Repeats,Pct_Instances,Cumulative_Pct_Instances) %>%
dplyr::rename('% of Instances'=Pct_Instances,
'Longest Run' = Longest_Run, 'Count of Repeats' = Count_Repeats,
'Cumulative % of Instances'=Cumulative_Pct_Instances ) )


output$longest_tails_run_sim_freqtable <- DT::renderDataTable( 
longest_tails_run_sim_freqtable %>% 
dplyr::ungroup() %>%
dplyr::select(Longest_Run,Count_Repeats,Pct_Instances,Cumulative_Pct_Instances) %>%
dplyr::rename(
'Longest Run' = Longest_Run, 'Count of Repeats' = Count_Repeats,
'% of Instances'=Pct_Instances, 'Cumulative % of Instances'=Cumulative_Pct_Instances )
 )


output$longest_heads_run_histogram <- renderPlot({ ggplot2::ggplot(data=longest_heads_run_sim_freqtable,
aes(x=Longest_Run,y=pct_instances))+geom_bar(stat='identity') +
  ggtitle(paste0("Longest Run of Heads in ",
  input$coincount,
  " Coin Flips, by % of Time Occurring. Mean Longest Run ",
  plyr::round_any(ev_heads_longestrun,0.01),
  " Heads"))+
  scale_x_continuous(breaks=seq(1,20,1)) +
  scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.02))+
  xlab("Longest Run of Heads") + ylab("% of Instances")+
  theme(text = element_text(size=15))})
  

output$longest_tails_run_histogram <- renderPlot({ ggplot2::ggplot(data=longest_tails_run_sim_freqtable,
aes(x=Longest_Run,y=pct_instances))+geom_bar(stat='identity') +
  ggtitle(paste0("Longest Run of Tails in ",
  input$coincount,
  " Coin Flips, by % of Time Occurring. Mean Longest Run ",
  plyr::round_any(ev_tails_longestrun,0.01),
  " Tails"))+
  scale_x_continuous(breaks=seq(1,20,1)) +
  scale_y_continuous(labels=scales::percent,breaks=seq(0,1,0.02))+
xlab("Longest Run of Heads") + ylab("% of Instances") +
theme(text = element_text(size=15)) })


output$pct_heads_histogram <- renderPlot({ ggplot2::ggplot(data=heads_p_sim %>% as.data.frame(),
aes(x=`.`))+geom_histogram(bins=100) + 
  ggtitle(paste0("Sampling Distribution of Proportion of Heads, ",
  input$repeats_count," Repeats of ",
  input$coincount," Coin Flips",
  " . Mean Proportion: ",
  plyr::round_any(mean(heads_p_sim),0.0001),
  " . Standard Error: ",
  plyr::round_any(sd(heads_p_sim), 0.0001)
  ))+
  scale_x_continuous(breaks=seq(0,1,0.02))+
  xlab("Proportion of Flips that were Heads")+
  ylab(paste0("Count of Trials (",input$repeats_count," Repeats Total)")) })
  
})






#next right curlybrace ends server
}

# Run the application 
shinyApp(ui = ui, server = server) ##

#test 