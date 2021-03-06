Crime Data Analysis
================
Stuart Barnum
1/28/2018

Initial test of Plotly graphics on GitHub

``` r
library(tidyverse)
library(plotly)
#sanfrancisco <- read.csv("sanfrancisco_incidents_summer_2014.csv")
#seattle <- read.csv("seattle_incidents_summer_2014.csv")
zero_assess <- seattle %>%
  mutate(LatEqualsZero = Latitude == 0) %>%
  group_by(LatEqualsZero, Summarized.Offense.Description) %>%
  summarize(m = n()) %>%
  group_by(Summarized.Offense.Description) %>%
  mutate(n = sum(m)) %>%
  ungroup() %>%
  mutate(sum_total = sum(m)) %>%
  group_by(LatEqualsZero) %>%
  mutate(sum_zero_test = sum(m)) %>%
  mutate(total_dist = n / sum_total) %>%
  mutate(marginal_distribution = m / sum_zero_test)
#result: visual comparision of total_dist and marginal_dist does not
#suggest a systematic relationship between omision and non-omision
#of the longitudinal and latitudinal data.

#Wrap the above into a function, to more quickly examine possible dependencies 
#with respect to other columns. use equo() and !! to handle the passing of the 
#column name into the. Note that the d.f. seattle needs to be present in the 
#environment
calculate_conditional_distribution <- function(column) {
  column <- enquo(column)
  zero_assess <- seattle %>%
    mutate(Position_Data_Missing = Latitude == 0) %>%
    group_by(Position_Data_Missing, !!column) %>%
    summarize(m = n()) %>%
    group_by(!!column) %>%
    mutate(n = sum(m)) %>%
    ungroup() %>%
    mutate(sum_total = sum(m)) %>%
    group_by(Position_Data_Missing) %>%
    mutate(sum_zero_test = sum(m)) %>%
    mutate(total_dist = n / sum_total) %>%
    mutate(conditional_distribution = m / sum_zero_test) %>%
    ungroup() %>%
    group_by(!!column) 
    
  #initialize list of items to be returned (a d.f. and a ggplot)
  return_list <- list()
  return_list[["df"]] <- zero_assess
  
  #create visualizations for assessment of independence
  plot <- zero_assess %>% 
    filter(n > 10) %>%
    mutate(Position_Data_Missing = 
             ifelse(Position_Data_Missing == TRUE,
                    "missing", "not missing")) %>%
    ggplot() + 
    geom_col(aes_(x = column, y = ~conditional_distribution, 
                  fill = ~Position_Data_Missing)) +
    labs(x = str_c(quo_name(column), 
                   "\n(move cursor over bars to see the categories)"), 
         y = "conditional distribution",
         fill = "position data") +
    ggtitle("Assessing the Effect of Missingness of Position Data") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_blank()) +
    guides(fill = guide_legend(reverse=TRUE))
    
  return_list[["plot"]] <- plot 
  
  return(return_list)
  }
assessment <- calculate_conditional_distribution(Zone.Beat)
df <- assessment[[1]]
plot <- assessment[[2]]
plot
```

![](Crime_Data_Analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#interactive_plot <- 
#  ggplotly(plot)
#interactive_plot %>% 
#  layout(margin = list(b=50, l=60, t=80)) 
```
