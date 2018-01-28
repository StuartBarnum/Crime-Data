sea <- seattle %>% mutate(Offense.Code = as.factor(Offense.Code))

seattle_gg <- get_map("Seattle", maptype = "toner-lite",
                      zoom = 11)

Violent_Crimes <- c("ASSAULT", "HOMICIDE", "ROBBERY")

#remove zero latitude and longitude elements, for mapping
seattle_nonzero <- seattle %>%
  filter(Latitude != 0 & Longitude != 0)

data <- seattle_nonzero %>%
  filter(is.element(Summarized.Offense.Description, Violent_Crimes))

data <- seattle_nonzero %>% 
  filter(Summarized.Offense.Description == "NARCOTICS")

ggmap(seattle_gg, darken = c(.01, "black")) + 
  geom_bin2d(data = seattle_nonzero, 
             aes(x = Longitude, y = Latitude), 
             bins = 200) +
  scale_fill_gradient(trans = "log10")

levels(seattle$Summarized.Offense.Description)

#note zeros for both longitude and latitude:
range(seattle$Longitude)
range(seattle$Latitude)

#find the zeros:
zero_position <- seattle %>%
  filter(Longitude == 0 | Latitude == 0)

#The following d.f. contians no obserations. So the Latitude 
#entry is zero iff the Longitude entry is zero
temp <- zero_position %>%
  filter(Longitude != Latitude) #one of zero iff the other is

#try to get an idea as to whether the zero position elements
#can be seen as a non-stratefied random sample of the dataset
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
    
  #initialize list of items to be returned
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
    ggtitle("Assessing the Effect of Missingness of Position Values") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_blank()) +
    guides(fill = guide_legend(reverse=TRUE))
    
  return_list[["plot"]] <- plot 
  
  return(return_list)
  }
assessment <- calculate_conditional_distribution(Zone.Beat)
assessment[[2]]
df <- assessment[[1]]
plot <- assessment[[2]]
interactive_plot <- 
  ggplotly(plot)
interactive_plot %>% 
  layout(margin = list(b=50, l=60, t=80)) 
  
    add_trace(y = ~TRUE, name = 'Tree 2') %>%
    add_trace(y = ~FALSE, name = 'Tree 2')
       

      #legend=dict(orientation="h"))
      #legend = list(x = 100, y = 0.5))
  
plot
#note prostitution always has a label

ggplot(df[[1]]) +
  geom_col(aes(x = Summarized.Offense.Description, 
               y = conditional_distribution, fill = LatEqualsZero)) +
  labs(x = "fasd", y = "conditional distribution",
       fill = "position\ndata\nmissing") 

+
  guides(fill = guide_legend(nrow = 3))

fred <- zero_assess %>% filter(LatEqualsZero == TRUE)
mutate(sum_sample = sum(m)) #%>%
  
  
  group_by(Latitude != 0) %>%
  mutate
  

  

