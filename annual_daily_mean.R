#Function Name:annual_daily_mean()
#Caculate the annual daily mean of a county. x is the input dataset and y is the name of the selected county.
#Author: Qingqing Xu

annual_daily_mean <- function (x,y){
  annual_daily_mean <- x %>%
    semi_join(subset(loc,loc$`County Name`==y), by = "site")%>%
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>% 
    summarize(annual_daily_mean = mean(o3, na.rm = TRUE))
  annual_daily_mean 
}

#example: apply the function to Merced County.
annual_daily_mean(daily,"Merced")