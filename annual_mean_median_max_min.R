#Function Name: Annual_mean_median_max_min()
#Calculate the annual mean, median, max and min of all sites that have either of the two special strings in the Site Name. 
#Input x is the original list data. Input y and z is the special strings. In this case, x is o3.filelist. y and z are  “San” or “Santa”.

Annual_mean_median_max_min <- function(x,y,z){
  annual <- x %>%
    rbindlist() %>%
    group_by(site = as.factor(site))%>%
    semi_join(filter(loc, grepl(paste(c(y,z), collapse="|"), loc$`Site Name`)), by = "site")%>%
    mutate(year = format(date, "%Y")) %>%
    group_by(site,year) %>%
    summarize(annual_mean = mean(obs, na.rm = TRUE), annual_median = median(obs, na.rm = TRUE),annual_max = max(obs, na.rm = TRUE),annual_min = min(obs, na.rm = TRUE) )
  annual
}

#example
Annual_mean_median_max_min(o3.filelist,"San","Santa") 
```