get_gdp_plot = function(req_country,real_or_nominal){
  
  cntry_df = lng_int_df %>% 
    filter(Country == req_country)
  
  if(real_or_nominal == "Nominal"){
    
    plt_df = cntry_df %>% 
      filter(`Subject Descriptor` == "Gross domestic product, current prices" & Units == "U.S. dollars") %>%
      # filter(`Subject Descriptor` == "Gross domestic product, constant prices" & Units == "National currency") %>%
      filter(!is.na(value)) %>% 
      filter(year <= `Estimates Start After`)
    
  } else {
    
    plt_df = cntry_df %>% 
      # filter(`Subject Descriptor` == "Gross domestic product, current prices" & Units == "U.S. dollars") %>%
      filter(`Subject Descriptor` == "Gross domestic product, constant prices" & Units == "National currency") %>%
      filter(!is.na(value)) %>% 
      filter(year <= `Estimates Start After`)
    
  }
  
  end_gdp = plt_df %>% 
    filter(year == max(year)) %>% 
    pull(value)
  
  highest_val = plt_df %>% 
    filter(value == max(value))
  
  lowest_val = plt_df %>% 
    filter(value == min(value))
  
  plt_df %>% 
    ggplot(aes(x = year,y = value,group = 1))+
    # geom_line()+
    geom_area(fill = ifelse(end_gdp >= highest_val$value, "#27b7ac", "#d82466"), alpha = 0.3) +
    geom_line(color = ifelse(end_gdp >= highest_val$value, "#27b7ac", "#d82466"), size = 2) +
    geom_point(data = highest_val, aes(x = year, y = value), color = "#27b7ac", size = 10) +
    geom_point(data = lowest_val, aes(x = year, y = value), color = "#d82466", size = 10) +
    expand_limits(y = 0) +
    theme_void()
  
  
}


get_shareofworld_plt = function(req_country){
  
  req_plot_df = lng_int_df %>% 
    filter(Country == req_country) %>% 
    filter(!is.na(value)) %>% 
    filter(year == `Estimates Start After`) %>% 
    filter(`Subject Descriptor` == "Gross domestic product based on purchasing-power-parity (PPP) share of world total")
  
  percentage = req_plot_df %>% pull(value)
  
  data = tibble(
    category = c("Value", "Remaining"),
    value = c(percentage, 100 - percentage)
  )
  
  data = data %>%
    arrange(desc(category)) %>%
    mutate(
      fraction = value / sum(value),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1))
    )
  
  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3.75, fill = category)) +
    geom_rect(color = "white") +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    theme_void() +
    scale_fill_manual(values = c("Value" = "#27b7ac", "Remaining" = "#e0e0e0")) +
    annotate("text", x = 2, y = 0.5, label = scales::percent(x = percentage,accuracy = 0.01,scale = 1),
             size = 20, fontface = "bold", color = "black") +
    
    theme(legend.position = "none")
  
  
}


get_unemp_plot = function(req_country){
  
  cntry_df = lng_int_df %>% 
    filter(Country == req_country)
  
  
  plt_df = cntry_df %>% 
    # filter(`Subject Descriptor` == "Gross domestic product, current prices" & Units == "U.S. dollars") %>%
    filter(`Subject Descriptor` == "Unemployment rate") %>%
    filter(!is.na(value)) %>% 
    filter(year <= `Estimates Start After`)
  
  
  end_unemp = plt_df %>% 
    filter(year == max(year)) %>% 
    pull(value)
  
  highest_val = plt_df %>% 
    filter(value == max(value))
  
  lowest_val = plt_df %>% 
    filter(value == min(value))
  
  latest_val = plt_df %>% 
    filter(year == `Estimates Start After`) %>% 
    pull(value) %>% 
    scales::percent(accuracy = 0.01,scale = 1)
  
  plt_df %>% 
    ggplot(aes(x = year,y = value,group = 1))+
    # geom_line()+
    geom_area(fill = ifelse(end_unemp >= highest_val$value, "#d82466", "#27b7ac"), alpha = 0.3) +
    geom_line(color = ifelse(end_unemp >= highest_val$value, "#d82466", "#27b7ac"), linewidth = 2) +
    geom_point(data = highest_val, aes(x = year, y = value), color = "#d82466", size = 10) +
    geom_point(data = lowest_val, aes(x = year, y = value), color = "#27b7ac", size = 10) +
    annotate("text", x = median(plt_df$year), y = median(plt_df$value), label = latest_val, 
             color = ifelse(end_unemp >= highest_val$value, "#d82466", "#27b7ac"), size = 30, vjust = 2.5, fontface = "bold") +
    expand_limits(y = 0) +
    theme_void()
  
  
}

get_inflation_plot = function(req_country){
  
  cntry_df = lng_int_df %>% 
    filter(Country == req_country)
  
  
  plt_df = cntry_df %>% 
    # filter(`Subject Descriptor` == "Gross domestic product, current prices" & Units == "U.S. dollars") %>%
    filter(`Subject Descriptor` == "Inflation, average consumer prices" & Units == "Percent change") %>%
    filter(!is.na(value)) %>% 
    filter(year <= `Estimates Start After`)
  
  
  end_infl = plt_df %>% 
    filter(year == max(year)) %>% 
    pull(value)
  
  highest_val = plt_df %>% 
    filter(value == max(value))
  
  lowest_val = plt_df %>% 
    filter(value == min(value))
  
  latest_val = plt_df %>% 
    filter(year == `Estimates Start After`) %>% 
    pull(value) %>% 
    scales::percent(accuracy = 0.01,scale = 1)
  
  plt_df %>% 
    ggplot(aes(x = year,y = value,group = 1))+
    # geom_line()+
    geom_area(fill = ifelse(end_infl >= highest_val$value, "#d82466", "#27b7ac"), alpha = 0.3) +
    geom_line(color = ifelse(end_infl >= highest_val$value, "#d82466", "#27b7ac"), linewidth = 2) +
    geom_point(data = highest_val, aes(x = year, y = value), color = "#d82466", size = 10) +
    geom_point(data = lowest_val, aes(x = year, y = value), color = "#27b7ac", size = 10) +
    annotate("text", x = median(plt_df$year), y = max(plt_df$value), label = latest_val, 
             color = ifelse(end_infl >= highest_val$value, "#d82466", "#27b7ac"),size = 30, vjust = 3, fontface = "bold") +
    expand_limits(y = 0) +
    theme_void()
  
  
}


