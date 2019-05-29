# library(tidyverse)
# library(rvest)
# library(skimr)
# library(lubridate)
# library(plotly)
# library(rpart)
# library(rpart.plot)
# library(RCurl)
# library(ggrepel)

#install.packages("pacman")
library(pacman)
p_load(tidyverse)
p_load(rvest)
p_load(skimr)
p_load(lubridate)
p_load(plotly)
p_load(rpart)
p_load(rpart.plot)
p_load(RCurl)
p_load(ggrepel)


url <- "https://en.wikipedia.org/wiki/List_of_fighter_aircraft"

fighter_html <- read_html(url)
fighter_html%>%
  html_nodes("table") %>%
  head()

fighter_html%>%
  html_nodes("a") %>%
  head()

fighter_html%>%
  html_nodes("div") %>%
  head()

fighter_html%>%
  html_nodes("link") %>%
  head()

fighter_html%>%
  html_nodes("div") %>%
  html_nodes("h2") %>% 
  head()

fighter_html %>% 
  html_nodes("h2")
  html_nodes("tbody")
  
#### This bit worked
table <- fighter_html %>% 
  html_table(fill = T)

(df <- table[[1]])

skim(df)

#df$`No. built`

spade <- df$`No. built` %>% first() %>% str_trunc(width = 2, side = "left", ellipsis = "") %>% str_trunc(width = 1, side = "right", ellipsis = "") 

colnames(df) <- c("name", "country", "year", "number", "status")

df <- df %>% 
  separate(col = number, into = c("junk", "number"), sep = spade) %>% 
  select(-junk)

df <- df %>% 
  mutate(number = number %>% str_remove_all(regex("\\D")))

df <- df %>% 
  mutate(number = number %>% as.numeric(), 
         year = year %>% as.numeric()) 

df %>% 
  ggplot(aes(x = year, y = number))+
  geom_point()

(plot <-df %>% 
  ggplot(aes(x = year, y = number, col = status, text = name, text2 = country))+
  geom_point())
ggplotly(plot)

## oh dear, the [note 13] superscript added a lot of numbers to some!

df <- table[[1]]

skim(df)

#df$`No. built`

spade <- df$`No. built` %>% first() %>% str_trunc(width = 2, side = "left", ellipsis = "") %>% str_trunc(width = 1, side = "right", ellipsis = "") 

colnames(df) <- c("name", "country", "year", "number", "status")

df <- df %>% 
  separate(col = number, into = c("junk", "number"), sep = spade) %>% 
  select(-junk)

df %>% skim()

df <- df %>% 
  mutate(number = number %>% str_remove_all(regex("\\[.+\\]"))) %>% 
  mutate(number = number %>% str_remove_all(regex("\\D")))

df <- df %>% 
  mutate(number = number %>% as.numeric(), 
         year = year %>% as.numeric()) 

df %>% 
  ggplot(aes(x = year, y = number))+
  geom_point()

(plot <-df %>% 
    ggplot(aes(x = year, y = number, col = status, text = name, text2 = country))+
    geom_point())
ggplotly(plot)

df %>% 
  group_by(country) %>% 
  summarise(total = sum(number)) %>% 
  arrange(desc(total)) 

df %>% 
  filter(year > 1945) %>% 
  group_by(country) %>% 
  summarise(total = sum(number)) %>% 
  arrange(desc(total)) 

mod <- df %>% 
  select(-name) %>% 
  rpart(formula = number~.)
rpart.plot(mod)
plotcp(mod)
summary(mod)

#### now testing on F14 for spec scrape ----

newurl <- "https://en.wikipedia.org/wiki/Grumman_F-14_Tomcat"
f14_html <- read_html(newurl)

(divs <- f14_html %>% 
  html_nodes("div") %>% 
  html_nodes("h2"))

list <- f14_html %>% 
  html_nodes("li") %>% 
  html_text() %>% 
  tibble()

colnames(list) <- "raw"

list <- list %>%
  filter(str_detect(raw,regex("\\w: \\d")))

(list <- list %>% 
  filter(str_detect(raw, "\\^") == F))

(list <- list %>% 
  separate(raw, into = c("attr", "rem", "rem2", "rem3"), sep = ":"))

(list <- list %>% 
  mutate(si = case_when(str_detect(rem, regex("(\\w+)")) == T ~ str_extract(rem, regex("\\(.+\\)")))))

(list <- list %>% 
  mutate(si = si %>% str_remove_all("\\(")) %>% 
  mutate(si = si %>% str_remove_all("\\)"))) 

(list <- list %>% 
  separate(si, into = c("value", "unit"), sep = " "))

(list <- list %>% 
  mutate(value = value %>% str_remove_all(",")) %>% 
  mutate(value = value %>% as.numeric()) %>% 
  filter(is.na(value) == F) %>% 
  mutate(unit = unit %>% str_remove_all(regex("[[:punct:]]"))) %>% 
  select(attr, value, unit))

#### creating a function that we can loop over ---

plane_name <- df %>% 
  filter(name == "Grumman F-14 Tomcat") %>% 
  pull(name)


specscraper <- function(plane_name){
  
  plane_key <- plane_name %>% str_replace_all(" ", "_")
  
  plane_url <- paste0("https://en.wikipedia.org/wiki/", plane_key)
  
  print(url.exists(plane_url))
  
  if(url.exists(plane_url)){
    plane_html <- read_html(plane_url)
    
    
    list <- plane_html %>% 
      html_nodes("li") %>% 
      html_text() %>% 
      tibble()
    
    colnames(list) <- "raw"
    
    list <- list %>%
      filter(str_detect(raw,regex("\\w: \\d")))
    list <- list %>% 
      filter(str_detect(raw, "\\^") == F)
    
    list <- list %>% 
      separate(raw, into = c("key", "rem", "rem2", "rem3"), sep = ":")
    
    list <- list %>% 
      mutate(si = case_when(str_detect(rem, regex("(\\w+)")) == T ~ str_extract(rem, regex("\\(.+\\)"))))
    
    list <- list %>% 
      mutate(si = si %>% str_remove_all("\\(")) %>% 
      mutate(si = si %>% str_remove_all("\\)"))
    
    list <- list %>% 
      separate(si, into = c("value", "unit"), sep = " ")
    
    list <- list %>% 
      mutate(value = value %>% str_remove_all(",")) %>% 
      mutate(value = value %>% as.numeric()) %>% 
      filter(is.na(value) == F) %>% 
      mutate(unit = unit %>% str_remove_all(regex("[[:punct:]]"))) %>% 
      select(key, value, unit) %>% 
      mutate(name = plane_name)
    
  }
  
  
}

plane_specs <- plane_name %>% specscraper()

# master_specs <- tibble()
# 
# tried_list <- c()

tried_list2 <- c()

demo_filler <- tibble()


for(j in c(1:5)){
  
  set.seed(j)
  print(paste0("loop number ", j))
  
  df2 <- df %>% filter(!name %in% tried_list2)
  remainder <- df2 %>% nrow()
  
  print(paste0("loop number ", j, " remaining: ", remainder))
  
  df3 <- df2 %>% sample_n(10)
  
  for(i in c(1:length(df3))){
    
    plane_name <- df3[i,] %>% 
      pull(name)
    
    plane_key <- plane_name %>% str_replace_all(" ", "_")
    
    plane_url <- paste0("https://en.wikipedia.org/wiki/", plane_key)
    
    # tried_list <- c(tried_list, plane_name)
    tried_list2 <- c(tried_list2, plane_name)
    
    print(paste0("Scraping ", plane_name))
    
    plane_specs <- tibble()
    
    if(url.exists(plane_url)){
      plane_specs <- plane_name %>% specscraper()
      
    }
    
    
    if(url.exists(plane_url)){
      
      print(paste0("Found ", (plane_specs %>% nrow()), " attributes"))
      
      # master_specs <- master_specs %>%
      #   bind_rows(plane_specs)
      
      demo_filler <- demo_filler %>%
        bind_rows(plane_specs)
    }
    
  }
  
  
}


#master_specs %>% write_csv("intdata/master_specs.csv")


##### Need to widen 

master_specs <- read_csv("intdata/master_specs.csv")

wide_m <- master_specs %>% 
  mutate(unit = str_replace_all(unit, "²", "2")) %>% 
  unite(key, key, unit, sep = " ") %>% 
  spread(key = "key", value = "value") 

master_specs$unit %>% unique()

units <- master_specs %>% 
  mutate(unit = str_replace_all(unit, "²", "2")) %>% 
  group_by(key, unit) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

attribs <- master_specs %>% 
  group_by(key) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

worker_specs <- master_specs

worker_specs <- worker_specs %>% 
  mutate(unit = str_replace_all(unit, "²", "2")) %>% 
  mutate(conversion = case_when(unit == "ft" ~ .3048,
                                 unit == "lb" ~ .453592,
                                 unit == "hp" ~ 0.7457,
                                 unit == "mph" ~ 1.60934,
                                unit == "mi" ~ 1.60934,
                                 unit == "ftmin" ~ 0.00508,
                                 unit == "miles" ~ 1.60934,
                                unit == "ft2" ~ .092903,
                                unit == "knot" ~ 1.852,
                                unit == "kn" ~ 1.852,
                                unit == "nm" ~ 1.852,
                                unit == "nmi" ~ 1.852,
                                unit == "in" ~ 25.4),
         newval = case_when(is.na(conversion) == F ~ value*conversion,
                            is.na(conversion) == T ~ value*1),
         unit = case_when(unit == "ft" ~ "m",
                          unit == "lb" ~ "kg",
                          unit == "hp" ~ "kW",
                          unit == "mph" ~ "kmh",
                          unit == "mi" ~ "km",
                          unit == "ftmin" ~ "ms",
                          unit == "miles" ~  "km",
                          unit == "ft2" ~ "m2",
                          unit == "knot" ~ "kmh",
                          unit == "kn" ~ "kmh",
                          unit == "nm" ~ "km",
                          unit == "nmi" ~ "km",
                          unit == "in" ~ "mm",
                          TRUE~unit))

(units <- worker_specs %>% 
  mutate(unit = str_replace_all(unit, "²", "2")) %>% 
  group_by(key, unit) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)))

(attribs <- worker_specs %>% 
  group_by(key) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)))

(topattribs <- attribs %>% 
  head(n = 20) %>% 
  pull(key))

(goodunits <- c("m", "kg", "kW", "kmh", "ms", "m2", "mm"))

worker_specs <- worker_specs %>% 
  unique()

## Here is the error!
wide_w <- worker_specs %>% 
  filter(key %in% topattribs) %>% 
  filter(unit %in% goodunits) %>% 
  mutate(value = case_when(is.na(conversion) == T ~ newval,
                           is.na(conversion) == F ~ value)) %>% 
  select(-newval, - conversion) %>% 
  unite(key, key, unit, sep = " ") %>% 
  spread(key = "key", value = "value") 

## Fixed

wide_w. <- worker_specs %>% 
  filter(key %in% topattribs) %>% 
  filter(unit %in% goodunits) %>% 
  mutate(value = case_when(is.na(conversion) == F ~ newval,
                           is.na(conversion) == T ~ value)) %>% 
  select(-newval, - conversion) %>% 
  unite(key, key, unit, sep = " ")

wide_w <- wide_w. %>% 
  unite(dupchecker, name, key, remove = F) %>% 
  filter(duplicated(dupchecker) == F) %>% 
  select(-dupchecker) %>% 
  unique() %>% 
  spread(key = "key", value = "value")


#### real data !!
wide_w %>% skim()

wide_w <- wide_w %>% 
  left_join(df, by = "name") 

wide_w %>% skim()

(plot <- wide_w %>% 
  ggplot(aes(x = `Empty weight kg`, y = `Maximum speed kmh`, size = number, col = year, text = name, text2 = country))+
  geom_point()+
  scale_colour_gradientn(colours = rainbow(10)))
ggplotly(plot)

(plot <- wide_w %>% 
  ggplot(aes(x = `Empty weight kg`, y = `Maximum speed kmh`, size = number, col = year, text = name))+
  geom_point()+
  scale_colour_gradientn(colours = rainbow(10))+
  facet_grid(.~status)+
  ggtitle(label = "plane speed vs weight"))
ggplotly(plot)

(plot <- wide_w %>% 
  ggplot(aes(x = `Powerplant kW`, y = `Empty weight kg`, size = number, col = `Maximum speed kmh`, text = name))+
  geom_point()+
  scale_colour_gradientn(colours = rainbow(10))+
  ggtitle(label = "plane power and speed"))
ggplotly(plot)

p <- ggplotly(plot)

Sys.setenv("plotly_username"="aidan.morrison")


chart_link = api_create(p, filename = "jets2")
chart_link

colnames(wide_w) <- colnames(wide_w) %>% str_replace_all(" ", "_")

mod <- wide_w %>% 
  select(-name, -country) %>% 
  rpart(formula = Maximum_speed_kmh~.)
mod %>% rpart.plot()
mod %>% plotcp()
summary(mod)

mod <- wide_w %>% 
  select(-name, -country) %>% 
  rpart(formula = Maximum_speed_kmh~., cp = 0.0005)
mod %>% rpart.plot()
mod %>% plotcp()
summary(mod)

wide_w %>% 
  write_csv("intdata/ftplanes.csv")

### some more vis

wide_w$Guns_mm %>% unique()

wide_w %>% skim()

topcountries <- df %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(7) %>% 
  pull(country)


(plot <-wide_w %>% 
  filter(Guns_mm %>% is.na() == F) %>% 
  filter(country %in% topcountries) %>% 
  mutate(Guns_mm = (Guns_mm %>% round())) %>% 
  filter(Guns_mm %in% c(8,13,20,30)) %>% 
  ggplot(aes(x = year, y = Service_ceiling_m, col = country))+
  geom_point()+
  facet_grid(Guns_mm~.)+
  ggtitle(label = "Fighter service ceiling grouped by gun calibre"))

topcountries <- master_specs %>% 
  left_join(df, by = "name") %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(12) %>% 
  pull(country)

topplanes <- wide_w %>% 
  arrange(desc(number)) %>% 
  head(n = 20) %>% 
  pull(name)

wide_w %>% 
  filter(country %in% topcountries) %>% 
  skim()

(plot <- wide_w %>%
  #filter(country %in% topcountries) %>%
  filter(name %in% topplanes) %>% 
  ggplot(aes(x = Empty_weight_kg, y = Powerplant_kW, col = country, size = number, label = name))+
  geom_point()+
  ggrepel::geom_label_repel()+
  ggtitle(label = "Power vs Weight"))

topplanes <- df %>% 
  arrange(desc(number)) %>% 
  head(n = 15) %>% 
  pull(name)

(plot <-df %>% 
    ggplot(aes(x = year, y = number , col = status, text = name, text2 = country))+
    geom_point()+
    ggtitle(label = "History of Fighters")+
    geom_label_repel(data = df %>% filter(name %in% topplanes), aes(x = year, y = number, label = name)))
ggplotly(plot)

(plot <- wide_w %>% 
    ggplot(aes(x = `Empty_weight_kg`, y = `Maximum_speed_kmh`, size = number, col = `year`, text = name))+
    geom_point()+
    scale_colour_gradientn(colours = rainbow(10))+
    scale_x_continuous(limits = c(0,20000))+
    ggtitle(label = "plane weight and speed"))
ggplotly(plot)
  

  
  
  