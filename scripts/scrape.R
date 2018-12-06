library(tidyverse)
library(rvest)
library(skimr)
library(lubridate)
library(plotly)
library(rpart)
library(rpart.plot)
library(RCurl)

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

df <- table[[1]]

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

master_specs <- tibble()

tried_list <- c()



df2 <- df %>% filter(!name %in% tried_list)

for(i in c(1:length(df2))){
  
  plane_name <- df2[i,] %>% 
    pull(name)
  
  plane_key <- plane_name %>% str_replace_all(" ", "_")
  
  plane_url <- paste0("https://en.wikipedia.org/wiki/", plane_key)
  
  tried_list <- c(tried_list, plane_name)
  
  print(paste0("Scraping ", plane_name))
  
  plane_specs <- tibble()
  
  if(url.exists(plane_url)){
    plane_specs <- plane_name %>% specscraper()
    
  }
  
  
  if(url.exists(plane_url)){
    
    print(paste0("Found ", (plane_specs %>% nrow()), " attributes"))
    
    master_specs <- master_specs %>% 
      bind_rows(plane_specs)
  }
  
}

