# Remove Everything from the R directory
rm(list=ls(all=TRUE))

# Set Seed
set.seed(123)

## Load Library
library(readxl)
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(lubridate) # Char to Date
library(tidyr)
library(widyr)
library(igraph)
library(ggraph)



## Load Data
# (Place Your Code for loading the data) 
myFile <- file.choose()
metadata_a <- read.csv(myFile)
#View(Data)
names(metadata_a)

#metadata_a <- as.character(metadata_a)
#class(metadata_a)
#class(metadata_a$Department)

#title
metadata_a$Title <- as.character(metadata_a$Title)
class(metadata_a$Title)
andeavor_title <- data_frame(id = metadata_a$?.._id, title = metadata_a$Title)
andeavor_title

#department
andeavor_dept <- data_frame(id = metadata_a$?.._id, dept = metadata_a$Department) %>%
  unnest(dept)
andeavor_dept

#site
andeavor_site <- data_frame(id = metadata_a$?.._id, site = metadata_a$Site)
andeavor_site %>%
  select(site) %>%
  sample_n(5)

#description

metadata_a$Event.Description <- as.character(metadata_a$Event.Description)
class(metadata_a$Event.Description)
andeavor_desc <- data_frame(id = metadata_a$?.._id, desc = metadata_a$Event.Description) %>%
  unnest(desc)
andeavor_desc

#tokenize
#title

andeavor_title <- andeavor_title %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words)
andeavor_desc <- andeavor_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words)

andeavor_desc
andeavor_title
  
#Word count
andeavor_title %>%
  count(word, sort = TRUE)
andeavor_desc %>%
  count(word, sort = TRUE)

#My stop words
undesirable_words = read.csv("~/Desktop/Andeavor/2. Raw_Dataset/2. raw_dataset-1/Mike_Syed_Text_Mining_Sub_Section/Data4.csv",stringsAsFactors = FALSE,header=FALSE)$V1
#undesirable_words
#undesirable_words <- as.list(undesirable_words)
#class(undesirable_words)

my_stopwords <- data_frame(word = c(as.character(1:10),
                                    "1", "2", "3", "ee",
                                    "fa", "pse", "4", "6410",
                                    "145", "600", "t3", "l1"))
#class(my_stopwords)
andeavor_title <- andeavor_title %>%
  anti_join(my_stopwords)
  #filter(!word %in% undesirable_words) 
  
andeavor_desc <- andeavor_desc %>%
  anti_join(my_stopwords)
#  filter(!word %in% undesirable_words) 
  
andeavor_desc
andeavor_title

#most comon departments
andeavor_dept %>%
  group_by(dept) %>%
  count(sort = TRUE)

#lower upper case
#mutate() = toupper())

#Pairwise
title_word_pairs <- andeavor_title %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)
title_word_pairs

desc_word_pairs <- andeavor_desc %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)
desc_word_pairs

#Plot of title
set.seed(1234)
title_word_pairs %>%
  filter(n >= 20) %>%
  graph_from_data_frame()%>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.1, "lines")) +
  theme_void()

#plot of description
set.seed(234)
desc_word_pairs %>%
  filter(n >=60) %>%
  graph_from_data_frame()%>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.1, "lines")) +
  theme_void()

