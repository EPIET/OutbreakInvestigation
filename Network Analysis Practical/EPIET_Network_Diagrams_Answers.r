#################  Social network practical ############################

# 01. Check that pacman is installed --------------------------------------

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# 02. Install the remaining packages --------------------------------------

# Install the required packages with pacman:
pacman::p_load(tidyverse,
               epicontacts,
               readxl,
               janitor,
               here,
               tidygraph,
               ggraph) 

# 03. Read in data using read_excel() function
## SNA_Practical_outbreak_network_list.xlsx
data_contacts <- read_excel(here("SNA_Practical_outbreak_network_list.xlsx")) %>% 
  clean_names()

## SNA_Practical outbreak_network_linelist.xlsx
data_ll <- read_excel(here("SNA_Practical outbreak_network_linelist.xlsx")) %>% 
  clean_names()

# 04. Examine the structure of your data using glimpse(), str() or any other function
glimpse(data_contacts)
glimpse(data_ll)

# 05. Make a interactive network diagram using the epicontacts package

## Create an epicontacts dataset using make_epicontacts
#arguments: linelist = data_ll, contacts = data_contacts
data <- make_epicontacts(linelist = data_ll, contacts = data_contacts, directed = TRUE)

## use the summary() function to describe the network dataset
summary(data)

## Quickly plot() the network dataset
plot(data)

## Modify plot so that male and female icons are used
##use the helpfile 

?vis_epicontacts

##use the web https://www.reconlearn.org/post/simulated-evd-early.html

# arguments: node_shape = , shapes =
plot(data, node_shape = "sex",
     shapes = c(M = "male", F = "female", Location = "square"),
     selector = FALSE)

## Modify plot so that node colour is by persons location
# arguments: node_color = 
plot(data, node_shape = "sex",
     shapes = c(M = "male", F = "female", Location = "square"),
     selector = FALSE,
     node_color = "location")

## Modify plot so that node colour is by persons location
# arguments: edge_color =
plot(data, node_shape = "sex",
     shapes = c(M = "male", F = "female", Location = "square"),
     selector = FALSE,
     node_color = "location",
     edge_color = "group")

## Export > Save to web page > open file in chrome


############################################# SECTION 2 ######################################################

# 05. Make a interactive network diagram using the epicontacts package
##tip 
## it is okay to ignore:
#"Warning messages: 1: In grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : font family not found in Windows font database" messages

## Convert data_contacts into a tbl_graph object using as_tbl_graph() function
data_tidyg <- as_tbl_graph(data_contacts, directed = FALSE)

## Part A - centrality
## using tidy notation to calculate centrality using centrality_authority() function
data_tidyg <- data_tidyg %>% 
  mutate(centrality = centrality_authority())

# create graph using ggraph, can use layout function to choose graph layout ("auto" picks for you)
p1 <-data_tidyg %>% 
  ggraph(layout = 'auto') #

# 1st step is to add the edges to the graph - add (+) geom_edge_link() to p1 save as p2
p2 <- p1 +
  geom_edge_link()

##2nd step is to add the nodes to the graph use geom_node_point()
### the size and/or colour of nodes can be changed based on the previosuly calculated centrality score
# argument: size = , colour = 

p3 <- p2 +
  geom_node_point(aes(size = centrality, colour = centrality))

## 3rd step Similar to ggplot use scale_color_continuous(guide = 'legend') function to combine size and colour gradients in the legend
p4 <- p3 + scale_color_continuous(guide = 'legend')
  
## 4th step - add labels to the graph based on name within the data geom_node_text()
#Arguments: aes(label = ), colour = ,  repel = TRUE

p5 <- p4 + geom_node_text(aes(label = name), colour = 'black', vjust = 0.4, repel = TRUE)

## 5th step - using ggplot notation set graph theme
p6 <- p5 + theme_graph()


## Part B - define communities on the graph using group_edge_betweenness() function
## use the same method as for centrality but mutate a "community" column and use this to adjust node colour
### hint - try and combine all comands together using pipes %>% and ggplot plot (+) notation

data_tidyg %>% 
  mutate(community = as.factor(group_edge_betweenness())) %>% # ## using tidy notation to define communities using group_edge_betweenness()
  ggraph(layout = 'auto') + ##Can change different layouts
  geom_edge_link() + 
  geom_node_point(aes(colour = community), size = 7) + ##Set nodes to be coloured by community variable, set size = 7
  geom_node_text(aes(label = name), colour = 'black', vjust = 0.4, repel = TRUE) + 
  theme_graph() ## using ggplot notation set graph theme


## Interpret the graphs 
