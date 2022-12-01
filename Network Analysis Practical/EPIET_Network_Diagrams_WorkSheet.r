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

# 03. Read in data using read_excel() and the here() function
## SNA_Practical_outbreak_network_list.xlsx
data_contacts <- read_excel() %>% 
  clean_names()

## SNA_Practical outbreak_network_linelist.xlsx
data_ll <- read_excel() %>% 
  clean_names()

# 04. Examine the structure of your data using glimpse(), str() or any other function
glimpse()
glimpse()

# 05. Make a interactive network diagram using the epicontacts package

## Create an epicontacts dataset using make_epicontacts 
#arguments: linelist = data_ll, contacts = data_contacts
data <- make_epicontacts()

## use the summary() function to describe the network dataset
summary()

## Quickly plot() the network dataset
plot()

## Modify plot so that male and female icons are used based on the sex of the person 
##use the helpfile 

?vis_epicontacts

##use the web https://www.reconlearn.org/post/simulated-evd-early.html

# arguments: node_shape = , shapes =
plot(data, 
     selector = FALSE)

## Modify plot above so that it also colours the node by persons location,
# arguments: node_color = 
plot(data, 
     selector = FALSE) 

## Modify plot so that node colour is by persons location
# arguments: edge_color =
plot(data, 
     selector = FALSE) 

## Export > Save to web page > open file in chrome


############################################# SECTION 2 ######################################################

# 05. Make a interactive network diagram using the epicontacts package
##tip 
## it is okay to ignore:
#"Warning messages: 1: In grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : font family not found in Windows font database" messages

## Convert data_contacts into a tbl_graph object using as_tbl_graph() function
data_tidyg <- as_tbl_graph(, directed = FALSE)

## Part A - centrality
## using tidy notation to calculate centrality using centrality_authority() function
data_tidyg <- data_tidyg %>% 
  mutate(centrality = )

# create graph using ggraph, can use layout function to choose graph layout ("auto" picks for you)
p1 <-data_tidyg %>% 
  ggraph() 

# 1st step is to add the edges to the graph - add (+) geom_edge_link() to p1 save as p2
p2 <- p1 
  

##2nd step is to add the nodes to the graph use geom_node_point()
### the size and/or colour of nodes can be changed based on the previosuly calculated centrality score
# argument size = , colour = 

p3 <- p2 +
  geom_node_point(aes())

## 3rd step Similar to ggplot use scale_color_continuous(guide = 'legend') function to combine size and colour gradients in the legend
p4 <- p3 
  
## 4th step - add labels to the graph based on name within the data geom_node_text()
#Arguments: aes(label = ), colour = ,  repel = TRUE
p5 <- p4 + geom_node_text()

## 5th step - using ggplot notation set graph theme
p6 <- p5 + theme_graph()


## Part B - define communities on the graph using group_edge_betweenness() function
## use the same method as for centrality but mutate a "community" column and use this to adjust node colour
### hint - try and combine all comands together using pipes %>% and ggplot plot (+) notation

data_tidyg %>% 
  mutate(community = as.factor()) %>% # ## using tidy notation to define communities using group_edge_betweenness()
  ggraph() + ##Can change different layouts
  geom_edge_link() + 
  geom_node_point() + ##Set nodes to be coloured by community variable, set size = 7
  geom_node_text() + 
  theme_graph() ## using ggplot notation set graph theme


## Interpret the graphs 
