library(tidyverse)
library(igraph)

#most disliked character
#number of times speak to IMDB rating

#http://pablobarbera.com/big-data-upf/html/02a-networks-intro-visualization.html

tuesdata <- tidytuesdayR::tt_load('2020-08-11')

avatar <- tuesdata$avatar

#network plot how much characters interact with eachother
unique_characters <- unique(avatar$character)

main_characters <- c("Katara", "Sokka", "Zuko", "Jeong Jeong","Kyoshi", "Aang", "Toph", "Roku","Iroh","Ozai", "Bumi", "Zhao", "Azula", "Pakku")

make_edges <- function(characters = main_characters){
  
  vals <- c(main_characters,main_characters)
  vals <- unique( vals )
  edges <- data.frame(t(combn( vals , 2 )))

  colnames(edges) <- c("source","target")

  return(edges)
}

edges <- make_edges(main_characters)

#make node list
make_nodes <- function(characters){
  nodes <- data.frame(name= main_characters, id = seq(1, length(main_characters), 1))
  
  return(nodes)
}

nodes <- make_nodes(main_characters)
weight <- seq(1, nrow(edges), 1)

edges <- data.frame(edges, weight)

search_scenes <- function(source, target){
  
  scene_desc <- filter(avatar, character == 'Scene Description')
  scene_desc <- filter(scene_desc, grepl(pattern = source, x = scene_desc$full_text) == TRUE)
  scene_desc <- filter(scene_desc, grepl(pattern = target, x = scene_desc$full_text) == TRUE)
  
  bending_words <- paste(c('bend','bending','conjure','summon','cast', 'fire punch', 'airbending kick', 'lava'), collapse = "|")
  
  basic_bending <- filter(scene_desc, grepl(bending_words, x=scene_desc$full_text) == TRUE)
  
  count <- nrow(basic_bending)
  
  return(count)
}


#function to get weights

calc_weights <- function(edges){
  
  weight <-  c()
  
  for (row in 1:nrow(edges)){
  
    source <- edges$source[row]
    target <- edges$target[row]

    count <- search_scenes(source, target)
    
    weight <- append(weight,count)
  }
  
  return(weight)
}

#calculate weights
edge_weights <- calc_weights(edges)
edges$weight <- edge_weights
colnames(edges) <- c("source","target","weight")

edges <- edges %>% filter(!(weight < 3))


###set colors for bending elements
all <- c('Aang', 'Roku', 'Kyoshi')
fire <- c('Zuko','Iroh','Zhao','Azula','Ozai', 'Jeong Jeong')
earth <- c('Bumi', 'Toph')
water <- c('Katara','Pakku')
none <- c('Sokka')


g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
g[]
V(g)$size <- log(strength(g))*2+2
V(g)$color <- NA
V(g)$color[V(g)$name %in% fire] <- "red"
V(g)$color[V(g)$name %in% water] <- "blue"
V(g)$color[V(g)$name %in% earth] <- "green"
V(g)$color[V(g)$name %in% none] <- "grey"
V(g)$color[V(g)$name %in% all] <- "white"

plot(g,vertex.label.cex = .4)
plot(g, layout=layout_in_circle, main="Circle")



#brainstorm or search common words (fire blast, water spear, etc)