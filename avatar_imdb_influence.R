library(tidyverse)
library(rlist)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2020-08-11')

avatar <- tuesdata$avatar

main_characters <- c("Aang","Sokka","Katara", "Toph", "Iroh", "Zuko", "Azula","Zhao")

avatar$word_total <- sapply(avatar$character_words, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

avatar <- avatar %>% group_by(character, book_num, chapter_num ) %>% mutate('CharWords'= sum(word_total)) %>% ungroup

avg_rating = 9.2

avatar <- avatar %>% mutate('delta' = imdb_rating - avg_rating)

get_words_per_rating <- function(person){

  dat <- filter(avatar, character == person)

  words <- dat$CharWords[which(avatar$chapter_num != dplyr::lag(dat$chapter_num))]
  ratings <- dat$imdb_rating[which(avatar$chapter_num != dplyr::lag(dat$chapter_num))]
  
  words_rating <- data.frame(words, ratings)
  words_rating <- unique(words_rating)
  
  
  return(words_rating)  
}

#plot x_y and get linear regression coeff
chars <- c()
coeffs <- c()
for (character in main_characters){
  
  x_y <- get_words_per_rating(character)
  colnames(x_y) <- c('X','Y')
  
  #linear regress model
  fit <- lm(Y~X, data = x_y)
  plot(Y~X, data = x_y)
  coeff <- summary(fit)$adj.r.squared
  
  #append stuff
  chars <- append(chars, character)
  coeffs <- append(coeffs, coeff)
  
}


influence <- data.frame(chars, coeffs) %>% arrange(coeffs)

colors <- c('red','red','red','red','green','grey','blue','lightblue')

influence %>% 
  ggplot(aes(fct_rev(fct_reorder(chars,coeffs)),coeffs))+
  geom_col(fill = colors)+
  scale_fill_manual(values = colors)+
  theme_classic()+
  xlab("Character")+
  ylab("Influence")
