library(tidytext)
library(tidyverse)
library(dplyr)
library(tokenizers)
library(cowplot)

#load sentiment lexicon
positive <- scan(paste("YOR DIR /PrecoPos.txt", sep="/"),
                what="character", sep="\f", quote = "", encoding = "UTF-8")
negative <- scan(paste("YOUR DIR/ PrecoNeg.txt", sep="/"),
                what="character", sep="\f", quote = "", encoding = "UTF-8")
#combine into one dataframa
positive <- tibble(positive)%>%
  mutate(szentiment = "pozitív")
colnames(positive) = c("word", "szentiment")
negative <- tibble(negative)%>%
  mutate(szentiment = "negatív")
colnames(negative) = c("word", "szentiment") 
szentiment <- bind_rows(mutate(negative),
                        mutate(positive))


#load lemmas of texts 
input.dir <- "YOUR DIR"
files.v <- dir(input.dir, "\\.txt$")
novella_lemma <- list ()
for(i in 1:length(files.v)){
  novella_lemma [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8" )
}
files.v <- gsub("_", " ", files.v)
files.v <- gsub("\\.txt", "", files.v)
names(novella_lemma) <- files.v


p <- list() #this list will conatain all the graphs of the texts
a <- list() #a list which keeps the info about the regression graphs
for (j in 1: length(novella_lemma)){
  # first keep words and leave punctuation
  lemma_nopunct <- novella_lemma[[j]] %>% 
    filter(V3 != "PUNCT") #%>% 
  #anti_join() #stopw?
  lemma_nopunct <- lemma_nopunct$V1
  # create equal sizes parts of the words
  lemma_group <- split(lemma_nopunct, ceiling(seq_along(lemma_nopunct)/300))
  lemma_group_df <- lapply(lemma_group, tibble)
  #add zero to dataframe if there is no sentiment
  add_zero <- tibble(szentiment = c("pozitív", "negatív"),
                     n = c(0,0))
  
  #loop inside the loop
  lemma_group_sentiment <- list()
  for(i in 1:length(lemma_group_df)){
    colnames(lemma_group_df[[i]]) <- "lemma"
    
    #count the sentiment words in each segment 
    lemma_group_sentiment [[i]] <- lemma_group_df[[i]] %>%
      unnest_tokens(word, lemma) %>% 
      inner_join(szentiment) %>% 
      bind_rows(add_zero) %>% 
      group_by(szentiment) %>% 
      ungroup() %>% 
      count(szentiment, sort = TRUE)%>% 
      mutate(ossz = sum(n)/nrow(lemma_group_df[[i]])*100)
    
    #lemma_group_sentiment[[i]] <- lemma_group_sentiment[[i]][1,3]
    lemma_group_sentiment[[i]]$szakasz <- i
  }
  # End of the embedded loop
  
  lemma_group_sentiment <- bind_rows(lemma_group_sentiment)
  
  # before plotting, check if the text has more than 4 segments
  # and add "loess" smoothing just in that case
  if (nrow(lemma_group_sentiment) > 8){
    a[[j]] <- geom_smooth(method = "loess", span= 1.2, se =F, color = "darkblue")
  }
  else{
    a[[j]] <- geom_line(alpha=0.5)
  }
  
  #visualization
  p[[j]] <- ggplot(lemma_group_sentiment, aes(szakasz, ossz))+
    geom_point(alpha=0.5)+
    geom_line(alpha=0.5)+
    #if quadtratic:   method = "lm",formula = y ~ x + I(x^2)
    # span: between 1-1.2
    #geom_smooth(method = "loess", span= 1.2, se =F, color = "darkblue")+
    a[[j]]+
    xlab ("Szakasz (300 szó)")+
    ylab("Szentiment érték")+
    #ggtitle( "Esti Kornél"
    #         , sub = paste(as.character(j), ". fejezet", sep = "")
    #)+
    ggtitle(files.v[[j]])+
    theme_bw()
}
# End of the loops

# plot all graphs at once
plot_at_once <- plot_grid(plotlist = p, byrow = FALSE, 
                          nrow = 4, ncol = 3)

# draw joint title to all the graphs
title <- ggdraw() + 
  draw_label(
    "Krúdy Gyula, 12 Szindbád-történet\nSzentimentanalízis",
    fontface = 'bold',
    size = 20,
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

# plot
plot_grid(
  title, plot_at_once,
  ncol = 1,
  rel_heights=c(0.1, 1)
)
