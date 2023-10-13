library(tidytext)
library(tidyverse)
library(dplyr)
library(tokenizers)
library(cowplot)
input.dir <- "YOUR DIR"
files.v <- dir(input.dir, "\\.txt$")
novella_lemma <- list ()
for(i in 1:length(files.v)){
  novella_lemma [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8" )
}

files.v <- gsub("_", " ", files.v)
files.v <- gsub("\\.txt", "", files.v)
names(novella_lemma) <- files.v


pv <- list() #a list for conatining the graphs to visualize them at once
a <- list() #list for the regression lines of the plots

for (i in 1:length(novella_lemma)){
  lemma_nopunct <- novella_lemma[[i]] %>% 
    filter(V3 != "PUNCT") #%>% 
  #anti_join() #stopw?
  #split into equal chunks
  lemma_group_all <- split(lemma_nopunct, ceiling((1:nrow(lemma_nopunct))/300))
  
  verb_portion <- lapply(lemma_group_all, function(x)
    nrow(x %>% filter(V3=="VERB")) / nrow(x))
  
  verb_portion_df <- as_tibble(t(bind_rows(verb_portion)))
  verb_portion_df$szakasz <- 1:nrow(verb_portion_df)
  colnames(verb_portion_df) <- c("verb", "szakasz")
  
  # before plotting, check if the text has more than 4 segments
  # and add "loess" smoothing just in that case
  if (nrow(verb_portion_df) > 4){
    a[[i]] <- geom_smooth(method = "loess", span= 1.2, se =F, color = "darkblue")
  }
  else{
    a[[i]] <- geom_line(alpha=0.5)
  }
  
  pv[[i]] <- ggplot(verb_portion_df, aes(szakasz, verb))+
    geom_point()+
    geom_line(alpha = 0.5)+
    a[[i]]+
    xlab("Szakasz (300 szó)")+
    ylab("Igék aránya")+
    #ggtitle( "Esti Kornél"
    #         , sub = paste(as.character(j), ". fejezet", sep = "")
    #)+
    ggtitle(files.v[[i]])+
    theme_bw()
}

# plot all graphs at once
plot_at_once <- plot_grid(plotlist = pv, byrow = FALSE, 
                          nrow = 4, ncol = 3)

# draw joint title to all the graphs
title <- ggdraw() + 
  draw_label(
    "Krúdy Gyula, 12 Szindbád-történet\nIgék arányának alakulása",
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
