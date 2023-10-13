input.dir <- "YOUR DIR"
files.v <- dir(input.dir, "\\.txt$")
make.file.l <- function(files.v, input.dir){
  text.l <- list()
  for(i in 1:length(files.v)){
    text.v <- scan(paste(input.dir, files.v[i], sep="/"),
                   what="character", sep="\f", quote = "", encoding = "UTF-8")
    text.l[[files.v[i]]] <- text.v
  }
  return(text.l)
}
my.corpus.l <- make.file.l(files.v, input.dir)
files.v <- gsub("koszt-esti_", "", files.v)
files.v <- gsub("\\.txt", "", files.v)

#preprocess the text for tokenization
regenyek <- c()
for (i in 1:length(my.corpus.l)) {
  regenyek[[i]] <- unlist(my.corpus.l[[i]], recursive = TRUE, use.names = TRUE)
  regenyek [[i]] <- gsub("([0-9]+)([A-zöüóőúéáűí])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("(– )([A-ZÖÜÓŐÚÉÁŰÍ])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("(- )([A-ZÖÜÓŐÚÉÁŰÍ])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("(\\.\\.\\.)( [A-ZÖÜÓŐÚÉÁŰÍ])", "\\.\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("([A-zzöüóőúéáűí])(-)", "\\1", regenyek[[i]])
  regenyek [[i]] <- gsub("([[:punct:]])([A-zzöüóőúéáűí])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("Dr\\. ", "Dr ", regenyek[[i]], ignore.case = TRUE)
  regenyek [[i]] <- gsub("stb\\. ", "stb ", regenyek[[i]])
  regenyek [[i]] <- gsub("Özv\\. ", "Özv ", regenyek[[i]], ignore.case = TRUE)
  regenyek [[i]] <- gsub("ifj\\. ", "ifj ", regenyek[[i]])
  regenyek [[i]] <- gsub("ún\\. ", "ún ", regenyek[[i]])
  regenyek [[i]] <- gsub("St\\. ", "st ", regenyek[[i]])
  regenyek [[i]] <- gsub("( [A-zzöüóőúéáűí])(\\.)", "\\1", regenyek[[i]])
  regenyek [[i]] <- gsub("([.?!])( [a-zöüóőúéáűí])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("([.?!])( [a-zöüóőúéáűí])", "\\2", regenyek[[i]])
  regenyek [[i]] <- gsub("([.?!])([\\)] [a-zöüóőúéáűí])", "\\2", regenyek[[i]])
}


#tokenize into sentenes and words
library(tokenizers)
token_sent <- sapply(regenyek, tokenize_sentences)
token_sent2 <- lapply(regenyek, unlist, recursive = TRUE, use.names = TRUE)
sentence_words <- sapply(token_sent2, tokenize_words)

# create equal sizes parts of the words
# size from narrative-arc_sentiment.R and narrative-arc_verb.R
pm <- list() #list for graphs to plot them at once
a <- list() #list for regression curves for plots
for (j in 1:length(novella_lemma)){
  lemma_nopunct <- novella_lemma[[j]] %>% 
    filter(V3 != "PUNCT") #%>% 
  #anti_join() #stopw?
  lemma_nopunct <- lemma_nopunct$V1
  lemma_group <- split(lemma_nopunct, ceiling(seq_along(lemma_nopunct)/300))
  
  chunk_no <- length(lemma_group)
  chunked_text <- split(sentence_words[[j]], 
                        cut(seq_along(sentence_words[[j]]),chunk_no, labels = F))
  
  sentence_length <- list ()
  for (i in 1:length(chunked_text)) {
    sentence_length [[i]] <- sapply(chunked_text[[i]], length)
    sentence_length [[i]] <- sentence_length[[i]][which(sentence_length[[i]] !=0)]
  }
  
  sentence_length_mean <- sapply(sentence_length, mean)
  #*-1 to invert the results
  sentence_length_mean1 <-sentence_length_mean*-1
  
  #make tibble for plot
  sl_df <- tibble("meanlength" = sentence_length_mean1)
  sl_df <- sl_df %>% 
    mutate(row_id=row_number())
  
  # before plotting, check if the text has more than 4 segments
  # and add "loess" smoothing just in that case
  if (nrow(sl_df) > 4){
    a[[i]] <- geom_smooth(method = "loess", span= 1.2, se =F, color = "darkblue")
  }
  else{
    a[[i]] <- geom_line(alpha=0.5)
  }
  
  pm [[j]] <- ggplot(sl_df, aes(row_id, meanlength))+
    geom_point()+
    geom_line(alpha=0.5)+
    a[[i]]+
    xlab("Szakasz") +
    ylab("Átlagos mondathossz * -1")+
    #ggtitle( "Esti Kornél"
    #         , sub = paste(as.character(j), ". fejezet", sep = "")
    #)+
    ggtitle(files.v[[j]])+
    theme_bw()
}


# plot all graphs at once
plot_at_once <- plot_grid(plotlist = pm, byrow = FALSE, 
                          nrow = 6, ncol = 3)

# draw joint title to all the graphs
title <- ggdraw() + 
  draw_label(
    "Kosztolányi Dezső: Esti Kornél\nÁtlagos mondathosszúság",
    fontface = 'bold',
    size = 24,
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




