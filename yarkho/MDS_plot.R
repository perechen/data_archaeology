library(tidyverse)
library(paletteer)
library(ggtext)


#read csv
corneille = read.csv("yarkho_corneille.csv", header=T,row.names = 1)

#fix colnames
colnames(corneille) = str_replace_all(colnames(corneille), "^X", "V")


# calculate distances
dist = corneille %>% as.matrix() %>%  dist("manhattan", upper=T, diag=T)/15


# points to df
mds.cmdscale <- as.data.frame(cmdscale(dist))

# add names
mds.cmdscale$names <- rownames(mds.cmdscale)

# add groups
mds_df = mds.cmdscale %>% mutate(color = str_extract(names, ".$"))


## plot
# color vector
cols=paletteer_d("ggsci::uniform_startrek")

ggplot(mds_df, aes(V1, V2, label=names,color=color)) +

  geom_text(size=5.5,position = position_jitter(width=-0.3,height = 0.6), 
            hjust = "center", vjust = "right",fontface="bold") +
  theme_classic() +
  labs(x="",y="",
       title=paste("<b style='color:", cols[1],"'>Comedies</b> and <b style='color:", cols[2], "'>Tragedies</b> of Pierre Corneille"),
       subtitle="Data come from large-scale quantiative study on distinistive features of classic dramatic genres in Corneille done by Boris I. Yarkho <b>in 1920s</b>. Each text was represented across 15 features that Yarkho tried to synthesise into clear 'comedy' vs. 'tragedy' cut. This study served as a general introduction to Yarkho's grand project of quantiative methodology for literary studies. 120 pages long work was first published only in 2006.") +
  theme(plot.title=element_markdown(lineheight = 1.1,size = 22,family="Bookman")) +
  xlim(c(-60,60)) +
  guides(color=F) +
  scale_color_paletteer_d("ggsci::uniform_startrek") 


