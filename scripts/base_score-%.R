## Calculates possession score probability based on throw origin location
## Takes k-nearest neighbors to each coordinate, default k=200
## arr contains the matrix of probabilities by coordinate

library(FNN)
library(reshape2)
library(stringr)
library(tidyverse)
library(plotly)

setwd('C:/Users/hiros/Desktop/Storage/documents/Ultimate-Frisbee-Possession-Analysis/data/audl')

FILENAME = 'audl_possession_outcomes_train.csv'

k = 1000

NROWS = 20
NCOLS = 10

FIELD_WIDTH = 52
FIELD_HEIGHT = 100

df = read.csv(FILENAME)

## normalize to remove bias
normalize = function(a) {
  return ((a - min(a)) / (max(a) - min(a))) }

X_MIN = min(df$x_0)
X_MAX = max(df$x_0)
Y_MIN = min(df$y_0)
Y_MAX = max(df$y_0)

arr = as.data.frame(matrix(nrow = NROWS,ncol=NCOLS))


## debugging
#score_prob_calc(0,1,df,k)
#sub = df[,4:5]

## returns probability of scoring from a given coordinate based on knn
score_prob_calc = function(x,y,df,k) {
  x_n = (x - X_MIN) / (X_MAX - X_MIN)
  y_n = (y - Y_MIN) / (Y_MAX - Y_MIN)
  #print(c(x_n,y_n))
  coord = as.data.frame(matrix(c(x_n,y_n),nrow = 1,ncol=2))

  df_subset_n = as.data.frame(lapply(df[,4:5], normalize))
  #print(head(df_subset_n))
  knn_index = get.knnx(df_subset_n,coord,k)$nn.index
  #print(knn_index)
  score_count = 0
  for (i in 1:k) {
    #print(df[knn_index[1,i],])
    score_count = score_count + df[knn_index[1,i],]$Possession_outcome
  }

  return(score_count/k)
}

ptm = proc.time()


## apply score_prob_calc to each field coordinate
for (i in (1:ncol(arr))) {
  for (j in (1:nrow(arr))) {
    print(c(j,i))
    x_cor = (-FIELD_WIDTH/2) + (i * (FIELD_WIDTH/NCOLS))
    y_cor = j * (FIELD_HEIGHT/NROWS)
    arr[j,i] = score_prob_calc(x_cor,y_cor,df,k)
  }
}

colnames(arr) = 1:NCOLS
arr$Y = 1:NROWS

proc.time() - ptm


## Visualization 

arr_melt = melt(arr,id.vars = 'Y')
colnames(arr_melt) = c('Y','X','Score_Prob')
arr_melt$Y = arr_melt$Y*(FIELD_HEIGHT/NROWS)
arr_melt$Y = 100 - arr_melt$Y

write.csv(arr_melt,file='unconditioned_score-%.csv',row.names = FALSE)

## throw count heatmap
#ggplot(sub,aes(x_0,y_0)) +
#  stat_bin2d(bins = 100) +
#  scale_fill_distiller(palette='Spectral', direction=-1,limits=c(0,50)) 
  #scale_fill_continuous(limits=c(0, 50))
  
## Score % heatmap
p = ggplot(arr_melt,aes(x = X, y = Y, z=Score_Prob, fill = Score_Prob)) + 
  geom_tile() + 
  geom_hline(yintercept=80) +
  scale_fill_distiller(palette='Spectral', direction=-1,name="Score Probability") +
  scale_y_continuous(breaks = c(0,20,40,60,80,100),limits=c(0,100)) +
  ylab('Distance from Endzone') +
  xlab('Target Endzone') +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
#print(p)

## Score % interactive
#ggplotly(p, tooltip=c("x", "y","z"),
#         width = 600,
#         height = 800)
