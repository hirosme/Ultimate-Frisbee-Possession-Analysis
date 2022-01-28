## Calculate possession score probability based on throw origin and destination
## coordinates.
## Takes k-nearest neighbors to each 4d coordinate. Default is k=100

library(tidyverse)
library(parallel)
library(FNN)

ncores = detectCores()

setwd(
  'C:/Users/hiros/Desktop/Storage/documents/Ultimate-Frisbee-Possession-Analysis/data/audl'
)

FILENAME = 'audl_possession_outcomes_train.csv'

df = read.csv(FILENAME) %>% na.omit()

k = 100

NROWS = 24
NCOLS = 10

FIELD_WIDTH = 52
FIELD_HEIGHT = 120

## normalize to remove bias
normalize = function(a) {
  return ((a - min(a)) / (max(a) - min(a)))
}

X_MIN = min(min(df$x_0), min(df$x_1))
X_MAX = max(max(df$x_0), max(df$x_1))
Y_MIN = min(min(df$y_0), min(df$y_1))
Y_MAX = max(max(df$y_0), max(df$y_1))

arr = expand.grid(1:NCOLS, 1:NROWS, 1:NCOLS, 1:NROWS)
colnames(arr) = c('x_0', 'y_0', 'x_1', 'y_1')

##testing
#arr = arr[1:1000,]

## returns probability of scoring given a pair of coordinates
score_prob_calc = function(x_0, y_0, x_1, y_1, df, k) {
  
  #print(c(x_0,y_0,x_1,y_1))
  x_0_n = (x_0 - X_MIN) / (X_MAX - X_MIN)
  y_0_n = (y_0 - Y_MIN) / (Y_MAX - Y_MIN)
  x_1_n = (x_1 - X_MIN) / (X_MAX - X_MIN)
  y_1_n = (y_1 - Y_MIN) / (Y_MAX - Y_MIN)
  coord = as.data.frame(matrix(
    c(x_0_n, y_0_n, x_1_n, y_1_n),
    nrow = 1,
    ncol = 4
  ))
  
  df_subset_n = as.data.frame(lapply(df[, 4:7], normalize))
  #print(head(df_subset_n))
  knn_index = get.knnx(df_subset_n, coord, k)$nn.index
  #print(knn_index)
  score_count = 0
  for (i in 1:k) {
    #print(df[knn_index[1,i],])
    score_count = score_count + df[knn_index[1, i], ]$Possession_outcome
  }
  return(score_count / k)
}


## export environment to cluster
cl = makeCluster(ncores)
clusterExport(cl,'df')
clusterExport(cl,'arr')
clusterExport(cl,'score_prob_calc')
clusterExport(cl,'k')
clusterExport(cl,'NROWS')
clusterExport(cl,'NCOLS')
clusterExport(cl,'FIELD_HEIGHT')
clusterExport(cl,'FIELD_WIDTH')
clusterExport(cl,'normalize')
clusterExport(cl,'X_MIN')
clusterExport(cl,'X_MAX')
clusterExport(cl,'Y_MIN')
clusterExport(cl,'Y_MAX')




ptm = proc.time()
arr$Score_prob = parApply(cl,arr, 1, function(l) {
  
  library(FNN)
  
  print(c(l[1], l[2], l[3], l[4]))
  x_0_cor = (-FIELD_WIDTH / 2) + (l[1] * (FIELD_WIDTH / NCOLS))
  y_0_cor = l[2] * (FIELD_HEIGHT / NROWS)
  x_1_cor = (-FIELD_WIDTH / 2) + (l[3] * (FIELD_WIDTH / NCOLS))
  y_1_cor = l[4] * (FIELD_HEIGHT / NROWS)
  if (y_0_cor <= 100) {
    return(score_prob_calc(x_0_cor, y_0_cor, x_1_cor, y_1_cor, df, k))
  } else {
    return(NA)
  }
})

proc.time() - ptm

colnames(arr) = c('X_0', 'Y_0', 'X', 'Y', 'Score_Prob')
arr$Y_0 = arr$Y_0 * (FIELD_HEIGHT / NROWS)
arr$Y_0 = 100 - arr$Y_0
arr$Y = arr$Y * (FIELD_HEIGHT / NROWS)
arr$Y = 100 - arr$Y

write.csv(arr, file = 'conditioned_score-%.csv', row.names = FALSE)
#arr = read.csv(CONDITIONED_FILENAME)

coord = c(5, 35)
test = arr[arr$X_0 == coord[1] & arr$Y_0 == coord[2], 3:5]

p = ggplot(test, aes(
  x = X,
  y = Y,
  z = Score_Prob,
  fill = Score_Prob
)) +
  geom_tile() +
  geom_hline(yintercept = 80) +
  geom_hline(yintercept = 0) +
  scale_fill_distiller(palette = 'Spectral',
                       direction = -1,
                       name = "Score Probability") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                     limits = c(-20, 100)) +
  ylab('Distance from Endzone') +
  xlab('Target Endzone') +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) +
  geom_point(aes(x = coord[1], y = coord[2]))



print(p)
