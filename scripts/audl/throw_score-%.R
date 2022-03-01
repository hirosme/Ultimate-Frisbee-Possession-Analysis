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

FILENAME = 'audl_possession_outcomes.csv'
FILENAME2 = 'unconditioned_score-%.csv'

df = read.csv(FILENAME) %>% na.omit()
uncond_df = read.csv(FILENAME2)

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
arr[,c('Callahan-%', 'Completion_Score-%','Start_Score-%', 'End_Score-%',
       'Score_Rate_Added', 'Completion-%','EPO','Average_Neighbor_Dist')] = NA
colnames(arr) = c('X_0', 'Y_0', 'X_1', 'Y_1','Callahan_Rate', 
                  'Completion_Score_Rate','Start_Score_Rate', 'End_Score_Rate',
                  'Score_Rate_Added', 'Completion_Rate','EPO',
                  'Average_Neighbor_Dist')

##testing
#arr = arr[1:1000,]

## returns probability of scoring given a pair of coordinates
score_prob_calc = function(l,x_0, y_0, x_1, y_1, df, k, unc_df) {
  
  ## uncond index transformation
  x_0_unc = l[1]
  y_0_unc = 100 - 5*l[2]
  x_1_unc = l[3]
  y_1_unc = 100 - 5*l[4]
  if (y_1_unc < 0) y_1_unc = 0
  
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
  knn = get.knnx(df_subset_n, coord, k)
  knn_index = knn$nn.index
  knn_avg = median(knn$nn.dist)
  #print(knn_mean)
  Score_count = 0
  Callahan_count = 0
  Incompletion_count = 0
  Completion_count = 0
  for (i in 1:k) {
    #print(df[knn_index[1,i],])
    throw = df[knn_index[1,i],]
    if (throw$Throw_outcome=='Callahan') {
      Callahan_count = Callahan_count + 1
    } else if (throw$Throw_outcome=='Turnover') {
      Incompletion_count = Incompletion_count + 1
    } else if (throw$Throw_outcome=='Completion' |
               throw$Throw_outcome=='Goal') {
      Completion_count = Completion_count + 1
      Score_count = Score_count + throw$Possession_outcome
    }
  }
  Cal_rate = Callahan_count / k
  Inc_rate = Incompletion_count / k
  Com_rate = Completion_count / k
  
  if (Completion_count > 0) {
    Com_score_rate = Score_count / Completion_count
  } else {
    Com_score_rate = 0
  }
  
  
  Start_score_rate = unc_df[unc_df$X_0==x_0_unc & unc_df$Y_0==y_0_unc,3]
  End_score_rate = Score_count / k
  #print(x_1_unc)
  #print(y_1_unc)
  Turnover_cost = unc_df[unc_df$X_0==x_1_unc & unc_df$Y_0==y_1_unc,4]

  Score_rate_added = End_score_rate - Start_score_rate
  
  EV_added = (-1)*Cal_rate + (-1)*Turnover_cost*Inc_rate + End_score_rate

  val = c(Cal_rate,
        Com_score_rate,
        Start_score_rate,
        End_score_rate,
        Score_rate_added,
        Com_rate,
        EV_added,
        knn_avg)
  #print(Turnover_cost)

  #print(Start_score_rate)
  return(val)
}


## export environment to cluster
cl = makeCluster(ncores)
clusterExport(cl,'df')
clusterExport(cl,'uncond_df')
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
results = parApply(cl,arr[,c(1:4)], 1, function(l) {
  
  library(FNN)
  
  print(c(l[1], l[2], l[3], l[4]))
  x_0_cor = (-FIELD_WIDTH / 2) + (l[1] * (FIELD_WIDTH / NCOLS))
  y_0_cor = l[2] * (FIELD_HEIGHT / NROWS)
  x_1_cor = (-FIELD_WIDTH / 2) + (l[3] * (FIELD_WIDTH / NCOLS))
  y_1_cor = l[4] * (FIELD_HEIGHT / NROWS)
  if (y_0_cor <= 100) {
    return(score_prob_calc(l,x_0_cor, y_0_cor, x_1_cor, y_1_cor, 
                           df, k, uncond_df))
  } else {
    return(rep(NA,8))
  }
}) %>% as.data.frame() %>% t() %>% rbind()

arr[,c(5:12)] = results

proc.time() - ptm

arr$Y_0 = arr$Y_0 * (FIELD_HEIGHT / NROWS)
arr$Y_0 = 100 - arr$Y_0
arr$Y_1 = arr$Y_1 * (FIELD_HEIGHT / NROWS)
arr$Y_1 = 100 - arr$Y_1
arr$Average_Neighbor_Dist = arr$Average_Neighbor_Dist * 5
arr$Accuracy = apply(arr,1,function(x) {
  x = as.numeric(x[12])
  if (is.na(x)) {return(NA)}
  else if (x < .575) {return ('High')}
  else if (x < 1.163) {return ('Medium')}
  else {return('Low')}
})

write.csv(arr, file = 'conditioned_score-%.csv', row.names = FALSE)
arr = read.csv('conditioned_score-%.csv')

# coord = c(10, 35)
# test = arr[arr$X_0 == coord[1] & arr$Y_0 == coord[2],]
# 
# p = ggplot(test, aes(
#   x = X_1,
#   y = Y_1,
#   z = End_Score_Rate,
#   fill = End_Score_Rate
# )) +
#   geom_tile() +
#   geom_hline(yintercept = 80) +
#   geom_hline(yintercept = 0) +
#   scale_fill_distiller(palette = 'Spectral',
#                        direction = -1,
#                        name = "Score Probability") +
#   scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
#                      limits = c(-30, 110)) +
#   ylab('Distance from Endzone') +
#   xlab('Target Endzone') +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     panel.grid = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank()
#   ) +
#   geom_point(aes(x = coord[1], y = coord[2]))
# 
# 
# 
# print(p)
