## Separate dataset into testing and training data
## Default ratio is 80% training, 20% testing
## Whole possessions are kept in same training set
library(stringr)

setwd('C:/Users/hiros/Desktop/Storage/documents/Ultimate-Frisbee-Possession-Analysis/data/audl')

FILENAME = 'audl_possession_outcomes.csv'

TRAIN_RATE = .80

df = read.csv(FILENAME)

train_count = TRAIN_RATE * 100
test_count = (1 - TRAIN_RATE) * 100

heading = colnames(df)

train_df = as.data.frame(matrix(nrow=0,ncol=9))
test_df = as.data.frame(matrix(nrow=0,ncol=9))

colnames(train_df) = heading
colnames(test_df) = heading

start_throw = 1

## extract sets

for (i in (1:nrow(df))) {
  
  if (df[i,]$Throw_outcome == 'Turnover' | df[i,]$Throw_outcome == 'Goal') {
    possession = df[start_throw:i,]
    
    ## randomly assign to train or test
    x = runif(1,min=0,max=train_count+test_count)
    if (x < train_count) {
      train_df = rbind(train_df,possession)
    } else {
      test_df = rbind(test_df,possession)
    }
    start_throw = i + 1
  } else if (df[i,]$Throw_outcome == 'Unknown') {
    start_throw = i + 1
  }
}


## write data

title = str_extract(FILENAME,'.+(?=.csv)')

write.csv(train_df,file=paste0(title,'_train.csv'),row.names = FALSE)
write.csv(test_df,file=paste0(title,'_test.csv'),row.names = FALSE)
