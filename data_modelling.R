# Apriori algorithm
library("arules")

training_set <- sub_data[sub_data$Date < "2016-01-01",]
testing_set <- sub_data[sub_data$Date >= "2016-01-01",]

pre_16_record <- calculate_team_perf_pairs("2016-01-01")

# Do clustering later
set.seed(66172)
# Clustering teams by win ratios
win_clusters <- kmeans(team_point_ratios, 6)
win_clusters$size
table(row.names(team_point_ratios), win_clusters$cluster)

d<-dist(team_point_ratios)
hc<-hclust(d)
#Hierarchical
plot(hc, main="Hierarchical clustering by team point ratio performance")

# This tells us how many points team a has gotten in matches with team b
# team_point_ratios["Liverpool","Man United"]
# team_point_ratios["Man United", "Liverpool"]

# Data modelling
tree1 <- rpart(Points ~ TeamName + Opponent, data=formed_data)
plot(tree1)
text(tree1)
# This is interesting. We see that this is picking up what can be described as tiers in the teams. Depending on the the 'tier' of the team on each side we expect a certain number of points 

training_set<-formed_data[formed_data$Date<as.Date("2016-01-01"),]
testing_set<-formed_data[formed_data$Date>=as.Date("2016-01-01"),]


party_tree<-ctree(Points ~ TeamName + Opponent +HomeOrAway, data=training_set)
plot(party_tree, main="Predicting match results by Decision Tree")
predictions<-predict(party_tree, newdata=testing_set)
table(predictions, testing_set$Points)

log_w_reg<-glm(W ~ TeamName + Opponent + HomeOrAway, data=training_set, family="binomial")
predictions<-predict(log_w_reg, type="response", newdata=testing_set)
prop.table(table(round(predictions), testing_set$Points),1)

log_d_reg<-glm(D ~ TeamName + Opponent + HomeOrAway, data=training_set, family="binomial")
predictions<-predict(log_d_reg, type="response", newdata=testing_set)
prop.table(table(round(predictions), testing_set$Points),1)

log_l_reg<-glm(L ~ TeamName + Opponent + HomeOrAway, data=training_set, family="binomial")
predictions<-predict(log_l_reg, type="response", newdata=testing_set)
prop.table(table(round(predictions), testing_set$Points),1)

#tree2<-rpart(FTR~ TeamName+Opponent+HomeOrAway, data=formed_data)
#plot(tree2)
#text(tree2)


# Make new columns to attempt logistic regression
formed_data$W<-0
formed_data$L<-0
formed_data$D<-0
formed_data$W[formed_data$Points==3]<-1
formed_data$L[formed_data$Points==0]<-1
formed_data$D[formed_data$Points==1]<-1




party_tree<-ctree(Result ~., data=sub_data[,c(2,7,8,13:15)], controls=ctree_control(maxdepth=4))
plot(party_tree, main="Predicting match results by Decision Tree")

truth <- testing_set$Points
#Lets get a baseline. All guess majority class, all wins.
base_preds <- rep(3,196)
confusionMatrix(base_preds, truth)
```
My baseline prediction here is all matches are wins as there are equal wins and losses in my long dataset. This benchmark is 37.24% accurate.

```{r}
fix_preds <- function(model, testing_set){
  predictions<-floor(predict(model, testing_set))
  predictions[predictions==2]=3
  return(predictions)
}

#First lets fit a simple tree. Team name and Opponents team name
tree1 <- rpart(Points ~ TeamName + Opponent, data=training_set)
plot(tree1)
text(tree1)

# These predictions are continuous floats. need to change to ints.
tree_predictions1<-fix_preds(tree1, testing_set)
confusionMatrix(tree_predictions1, truth)

#First lets fit a more complex tree. Adding HomeOrAway
tree2 <- rpart(Points ~ TeamName + Opponent + HomeOrAway, data=training_set)
plot(tree2)
text(tree2)

tree_predictions2 <- fix_preds(tree2, testing_set)
# Actually makes us less accurate. 42.86%
confusionMatrix(tree_predictions2, truth)


party_tree<-ctree(Points ~ TeamName + Opponent + HomeOrAway, data=training_set, controls=ctree_control(maxdepth=6))
plot(party_tree, main="Predicting match results by Decision Tree")
predictions<-fix_preds(party_tree, testing_set)
confusionMatrix(predictions, testing_set$Points)
```

So my trees are more accurate than simple guessing the majority class. 43.88% versus 37.24%. However that is not a huge improvement. Though here I only made use of the teams on both sides and the home/away status. Lets see if adding some calculated metrics can improve my prediction accuracy.

```{r}
training_set<-merge(training_set, historical_point_record, by=c("TeamName", "Opponent"))

testing_set<-merge(testing_set, historical_point_record, by=c("TeamName", "Opponent"))

# Get means and sd for goals for given teams to build assumption that it is a random process
means<-tapply(training_set$Goals, training_set$TeamName, FUN=mean)
sds<-tapply(training_set$Goals, training_set$TeamName, FUN=sd)
goal_frame<-data.frame(cbind(rownames(sds),means, sds))
rownames(goal_frame)<-NULL
colnames(goal_frame)<-c("TeamName","MeanGoals","SDGoals")
goal_frame$MeanGoals<-as.numeric(as.character(
  goal_frame$MeanGoals))
goal_frame$SDGoals<-as.numeric(as.character(
  goal_frame$SDGoals))

table(sub_data$Goals)/sum(sub_data$Goals)
# What distribution are goals drawn from?
# Poisson
norm_goals<-merge(goal_frame, goal_frame, by=NULL)#, type="full")
win_rate<-vector()
draw_rate<-vector()
lose_rate<-vector()
norm_favour<-vector()

for(i in 1:nrow(norm_goals)){
  reps<-10000
  a<-rnorm(reps, mean=norm_goals$MeanGoals.x[i], sd=norm_goals$SDGoals.x[i])
  b<-rnorm(reps, mean=norm_goals$MeanGoals.y[i], sd=norm_goals$SDGoals.y[i])
  sum(a<b)/reps
  apois<-rpois(reps, lambda=norm_goals$MeanGoals.x[i])
  bpois<-rpois(reps, lambda=norm_goals$MeanGoals.y[i])
  norm<-sum(a<b)/reps
  win<-sum(apois>bpois)/reps
  draw<-sum(apois==bpois)/reps
  lose<-sum(apois<bpois)/reps
  
  win_rate<-c(win_rate,win)
  draw_rate<-c(draw_rate,draw)
  lose_rate<-c(lose_rate, lose)
  norm_favour<-c(norm_favour,norm)
}

norm_goals<-cbind(norm_goals,
                  win_rate,
                  draw_rate,
                  lose_rate)
colnames(norm_goals) <- c("TeamName", "MeanGoals", "SDGoals", "Opponent", "OpponentMeanGoals",
                          "OpponentSDGoals", "WinRate", "DrawRate","LoseRate")

training_set<-join(training_set, norm_goals, by=c("TeamName", "Opponent"))
testing_set<-join(testing_set, norm_goals, by=c("TeamName", "Opponent"))

# Replace messy joined names
colnames(training_set) <- gsub('\\.(x|y)','', colnames(training_set),)
colnames(testing_set) <- gsub('\\.(x|y)','', colnames(testing_set),)

```
Lets see how a model does on these new metrics. I fit several other trees at this point but none of them beat the 43.88% accuracy scored above. Until...

```{r}
set.seed(377199)
#rfm = random forest model.
# Increases accuracy to 46.94%
rfm1 <- randomForest(Result ~ TeamName + Opponent +
                       HistoricalPointRatio +
                       HistoricalOpponentPointRatio +
                       WinRate+ LoseRate + DrawRate,
                     data=training_set, ntree=50)
rf_pred<-predict(rfm1, testing_set)
confusionMatrix(rf_pred, testing_set$Result)
# Look at the relative importance of the different features in my random forest.
rfm1$importance

```
46.94% is a nice improvement over 43.88%.
