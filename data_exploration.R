# Libraries.
# Best plotting library ever
library("ggplot2")
# For data munging
library("reshape")
# More data munginf
library("reshape2")
# Same
library("plyr")
# Recursive Partitioning decision trees
library("rpart")
# The party package provides nonparametric regression trees
# for nominal, ordinal, numeric, censored, and multivariate
# responses.
library("party")
# Apriori algorithm
library("arules")


# Clear R environment.
rm(list=ls())

# Set working directory .
setwd("~/UCD/DataMining/HW2/")
# This creates an object called cleaned_data in our environment.
load("CleanedData/cleaned_data.Rda")

#Take a subset of our data
subset <- cleaned_data[,c(1:6,10,71,75,76,77)]
sub_data <- melt(subset, id=c("Date","FTHG","FTAG","FTR",
                              "Referee","Season","HomePoints",
                              "AwayPoints", "TotalGoals"))
sub_data <- rename(sub_data, c("value"="TeamName"))
sub_data <- rename(sub_data, c("variable"="HomeOrAway"))

# Calculate the respective points for each team.
sub_data$Points <- 0
sub_data$Points[sub_data$HomeOrAway=="AwayTeam"] <- sub_data$AwayPoints[sub_data$HomeOrAway=="AwayTeam"]
sub_data$Points[sub_data$HomeOrAway=="HomeTeam"] <- sub_data$HomePoints[sub_data$HomeOrAway=="HomeTeam"]

sub_data$Goals <-0
sub_data$Goals[sub_data$HomeOrAway=="AwayTeam"] <- sub_data$FTAG[sub_data$HomeOrAway=="AwayTeam"]
sub_data$Goals[sub_data$HomeOrAway=="HomeTeam"] <- sub_data$FTHG[sub_data$HomeOrAway=="HomeTeam"]

opposition <-cbind.data.frame(rep(cleaned_data$AwayTeam, 2),
                              rep(cleaned_data$HomeTeam, 2))
vec<-vector()
for(i in 1:nrow(opposition)){
  if(sub_data$TeamName[i]==opposition[i, 1]){
   vec[i] <- as.character(opposition[i, 2])
  }
  else{
    vec[i] <- as.character(opposition[i, 1])
  }
}
sub_data$Opponent <-as.factor(vec)

# Introduce a calculate metric. goal ratio. Which tells us
# what proportion of the goals in that match that team scored.
# To prevent division by zero NaNs. The round later removes this.
ratio_offset<-1e-6
sub_data$GoalRatio<-round((sub_data$Goals + ratio_offset)/
                          (sub_data$TotalGoals + ratio_offset),
                          digits=2)


#Take a subset again
interestcols<-c("Season", "Date", "HomeOrAway", "Referee",
                "TeamName", "Opponent", "Points", "Goals",
                "TotalGoals", "GoalRatio")
formed_data <- sub_data[, interestcols]

team_points <-aggregate(sub_data$Points, by=list(
  sub_data$TeamName, sub_data$Opponent), FUN=sum)

team_points_mat <-cast(team_points, Group.1~Group.2, sum)

team_point_ratios <-round((team_points_mat[,-1])/((table(cleaned_data$HomeTeam,cleaned_data$AwayTeam) +table(cleaned_data$AwayTeam,cleaned_data$HomeTeam))*3),4)
aggregate(sub_data$Points, by=list(sub_data$TeamName, sub_data$Opponent), FUN=count)
#Update row names
row.names(team_point_ratios)<-colnames(team_point_ratios)

# Replace all 0s.
team_point_ratios <- replace(team_point_ratios, is.na(team_point_ratios), 0)

heatmap(as.matrix(team_point_ratios))

# Melt the team point raios to get a historical point record.
# A variable that tells use how well this team has done
# against this opponent.
historical_point_record<-melt(cbind(row.names(
  team_point_ratios), team_point_ratios))
colnames(historical_point_record)<-c("TeamName", "Opponent",
                                     "PointRatio")

historical_point_record<- merge(
  historical_point_record, historical_point_record,
  by.x=c("TeamName", "Opponent"), by.y=c("Opponent",
                                         "TeamName"))
colnames(historical_point_record)[3:4]<-c(
  "HistoricalPointRatio","HistoricalOpponentPointRatio")

sub_data<-merge(sub_data, historical_point_record, by=c(
  "TeamName", "Opponent"))

# Get means and sd for goals for given teams to build assumption that it is a random process
means<-tapply(sub_data$Goals, sub_data$TeamName, FUN=mean)
sds<-tapply(sub_data$Goals, sub_data$TeamName, FUN=sd)

goal_frame<-data.frame(cbind(rownames(sds),means,sds))
rownames(goal_frame)<-NULL
colnames(goal_frame)<-c("TeamName","MeanGoals","SDGoals")
goal_frame$MeanGoals<-as.numeric(as.character(
  goal_frame$MeanGoals))
goal_frame$SDGoals<-as.numeric(as.character(
  goal_frame$SDGoals))


table(sub_data$Goals)/sum(sub_data$Goals)
# What distribution are goals drawn from?

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

norm_goals<-cbind(norm_goals,win_rate,draw_rate,lose_rate)
colnames(norm_goals)<-c("TeamName","MeanGoals","SDGoals","Opponent","OpponentMeanGoals","OpponentSDGoals","WinRate","DrawRate","LoseRate")

join(sub_data, goal_frame, by=c("TeamName"))



model <-lm(Points~PointRatio + OpponentPointRatio, data=sub_data)
summary(model)

model <-rpart(Points~PointRatio +OpponentPointRatio, data=sub_data)
summary(model)
plot(model)
## More exploration
numerical <- formed_data[,-c(1,2,3,4,5,6)]
sum_mat<-aggregate(numerical, by=list(formed_data$Season, formed_data$TeamName), FUN=sum)
mean_mat<-aggregate(numerical, by=list(formed_data$Season, formed_data$TeamName), FUN=mean)
sum_mat$GoalRatio<-mean_mat$GoalRatio
sum_mat <- rename(sum_mat, c("Group.1"="Season","Group.2"="TeamName"))
sum_mat<-sum_mat[order(as.character(sum_mat$Season), sum_mat$Points, decreasing=TRUE),]
row.names(sum_mat)<-1:120
sum_mat<-ddply(sum_mat, .(Season), transform, Rank = rank(-Points, ties.method="first"))


p<- ggplot(sum_mat, aes(Season, Points, group=TeamName,
                        colour=TeamName))
p + geom_path(alpha=1)+ labs(title = "Team Points by Season")
ggsave("teamseasonperformance.pdf", width=8, height =8)

consistent<-rownames(table(sum_mat$TeamName)[table(sum_mat$TeamName)==6])
p<- ggplot(sum_mat[sum_mat$TeamName%in%consistent,], aes(Season, Points, group=TeamName,
                        colour=TeamName))
p + geom_path(alpha=1)+ labs(title = "Team Points by Season")
ggsave("teamseasonperformancetopperformers.pdf", width=8, height =8)


p<- ggplot(sum_mat, aes(Season, Rank, group=TeamName,
                        colour=TeamName))
p + geom_path(alpha=1)+ labs(title= "Team Rank by Season")
ggsave("teamseasonrank.pdf", width=8, height=8)

avg_rank <- sort(tapply(sum_mat$Rank,c(sum_mat$TeamName), FUN=mean))
avg_goal_ratio <- sort(tapply(sum_mat$GoalRatio, c(sum_mat$TeamName), FUN=mean), decreasing=TRUE)

# Look at trends over time
cast(sum_mat, TeamName ~Season, mean, value='GoalRatio')

cast(sum_mat, TeamName ~Season, mean, value='Rank')

# Look back to top cleaned data.
summary(lm(cleaned_data$TotalGoals~cleaned_data$Referee))
p <- ggplot(cleaned_data, aes(Referee, TotalGoals))
p + geom_boxplot() + geom_jitter()
ggsave("TotalGoalsByRef.pdf", width=24, height=8)


prop.table(table(cleaned_data$FTR, cleaned_data$HTR),2)

# Examine relationship between home and away
p <- ggplot(cleaned_data, aes(FTHG, FTAG))
p + geom_point()

tapply(cleaned_data$FTHG, cleaned_data$Season, FUN=mean)
tapply(cleaned_data$FTAG, cleaned_data$Season, FUN=mean)
# There is a statistical difference
t.test(cleaned_data$FTHG,cleaned_data$FTAG)

summary(lm(cleaned_data$FTHG~cleaned_data$FTAG))


# Relationship between shots and goals
p <- ggplot(cleaned_data, aes(HS, FTHG))
p + geom_point()+ labs(title = "Home Team Goals as a Function of Shots")
ggsave("TotalGoalsByShotsHome.pdf", width=8, height=8)
summary(lm(cleaned_data$FTHG~cleaned_data$HS ))

# Score drivers
# 2,3 are team names
match_stats<-cleaned_data[,colnames(cleaned_data)[c(2,3,4,5,11:22)]]
cor(match_stats)

summary(lm(HS ~ .,data=match_stats))
# R^2 .56 when we have all this data.

library("rpart")
library("randomForest")
rfm <- randomForest(HS ~ .,data=match_stats)
# What variables are important to the random forest?
rfm$importance



##Back to the top
# What bookies are best
# Win odds

a <- cleaned_data[23:75]

summary(bookie)
na_cols<-colnames(a)[colSums(is.na(a)) > 0]
a<-a[,!(colnames(a)%in%c(na_cols,"Season","HomePoints"))]
rfm <- randomForest(ordered(a$HomePoints) ~ .,data=a)
bookie<-lm(a$HomePoints~.,data=a)

p <- ggplot(cleaned_data, aes(B365H, WHH))
p + geom_point()

p <- ggplot(cleaned_data, aes(VCH, WHH))
p + geom_point()
#Bookie probability matrix
probs<-1/a

pca<-princomp(probs)
pca_frame<-as.data.frame(cbind(cleaned_data$FTR,pca$scores))
colnames(pca_frame)[1]<-"FTR"
pca_frame$FTR<-as.factor(pca_frame$FTR)
levels(pca_frame$FTR)<-c("A","D","H")
#2 components represetn 71% of the variance
p <- ggplot(pca_frame, aes(Comp.1, Comp.2, colour=FTR))
p + geom_point()+ ggtitle("Principle Component Analysis of Bookie Data W.R.T. match result.")
ggsave("PrincompBookie.pdf", width=8, height=8)



library("e1071")
support<-svm(pca_frame$FTR[1:2000]~., data=pca_frame[1:2000,])
predictions = predict(support, newdata=pca_frame[2001:2188,])
prop.table(table(predictions,pca_frame$FTR[2001:2188]),2)

# Team table
match_pairs<-table(cleaned_data$HomeTeam,cleaned_data$AwayTeam)
home_win_pairs<-table(cleaned_data$HomeTeam[cleaned_data$FTR=="H"],cleaned_data$AwayTeam[cleaned_data$FTR=="H"])
away_win_pairs<-table(cleaned_data$HomeTeam[cleaned_data$FTR=="A"],cleaned_data$AwayTeam[cleaned_data$FTR=="A"])
draw_pairs <-table(cleaned_data$HomeTeam[cleaned_data$FTR=="D"],cleaned_data$AwayTeam[cleaned_data$FTR=="D"])
win_pairs <-home_win_pairs + away_win_pairs
win_ratio <-win_pairs/match_pairs

# Should use sub_data from earlier.


