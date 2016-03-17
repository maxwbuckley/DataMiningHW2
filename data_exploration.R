# Libraries.
library("ggplot2")
library("reshape")
library("reshape2")
library("plyr")
library("rpart")
library("arules")
# The party package provides nonparametric regression trees
# for nominal, ordinal, numeric, censored, and multivariate
# responses.
library("party")



# Clear R environment.
rm(list=ls())

# Set working directory .
setwd("~/UCD/DataMining/HW2/")
load("CleanedData/cleaned_data.Rda")

#Calculate points
HomePoints<-rep(1, nrow(cleaned_data))
HomePoints[cleaned_data$FTR=="H"]<-3
HomePoints[cleaned_data$FTR=="A"]<-0
cleaned_data$HomePoints<-HomePoints
AwayPoints<-rep(1,nrow(cleaned_data))
AwayPoints[cleaned_data$FTR=="H"]<-0
AwayPoints[cleaned_data$FTR=="A"]<-3
cleaned_data$AwayPoints<-AwayPoints
cleaned_data$TotalGoals<-cleaned_data$FTHG+cleaned_data$FTAG

subset <- cleaned_data[,c(1:6,10,71,75,76)]
sub_data <- melt(subset, id=c("Date","FTHG","FTAG","FTR",
                              "Referee","Season","HomePoints",
                              "AwayPoints"))
sub_data <- rename(sub_data, c("value"="TeamName"))
sub_data <- rename(sub_data, c("variable"="HomeOrAway"))

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

team_points <-aggregate(sub_data$Points, by=list(
  sub_data$TeamName, sub_data$Opponent), FUN=sum)

team_points_mat <-cast(team_points, Group.1~Group.2, sum)

team_point_ratios <-round((team_points_mat[,-1])/((table(cleaned_data$HomeTeam,cleaned_data$AwayTeam) +table(cleaned_data$AwayTeam,cleaned_data$HomeTeam))*3),4)
aggregate(sub_data$Points, by=list(sub_data$TeamName, sub_data$Opponent), FUN=count)
#Update row names
row.names(team_point_ratios)<-colnames(team_point_ratios)

# Replace all 0s.
team_point_ratios<-replace(team_point_ratios, is.na(team_point_ratios), 0)

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

# Can maybe remove this as I put it in cleaned data above.
sub_data$TotalGoals<-sub_data$FTHG+sub_data$FTAG
# To prevent division by zero NaNs
ratio_offset<-1e-6
sub_data$GoalRatio<-round((sub_data$Goals+ratio_offset)/(sub_data$TotalGoals+ratio_offset), digits=2)


interestcols<-c("Season", "Date", "HomeOrAway", "Referee", "TeamName", "Opponent", "Points", "Goals", "TotalGoals", "GoalRatio")
formed_data <- sub_data[, interestcols]
# Make new columns to attempt logistic regression
formed_data$W<-0
formed_data$L<-0
formed_data$D<-0
formed_data$W[formed_data$Points==3]<-1
formed_data$L[formed_data$Points==0]<-1
formed_data$D[formed_data$Points==1]<-1


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








numerical <- formed_data[,-c(1,2,3,4)]
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


