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

options(stringsAsFactors=FALSE)

# Clear R environment.
rm(list=ls())

#This will be used to avoid some division by zero errors later.
ratio_offset<-1e-6

# Set working directory .
setwd("~/UCD/DataMining/HW2/")
# This creates an object called cleaned_data in our environment.
load("CleanedData/cleaned_data.Rda")

# Gets our long form dataset.
get_long_data <- function(){
  # Take a subset of our data
  # Team names, results, refs and dates/seasons
  subset <- cleaned_data[,c(1:6, 10, 71, 75, 76, 77)]
  # Convet dataset to long form from wide form.
  sub_data <- melt(subset, id=c("Date","FTHG","FTAG","FTR",
                                "Referee","Season","HomePoints",
                                "AwayPoints", "TotalGoals"))
  sub_data <- rename(sub_data, c("value"="TeamName"))
  sub_data <- rename(sub_data, c("variable"="HomeOrAway"))
  
  # Calculate the respective points for each team.
  sub_data$Points <- 0
  sub_data$Points[sub_data$HomeOrAway=="AwayTeam"] <-
    sub_data$AwayPoints[sub_data$HomeOrAway=="AwayTeam"]
  sub_data$Points[sub_data$HomeOrAway=="HomeTeam"] <-
    sub_data$HomePoints[sub_data$HomeOrAway=="HomeTeam"]
  
  # Create a factor as it can be easier for prediction.
  sub_data$Result<-as.factor(sub_data$Points)
  levels(sub_data$Result)<-c("L","D","W")
  
  sub_data$Goals <-0
  sub_data$Goals[sub_data$HomeOrAway=="AwayTeam"] <-
    sub_data$FTAG[sub_data$HomeOrAway=="AwayTeam"]
  sub_data$Goals[sub_data$HomeOrAway=="HomeTeam"] <-
    sub_data$FTHG[sub_data$HomeOrAway=="HomeTeam"]
  
  # Opponent
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
  
  # Introduce a calculated metric. goal ratio. Which tells us
  # what proportion of the goals in that match that team scored.
  # To prevent division by zero NaNs. The round later removes this.
  sub_data$GoalRatio<-round((sub_data$Goals + ratio_offset)/
                            (sub_data$TotalGoals + ratio_offset),
                             digits=2)
  return(sub_data)
}

sub_data <- get_long_data()

#Take a peek.
head(sub_data)

# I will use this more in modelling stage
get_streak<-function(n, long_form_data){
  output<- data.frame()
  teams <- levels(sub_data$TeamName)
  for(i in 1:length(teams)){
    team <- teams[i]
    set <- long_form_data[long_form_data$TeamName==team,]
    for(val in n:nrow(set)){
      date <- set$Date[val]
      row <- c(team, date, sum(set$Points[(val-n):val])/(3*n))
      output <- rbind(output, row)
    }
  }
  colnames(output)<-c("TeamName","Date","StreakPoints")
  return(output)
}



calculate_team_perf_pairs <- function(max_date){
  sub_data <- sub_data[sub_data$Date<max_date, ]
  cleaned_data <- cleaned_data[cleaned_data$Date<max_date, ]
  team_points <-aggregate(
    sub_data$Points, by=list(sub_data$TeamName, sub_data$Opponent),
    FUN=sum)
  
  # Make a function to get the team point ratios for a given
  # Team pair.
  team_points_mat <-cast(team_points, Group.1~Group.2, sum)
  
  team_point_ratios <-round((team_points_mat[,-1])/
                              ((table(cleaned_data$HomeTeam,
                                      cleaned_data$AwayTeam) +
                                  table(cleaned_data$AwayTeam,
                                        cleaned_data$HomeTeam))*3),4)
  # aggregate(sub_data$Points, by=list(sub_data$TeamName, sub_data$Opponent), FUN=count)
  #Replace/update row names
  row.names(team_point_ratios)<-colnames(team_point_ratios)
  
  # Replace all  NAs with 0s.
  team_point_ratios <- replace(team_point_ratios,
                               is.na(team_point_ratios), 0)
  
  # Explore what this looks like.
  heatmap(as.matrix(team_point_ratios))
  
  # Melt the team point raios to get a historical point record.
  # A variable that tells use how well this team has done
  # against this opponent.
  historical_point_record<-melt(cbind(row.names(
    team_point_ratios), team_point_ratios))
  
  colnames(historical_point_record) <- c("TeamName", "Opponent",
                                         "PointRatio")
  
  historical_point_record<- merge(
    historical_point_record, historical_point_record,
    by.x=c("TeamName", "Opponent"), by.y=c("Opponent",
                                           "TeamName"))
  colnames(historical_point_record)[3:4]<-c(
    "HistoricalPointRatio","HistoricalOpponentPointRatio")
  
  
  return(historical_point_record)
}

historical_point_record <- calculate_team_perf_pairs("2017-01-01")


sub_data<-merge(sub_data, historical_point_record, by=c(
  "TeamName", "Opponent"))

# Get means and sd for goals for given teams to build assumption that
# it is a random process
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

norm_goals<-cbind(norm_goals,
                  win_rate,
                  draw_rate,
                  lose_rate)
colnames(norm_goals) <- c("TeamName", "MeanGoals", "SDGoals",
                          "Opponent", "OpponentMeanGoals",
                          "OpponentSDGoals", "WinRate",
                          "DrawRate","LoseRate")

sub_data<-join(sub_data, norm_goals, by=c("TeamName",
                                          "Opponent"))

#Take another subset
interestcols<-c("Season", "Date", "HomeOrAway", "Referee",
                "TeamName", "Opponent", "Points", "Goals",
                "TotalGoals", "GoalRatio")
formed_data <- sub_data[, interestcols]


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
p + geom_path(alpha=1)+ labs(title = "Team Points by Season") +
  guides(colour=guide_legend(ncol=2))


p <- ggplot(sum_mat, aes(Season, Rank, group=TeamName,
                        colour=TeamName))
p + geom_path(alpha=1)+ labs(title= "Team Rank by Season") +
  guides(colour=guide_legend(ncol=2))
  

#avg_rank <- sort(tapply(sum_mat$Rank,c(sum_mat$TeamName), FUN=mean))
#avg_goal_ratio <- sort(tapply(sum_mat$GoalRatio, c(sum_mat$TeamName), FUN=mean), decreasing=TRUE)

# Look at trends over time
#cast(sum_mat, TeamName ~Season, mean, value='GoalRatio')

rank_by_season <- cast(sum_mat, TeamName ~Season, mean, value='Rank')
rank_by_season

# Are there differences between referees?

tapply(cleaned_data$TotalGoals,cleaned_data$Referee, FUN=mean)

p <- ggplot(cle
            aned_data, aes(Referee, TotalGoals))
p + geom_boxplot() + geom_jitter()

# Relationship betwen half time result and full time result.
prop.table(table(cleaned_data$FTR, cleaned_data$HTR),2)

# Examine relationship between home and away
win_odds<-prop.table(table(cleaned_data$FTR))
win_odds

round(win_odds/min(win_odds),2)


summary(lm(cleaned_data$FTHG~cleaned_data$FTAG))
p <- ggplot(cleaned_data, aes(FTAG, FTHG))
p + geom_point() + geom_smooth(method = "lm", se = FALSE)

tapply(cleaned_data$FTHG, cleaned_data$Season, FUN=mean)
tapply(cleaned_data$FTAG, cleaned_data$Season, FUN=mean)
# There is a statistical difference
t.test(cleaned_data$FTHG, cleaned_data$FTAG)


# Relationship between shots and goals
p <- ggplot(cleaned_data, aes(HS, FTHG))
p + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Home Team Goals as a Function of Shots")
#ggsave("TotalGoalsByShotsHome.pdf", width=8, height=8)
summary(lm(cleaned_data$FTHG~cleaned_data$HS ))

# Score drivers
# 2,3 are team names
match_stats<-cleaned_data[,colnames(cleaned_data)[c(2,3,4,5,11:22)]]
cor(match_stats[,-c(1,2)])

summary(lm(FTHG ~ ., data=match_stats))
# R^2 2723 when we have all this data.

library("rpart")
library("randomForest")
rfm <- randomForest(FTHG ~ .,data=match_stats)
# What variables are important to the random forest?
rfm$importance



##Back to the top
# What bookies are best
# Win odds

a <- cleaned_data[c(6,23:77)]

na_cols<-colnames(a)[colSums(is.na(a)) > 0]
a<-a[,!(colnames(a)%in%c(na_cols,"Season", "AwayPoints",
                         "TotalGoals", "FTR"))]
rfm <- randomForest(ordered(a$FTR) ~ .,data=a)
rfm$importance

bookie<-lm(a$HomePoints~.,data=a)

summary(bookie)


p <- ggplot(cleaned_data, aes(B365H, WHH))
p + geom_point() + geom_smooth(method = "lm", se = FALSE)

p <- ggplot(cleaned_data, aes(VCH, WHH))
p + geom_point() + geom_smooth(method = "lm", se = FALSE)
#Bookie probability matrix
probs<-1/(a+ ratio_offset)

pca<-princomp(probs)
pca_frame<-as.data.frame(cbind(cleaned_data$FTR, pca$scores))
colnames(pca_frame)[1]<-"FTR"
pca_frame$FTR<-as.factor(pca_frame$FTR)
levels(pca_frame$FTR)<-c("A","D","H")
#2 components represetn 71% of the variance
p <- ggplot(pca_frame, aes(Comp.1, Comp.2, colour=FTR))
p + geom_point()+ ggtitle("Principle Component Analysis of Bookie Data W.R.T. match result.")
