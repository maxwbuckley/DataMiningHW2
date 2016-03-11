# Libraries.
library("ggplot2")
library("reshape")
library("reshape2")
library("plyr")


# Clear R environment.
rm(list=ls())

# Set working directory .
setwd("~/UCD/DataMining/HW2/")
load("CleanedData/cleaned_data.Rda")

#Calculate points
HomePoints<-rep(1,nrow(cleaned_data))
HomePoints[cleaned_data$FTR=="H"]<-3
HomePoints[cleaned_data$FTR=="A"]<-0
cleaned_data$HomePoints<-HomePoints
AwayPoints<-rep(1,nrow(cleaned_data))
AwayPoints[cleaned_data$FTR=="H"]<-0
AwayPoints[cleaned_data$FTR=="A"]<-3
cleaned_data$AwayPoints<-AwayPoints
cleaned_data$TotalGoals<-cleaned_data$FTHG+cleaned_data$FTAG



subset<-cleaned_data[,c(1:6,10,71,75,76)]
sub_data <- melt(subset, id=c("Date","FTHG","FTAG","FTR",
                              "Referee","Season","HomePoints",
                              "AwayPoints"))
sub_data <- rename(sub_data, c("value"="TeamName"))

sub_data$Points<-0
sub_data$Points[sub_data$variable=="AwayTeam"]<-sub_data$AwayPoints[sub_data$variable=="AwayTeam"]
sub_data$Points[sub_data$variable=="HomeTeam"]<-sub_data$HomePoints[sub_data$variable=="HomeTeam"]


sub_data$Goals<-0
sub_data$Goals[sub_data$variable=="AwayTeam"]<-sub_data$FTAG[sub_data$variable=="AwayTeam"]
sub_data$Goals[sub_data$variable=="HomeTeam"]<-sub_data$FTHG[sub_data$variable=="HomeTeam"]

# Can maybe remove this as I put it in cleaned data above.
sub_data$TotalGoals<-sub_data$FTHG+sub_data$FTAG
# To prevent division by zero NaNs
ratio_offset<-1e-6
sub_data$GoalRatio<-round((sub_data$Goals+ratio_offset)/(sub_data$TotalGoals+ratio_offset), digits=2)


interestcols<-c("Season", "Date", "Referee", "TeamName", "Points", "Goals", "TotalGoals", "GoalRatio")
formed_data <- sub_data[, interestcols]

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
match_stats<-cleaned_data[,colnames(cleaned_data)[c(4,5,11:22)]]
cor(match_stats)


