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
