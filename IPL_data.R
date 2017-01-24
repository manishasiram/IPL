##1
library(ggplot2)
read.csv("matches.csv")#it reads the files in directory
data=read.csv("matches.csv")#it assigns values for data
data#it gives the values in data
qplot(data$season,data$city,data)#it plots the data b/w city and season
data1=read.csv("deliveries.csv")#it reads and assigns the files in directory

##2
library(plotrix)
out=c("other","bowled","caught","caught and bowled","hit wicket","lbw","obstructing the field","retired hurt","run out","stumped")
count1=subset(data1,data1$dismissal_kind!="")#we ommit the null values
count2=table(count1$dismissal_kind)#it caliculates each kind of dismissal values
pie3D(count2,labels=out,labelcex=0,radius=0.5,height=0.1,
      main="Pie chart for dissimisal rate", col = rainbow(10)) #it plots 3d pie
legend("topright",cex = 0.8,bty = "n",pch = 9,
       c("other","bowled","caught","caught and bowled","hit wicket","lbw",
    "obstructing the field","retired hurt","run out","stumped"), fill = rainbow(10))

##3
runs=aggregate(batsman_runs~match_id,data=data1,sum)#assign the values
runs#it displays
df=data.frame(data)#we assign values
names(df)#to display
df$batsman_runs=runs$batsman_runs#we assign values
names(df)#to display
s1=subset(df,df$season==2008)#we take the subset values
s1
s2=subset(df,df$season==2010)#we take the subset values

s3=subset(df,df$season==2012)#we take the subset values
s4=subset(df,df$season==2014)#we take the subset values
par(mfrow=c(2,2))#it divides the page into 4 
d <- density(s1$batsman_runs)#we assign values
plot(d, main="Density plot for runs in 2008")#we plot the density graph
polygon(d, col="red") #it fills the graph with color 
d <- density(s2$batsman_runs)#we assign values
plot(d, main="Density plot for runs in 2010")#we plot the density graph
polygon(d, col="red") #it fills the graph with color 
d <- density(s3$batsman_runs)#we assign values
plot(d, main="Density plot for runs in 2012")#we plot the density graph
polygon(d, col="red") 
d <- density(s4$batsman_runs)
plot(d, main="Density plot for runs in 2014")
polygon(d, col="red") #it fills the graph with color 

##4
team1 <- aggregate(total_runs~match_id,data=data1,sum)#we assign values
team1
df<-data.frame(data)#we assign values
names(df)# it displays
df$total_runs = team1$total_runs#we assign values
df
write.csv(team$total_runs,"matches.csv")#it writes permenantly
x=df$season#we assign values
y=df$total_runs#we assign values
with(df,plot(total_runs,win_by_runs,col="blue",main="Linear-Regression for total_runs and win_by_runs",
             abline(lm(df$win_by_runs~df$total_runs)),cex=1.3,pch=16))#it gives linear regression

##5
c=subset(data1,data1$batsman=="V Kohli")#we assign values
c
barplot(table(c$bowler),main = "VIRAT KOHLI performances against all bowlers")#it gives barplot

##6
with(data,qplot(season,winner,
          data,colour=toss_winner,main="Teams that win both toss and the match"))#it gives the scatter plot 

##7
with(data,qplot(season,winner,data,color=player_of_match))#it gives the scatter plot

##8
b<- data$winner#we assign values
w1 <- length(which(b=="Kolkata Knight Riders"))#we assign values
w1
w2 <- length(which(b=="Chennai Super Kings"))#we assign values
w3 <- length(which(b=="Delhi Daredevils"))#we assign values
w4 <- length(which(b=="Royal Challengers Bangalore"))#we assign values
w5 <- length(which(b=="Rajasthan Royals"))#we assign values
w6 <- length(which(b=="Kings XI Punjab"))#we assign values
w7 <- length(which(b=="Deccan Chargers"))#we assign values
w8 <- length(which(b=="Mumbai Indians"))#we assign values
w9 <- length(which(b=="Sunrisers Hyderabad"))#we assign values
w10<- length(which(b=="Rising Pune Supergiants"))#we assign values
w11<- length(which(b=="Gujarat Lions"))#we assign values
w12<- length(which(b=="Pune Warriors"))#we assign values


w13 <- length(which(b=="Kochi Tuskers Kerala")) #we assign values
w <-c(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10)#we assign values
print(w)#it displays
t <-c("Kolkata Knight Riders","Chennai Super Kings","Delhi Daredevils","Royal Challengers Bangalore","Rajasthan Royals","Kings XI Punjab","Deccan Chargers","Mumbai Indians","Sunrisers Hyderabad","Rising Pune Supergiants","Gujarat Lions","Pune Warriors","Kochi Tuskers Kerala")#we assign values
pie(w,labels=t,main = "Pie chart for each team winning rate",col = rainbow(length(w)))#it gives pie chart 
