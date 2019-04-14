####Libraries####

Install.packages("scales")
Install.packages("KableExtra")
Install.pacakges("dplyr")
Install.packages("ggplot2")
Install.packages("reshape2")
install.packages("Knitr")
install.packages("matrixStats")
install.packages("yarr")
library(scales)
library(kableExtra)
library(dplyr)
library(knitr)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(yarr)


####Data Importing Process####
## set working directory##
setwd("D:/Harrisburg_Active Folder/ANLY 506- Exploratory Analysis/Code Library-Practice")
## Data Import Command##
all_data<-read.csv("ipeds2016.csv",Header=TRUE)

#####Data Wrangling and Data Analysis ####

## Filter the data set to extract 5 states##
data1<-filter(all_data, STABBR %in% c("CA", "MA", "NY", "NC", "WA"))
# Get the count of universities by state
sum1<-data1 %>%
  group_by(STABBR) %>%
  summarise(Number_of_Universities = n())

# Assign descriptions to the Sector classification
data1$SECTOR.f<-factor(data1$SECTOR, labels = c(
  "Public, >=4 yrs",
  "Non-Profit, >=4 yrs",
  "Profit, >=4 yrs",
  "Non-Profit, 2 yrs",
  "Profit, 2 yrs"
))

# Assign descriptions to the Degree classification
data1$HLOFFER.f<-factor(data1$HLOFFER, labels = c(
  "Associate's Degree",
  "Postsecondary Award",
  "Bachelor's Degree",
  "Postbaccalaureate",
  "Master's Degree",
  "Post-master's Degree",
  "Doctoral Degree"
))

# Count of universities by State and Sector
sum2<-data1 %>%
  group_by(STABBR, SECTOR.f) %>%
  summarise(Number_of_Universities = n())



sum3<-data1 %>%
  group_by(STABBR, SECTOR.f) %>%
  summarise(Average_Salaries = mean(SAAVMNT))             

sum4<-filter(data1, HLOFFER %in% c(5,6,7,8,9))%>%
  group_by(STABBR, HLOFFER.f) %>%
  summarise(Number_of_Universities = n(),
            Average_Salaries = mean(SAAVMNT))  



#### Data Visualization####

##Analysis of Salaries##
ggplot(sum4, aes(x=Number_of_Universities, y=Average_Salaries, group=STABBR, fill=HLOFFER.f))+
  geom_point(aes(shape=STABBR, color=HLOFFER.f, size=2))+
  ggtitle("Chart 3: Average salary vs University count")+
  xlab("Uniersity Count")+ylab("Average Salary")+
  labs(fill="Level of Degree", color = "Level of Degree")+
  labs(shape="State")+
  annotate("segment", x=1, y=14700, xend=52, yend=13000, color="blue")+
  annotate("text", x =10, y=15500, label="CA has higher salary for Undegrad than Grad degree")+
  annotate("rect", xmin=11, xmax=18, ymin=10000,ymax=11800, alpha=0.2)+
  annotate("segment", x=18,y=11800,xend=25,yend=14500,color="black")+
 
 annotate("text", x=15, y=9800, label = "MA has on average similar salaries but higher on Doctoral")

##Number of Universities by state and county##
ggplot(sum2, aes(x=STABBR, y=Number_of_Universities, fill=SECTOR.f))+
  geom_bar(aes(fill=SECTOR.f), position = "dodge", stat="identity")+
  ggtitle("Chart 1: Number of universities by state and category")+
  xlab("State Name")+ylab("Number of Universities")+
  geom_text(aes(label = sum2$Number_of_Universities), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(fill="Sector")+
  annotate("text", x = 4, y = 90, label = "NY has the most Non-profits")
##### miscellaneous data sets practice code#### 
##Data Import##
# Set working Directory#
setwd("D:/Harrisburg_Active Folder/ANLY 506- Exploratory Analysis/Code Library-Practice")
# Data Import#
olerance.pp<-read.csv("C:/bac/HU/ANLY512/data/Longitudinal_alda/tolerance1_pp.csv")
midterm<-read.csv("US EPA data 2017.csv",header=TRUE)
#Data Wrangling #
str(midterm)
glimpse(midterm)
x<-melt(mpg,id.vars  =c("id","drv"),measure.vars =c("cty","hwy"))-- Melting the dataset for preparation
# Data Analysis##
# Weighted median Function#
weighted_median <- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    
    df_omit <- na.omit(data.frame(x, w))
    
    return(weightedMedian(df_omit$x, df_omit$w, ...))
    
  } 
  
  weightedMedian(x, w, ...)
  
}

weighted_median(quiz7[["M_weekly"]],quiz7[["Industry"]],na.rm=TRUE)

##Missing Value in a selected column

glimpse(midterm$X2nd.Max.Value)
sum(is.na(midterm$X2nd.Max.Value))

## mtcars data set practice###
glimpse(mtcars)
#Mean of mpg variable
a<-mean(mtcars[["mpg"]])
#Trimmed mean to get accuarate value
b<-mean(mtcars[["mpg"]], trim=0.1)
#weighted mean 
c<-weighted.mean(mtcars[["mpg"]], w=mtcars[["wt"]])
# Median of mpg varibale
d<-median(mtcars[["mpg"]])
#weighted median
e<-weightedMedian(mtcars[["mpg"]], w=mtcars[["wt"]])
# to find conditional statements
a<c
a<d
a>b
b>d
d>c
c>e

##Standard deviation,interquartile range,median absolute deviation##
f<-sd(mtcars$mpg)
g<-IQR(mtcars$mpg)
h<-mad(mtcars$mpg)
h>f
g>f
f>h
f>g
#Determine which variable is numeric? in mpg##

str(mpg)

is.numeric(mpg$cyl)
is.numeric(mpg$hwy)
is.numeric(mpg$displ)
is.numeric(mpg$model)
is.numeric(mpg$year)
# tidy verse practice##

glimpse(iris)
pe<-filter(iris,Petal.Length>4.5) 
glimpse(pe)

pe <- iris %>%
  group_by(Species) %>%
  filter(Petal.Length>4.5)
mean(pe$Sepal.Length) 
sd(pe$Sepal.Width) 
max(pe$Petal.Length)
#### Visualization###
# GGplot scatter plot ###

ggplot(mpg,aes(x=displ,y=hwy))+geom_point(shape=24,fill="red",colour="black")
## flights data set;how many flights had their departure in January or June#

#### Data set 3 practice###
# Data Import
library("nycflights13")
glimpse(flights)
jan<-filter(flights,month %in% c(1,6))
 
## Data Analysis and Wrangling##
#how many observations of diamonds have the best color which codified as D in the column color###
library(diamonds)
glimpse(diamonds)
bestD<-filter(diamonds,color=="D")
#Find 25% percentile for the column eruptions. dataset faithful##
glimpse(faithful)
duration = faithful$eruptions 
qf<-quantile(duration, c(.25)) 
round(qf,2)
### data set WHO tidyverse#
install.packages("tidyverse")
library(tidyverse)
glimpse(who)
str(who)
test<-as.data.frame(c(1,2,3),c("raghu","chantu","reddy"))
test
rm(test)
test

#### Visualization###
ggplot(mtcars,aes(x=disp,y=hp))+geom_smooth()+labs(title="hpvsdisp")+xlab("Display")+ylab("HorsePower")


-- These codes were used during analysis practice for 
quiz, class material etc.
#### Social Network Analysis Practice Code####
###PURPOSE###
##The purpose of this project is to analyze the data collected from a survey conducted by a professor 
in 512 class. This survey data talks about how students are connected for doing their assignments, are they 
pursuing careers in the same field, and how far the student is living from where the primary responder live, etc. 
The primary objective is to get students hands-on experience in network graphs visualization topics and gage 
the knowledge on how to design a compelling social networking graph, which became a dominant analytical approach 
for analyzing social networking data in today's world. By completing this assignment, we are confident that
we would be able to solve real-world problems related to social networking data using data visualizations 
techniques##

##METHODLOGY###
igraph is a library collection for creating and manipulating graphs and analyzing networks. It is widely used in academic research in network science and related fields. It can be used for large social networking analysis efficiently. The data set for this assignment consists of 17 variables and 18 observations. We will be analyzing different categories of network connections between the students. We will use the network diagrams to show relationships visually. We will use the histogram to plot the degrees vs. frequency of the student connections. Besides, we will explore various factor and numeric variables using exploratory,
and I graph visualization techniques.

### DATA PROCESSSING###


In this section, we listed a list of libraries we would require for our analysis and preliminary steps that we take for any data visualization assignments when using R studio. Example: setup the directory,  load the data, check the structure and subset the data as needed for visualization purposes.
library(igraph)
library(dplyr)
library(ggplot2)
setwd("D:/Harrisburg_Active Folder/512/Assignment Social Networking")
dataset<-read.csv("SNAspring19 - Form Responses 1.csv",header=TRUE)
str(dataset)

name<-c(dataset[2],dataset[3],dataset[4])
numberoftimes<-data.frame(dataset$What.is.YOUR.name.,dataset$Student.1..Who.have.you.worked.with.,dataset$Student.1..Number.times.you.worked.with.this.student.this.semester..enter.a.number.)
network1<-graph.data.frame(numberoftimes,directed=T)
V(network1)## vertices 
E(network1)##edges 
V(network1)$label<-V(network1)$name
V(network1)$degree<-degree(network1)
V(network1)$label
V(network1)$degree

par(mfrow=c(1,2))
set.seed(222)
## Network Diagram 1 ###

plot(network1,vertex.color="red",vertex.size=10,vertex.label.dist=0.1,edge.arrow.size=0.5,vertex.label.cex=1,main="WhoWorkedwithWho",layout=layout.fruchterman.reingold)

##Network Diagram2 ###

plot(network1,vertex.color=rainbow(52),vertex.size=V(network1)$degree*8,edge.arrow.size=1,layout=layout.fruchterman.reingold,main="WhoWorkedwithWho2")

## Figure 1 and 2 Descriptions###
Figure 1 and two talks relatively the same; however, it has differences in the visuals. We will talk about the differences in this section outlining the above graphs; Graph 1 is talking about who worked with who during their semester, and graph two talks about the same, but the vertices in graph two were derived from the degree of the two character variables. Besides, we tried a different color and basic format. Though there is no much to talk about these graphs since they are straightforward, however, the critical difference that should be observed in the graphs is how the data is being influencing the vertex, the more people have worked with the person, the more the size of the vertex. Also, we can observe how there are some outliers where people worked by themselves without having an assignment partner; in these cases, the arrows are pointing to themselves representing the same. That is another exciting learning from this exercise.


##Figure 3###
Networkgraph3<-data.frame(dataset$What.is.YOUR.name.,dataset$Student.1..Number.times.you.worked.with.this.student.this.semester..enter.a.number.,dataset$Student.1..Is.this.student.male.or.female.)
network3<-graph.data.frame(Networkgraph3,directed=T)
V(network3)$degree<-degree(network3)
V(network3)$degree
plot(network3, vertex.color=rainbow(52),vertex.size=V(network3)$degree*5,vertex.label.dist=0.1,edge.arrow.size=0.5,vertex.label.cex=1,layout=layout.fruchterman.reingold,main="How Many Times Student Worked with his partner")

###Figure 4###
network3dense<-graph.data.frame(Networkgraph3,directed=F)
clusternest<-cluster_edge_betweenness(network3dense)
plot(clusternest,network3dense,main="ClusterAnalysis-How many students have chosen same number of times worked with his partner",vertex.color=rainbow(52),layout=layout.fruchterman.reingold )

##Description###

Figure 3 and 4 are an extension to the graph 1, it is talking about how many times each student had worked with his partner through out the semester. Also, it shows the clusters of numbers that student have worked with his parter. 
the larger the size of the edge, the most of the students have choosen that number.



## Histogram###
hist(V(network1)$degree,col="red",main="Histogram of Degree",Xlab="Degree of Vertices",Ylab="Frequency")

We have conducted more anlayis by taking the degree and plot the values on the histomgram. It seems like the degree of the connections fallen between 1 and 3 as shown in the figure. By looking at the 
graph it is clear most of the students has opted betwen 1 and 2.0 so we should be considered that has mode for this analysis by using the histogram.



###Results and Conclusion:###

After conducting the minimal analysis using the survey dataset, We admit that it was a great practice exercise where we had an opportunity to use Igraph package and analyze how students are connected in the class and how reliable are the connections in terms of collaboration in the assignments and sharing similar academic and non-academic ideas throughout the semester. The critical observations are, it seems only two students had more connections compare to others and most of the connections have not interacted more than once throughout the semester. We conclude this research with excellent hands-on experience using Igraph package and will continue our research using more variables in the data set or any relevant dataset that could be found via the internet, which could potentially help us to attain knowledge in Igraph/network analysis.

##### Maps Practice Code ####


library(rgdal)
library(leaflet)
library(dplyr)
library(mapview)
library(anchors)

# Set Working Directory
setwd("D:/Harrisburg_Active Folder/ANLY 506- Exploratory Analysis/Code Library-Practice")

# Load the districts Shapefile
districts<-readOGR(dsn = "D:/Harrisburg_Active Folder/ANLY 506- Exploratory Analysis/Pennsylvania_School_Districts",layer="cb_2016_42_unsd_500k")

# Load the counties shapefile
counties<-readOGR(dsn = "D:/Harrisburg_Active Folder/ANLY 506- Exploratory Analysis",layer="cb_2017_us_county_500k")

#Limit it to Penn state
districts<-districts["STATEFP"=="42"]
counties<-subset(counties,STATEFP=="42")

#Read the school data
easternschools<-read.csv("EasternSchools.csv",header=TRUE)

#Defaultvalue for number of students in a grade is -2, replaced with 0 to indicate there are no students in that grade
replace.value(easternschools, colnames(easternschools), from = -2, to = as.integer(0))

#Create new variables for % White and % Non-White
easternschools$nonwhite<-(easternschools$MEMBER-easternschools$raceWHITE)/easternschools$MEMBER
easternschools$white<- 1- easternschools$nonwhite

### Introduction: Initial Summary

The data deals with public schools in Pennsylvalnia. The questions dealt with this analysis are those of race,
income levels (school lunches), level of schools and school attendance. Initial setup of the data involves 
loading the counties and district data, creating new variables for % white and % nonwhite and to replace all 
default values for population in a particular grade KG, PKG, G01-G13 from -2 to 0[^5].


### Question 1:
What is the distribution of White vs Non-White population across the state?
#1 - White vs Non-White %
nonwhiteperc = leaflet() %>% addTiles() %>%
  addCircleMarkers(data = easternschools, radius = ~nonwhite*10, color = "red", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "red", fillOpacity = 0.2, label = ~city) %>%
  addLegend("topright", colors = c("red"), labels = c("% Non-White"), title = "Races", opacity = 1) %>%
  addRectangles(
    lng1 = -74.7,
    lat1 = 39.7,
    lng2 = -75.7,
    lat2 = 40.3,
    fillColor = "transparent",
    weight = 2.5,
    color = 'green'
  )


whiteperc = leaflet() %>% addTiles() %>%
  addCircleMarkers(data = easternschools, radius = ~white*10, color = "green", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "green", fillOpacity = 0.2, label = ~city) %>%
  addLegend("topright", colors = c("green"), labels = c("% White"), title = "Races", opacity = 1)


latticeView(nonwhiteperc,whiteperc)
```
**Figure 1: Distribution of White vs Non-White population across the geography**
  
  Leaflet is used for the above plot[^1]. The idea behind developing the above plot is to identify where the white and non-white school population is distributed acros the geography. Although both plots are essentially of the same data points with one highlighting % nonwhites and the other identifying % whites, the idea behind showing both is to indicate the stark contrast in distribution. Non-white populations cluster together with a particularly large cluster around Philadelphia as highlighted. White population on the other hand is dispersed widely across the geography.

###Question 2:
What is the distribution of levels of schooling across the state?
  
  ```{r warning=FALSE, message=FALSE}
#2 - School Level
elementary = easternschools %>% filter(LEVEL==1)
middle = easternschools %>% filter(LEVEL==2)
high = easternschools %>% filter(LEVEL==3)
combined = easternschools %>% filter(LEVEL==4)

schoollevel = leaflet(counties) %>% addTiles() %>%
  setView(lat=40.2, lng=-75.5,zoom=8.5)%>%
  addPolygons(weight=1,color="blue",fillOpacity = 0.05) %>%
  addCircleMarkers(data = elementary, radius = 4, color = "magenta", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "magenta", fillOpacity = 1, label = ~city) %>%
  addCircleMarkers(data = middle, radius = 4, color = "cyan", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "cyan", fillOpacity = 1, label = ~city) %>%
  addCircleMarkers(data = high, radius = 4, color = "green", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "green", fillOpacity = 1, label = ~city) %>%
  addLegend("topright", colors = c("magenta", "cyan", "green"), labels = c("Elementary School", "Middle School", "High School"), title = "School Level", opacity = 1)

schoollevel

```
**Figure 2: Distribution of levels of schools across the state**
  
  
  A plot of the school level distribution shows that there is heavy concentration of all levels of schooling around urban centers like phildelphia. The plot has been zoomed in to focus on a particular area using setview[^4]. While the rest of the state has plenty of elementary schools and to a certain extent middle schools, high schools seem to be far and dispersed in non-urban centers, which would imply that children would have to travel a greart distance to attend high school. 

###Question 3:



```{r warning=FALSE, message=FALSE}
#3 - school lunches
easternschools$PctFreeLunch<-as.numeric(as.character(easternschools$PctFreeLunch))
easternschools$freelunch <- ifelse(easternschools$PctFreeLunch >= 30,1,0)
freelunchesnonwhite = easternschools %>% filter(nonwhite>0.5, freelunch==1)
freeluncheswhite = easternschools %>% filter(white>0.5, freelunch==1)
schoolluncheswhite1 = leaflet(districts) %>% addTiles() %>%
  #  setView(lat=40.2, lng=-75.5,zoom=8.5)%>%
  #  addPolygons(weight=1,color="blue",fillOpacity = 0.05) %>%
  addCircleMarkers(data = freeluncheswhite, radius = 4, color = "red", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "red", fillOpacity = 0.2, label = ~city) %>%
  addLegend("topright", colors = c("red"), labels = c("Free Lunch %>30% in mostly White schools"), title = "Free Lunches", opacity = 1)

schoollunchesnonwhite1 = leaflet(districts) %>% addTiles() %>%
  #  setView(lat=40.2, lng=-75.5,zoom=8.5)%>%
  #  addPolygons(weight=1,color="blue",fillOpacity = 0.05) %>%
  addCircleMarkers(data = freelunchesnonwhite, radius = 4, color = "green", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "green", fillOpacity = 0.2, label = ~city) %>%
  addLegend("topright", colors = c("green"), labels = c("Free Lunch %>30% in mostly nonWhite schools"), title = "Free Lunches", opacity = 1)
latticeView(schoolluncheswhite1,schoollunchesnonwhite1)
```
**Figure 3: Distribution of free lunch recepients by race**
  
  The graph shows the schools which have more than 30% of their enrollees being free lunch recepients. The graphs are based on classifying the data into mostly white (>50% white) and mostly non-white (>50% non-white). The data is filtered using ifelse condition[^2]. What is interesting about the two plots is that while the population of non-white schools with high levels of free lunch recepients are few and concentrated in pockets, among white schools, the incidence is very high and spread across the state. 


###Question 4

```{r warning=FALSE, message=FALSE}
#4 - School Attendance

nonwhiteschools = easternschools %>% filter(nonwhite>0.5)
whiteschools = easternschools %>% filter(white>0.5)
attendance = leaflet(districts) %>% addTiles() %>%
  setView(lat=40.2, lng=-75.5,zoom=8.5)%>%
  #    addPolygons(weight=1,color="blue",fillOpacity = 0.05) %>%
  addCircleMarkers(data = nonwhiteschools, radius = ~(100-AttendRate), color = "red", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "red", fillOpacity = 0.2, label = ~city) %>%
  addCircleMarkers(data = whiteschools, radius = ~(100-AttendRate), color = "blue", weight = 0.75, opacity = 1, fill = TRUE, fillColor = "blue", fillOpacity = 0.2, label = ~city) %>%
  addLegend("topright", colors = c("red", "blue"), labels = c("Mostly Non-White", "Mostly White"), title = "Absence Rate", opacity = 1)
attendance
```
**Figure 4: Absence rate among schools by race**
  
  The plot shows the absence rate (1-Attendance Rate) of schools split into two groups - schools that are mostly white (>50%) vs mostly non-white. From the plot, we can see that the absence rate among mostly nonwhite schools is much higher than mostly white schools. 
