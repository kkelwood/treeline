#Spatial Statistics on area 1 (rofmerly Q7)

#first load in spat stats
library(spatstat)

#then define the polygon of the study area --- vertices counterclockwise
P <- owin(poly = list(x = c(492072.58527, 492031.45145, 492063.59083, 492104.72465), y = c(4301354.09373, 4301319.57835, 4301281.27613 , 4301315.79150)))

#plot the polygon tto make sure that it actually works
plot(P) 

#load in the full datafile for A1. It is important to use read.csv because it delineates the appropriate columns. to get the correct number of decimal places. read.csv was truncating it down to one decimal place!
A1_all_csv<-read.csv(file=file.choose(), header=TRUE)

#make the file into a ppp object, remembering the format A1<-ppp(x, y, window = W)   and remembering that $ selects a column from a matrix by its name 
A1_all<- ppp(A1_all_csv$X, A1_all_csv$Y, window=P)

#plot it to make sure that it looks good 
plot(A1_all)

#try to look for summary of the data --- I have spent an hour trying to get more decimal places into the output
summary(A1_all)

#density map --- try to see the scale of aggreagation that makes most sense. I like 1.5-2m the best
plot(density(A1_all))
plot(density(A1_all,2))
plot(density(A1_all,1)) #too fragmented
plot(density(A1_all,5)) #too fuzzy

#can also plot density contour map
contour(density(A1_all,2))

#can easily run ripley's K function
plot(envelope(A1_all, Kest))

#"Now we can select trees based on their germination date --- highlight it all and do this in one go"
A1_bf1800<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1799)
A1_bf1825<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1824)
A1_bf1850<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1849)
A1_bf1875<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1874)
A1_bf1900<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1899)
A1_bf1925<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1924)
A1_bf1950<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1949)
A1_bf1975<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1974)
A1_bf2000<-subset(A1_all_csv,A1_all_csv$Germ_Year<=1999)

#"Now make them all into ppp objects--- highlight them all and say run"
A.1_bf1800<- ppp(A1_bf1800$X, A1_bf1800$Y, window=P)
A.1_bf1825<- ppp(A1_bf1825$X, A1_bf1825$Y, window=P)
A.1_bf1850<- ppp(A1_bf1850$X, A1_bf1850$Y, window=P)
A.1_bf1875<- ppp(A1_bf1875$X, A1_bf1875$Y, window=P)
A.1_bf1900<- ppp(A1_bf1900$X, A1_bf1900$Y, window=P)
A.1_bf1925<- ppp(A1_bf1925$X, A1_bf1925$Y, window=P)
A.1_bf1950<- ppp(A1_bf1950$X, A1_bf1950$Y, window=P)
A.1_bf1975<- ppp(A1_bf1975$X, A1_bf1975$Y, window=P)
A.1_bf2000<- ppp(A1_bf2000$X,A1_bf2000$Y, window=P)

#"plot maps for each of the datafiles, ---highlight and run one line at a time" 
plot(A.1_bf1800)
plot(A.1_bf1825)
plot(A.1_bf1850)
plot(A.1_bf1875)
plot(A.1_bf1900)
plot(A.1_bf1925)
plot(A.1_bf1950)
plot(A.1_bf1975)
plot(A.1_bf2000)

#plot the density "heat" maps, again one at a time--- I am still trying to figure out how to specify the color chart so that they are directly comparable
plot(density(A.1_bf1800,2))
plot(density(A.1_bf1825,2))
plot(density(A.1_bf1850,2))
plot(density(A.1_bf1875,2))
plot(density(A.1_bf1900,2))
plot(density(A.1_bf1925,2))
plot(density(A.1_bf1950,2))
plot(density(A.1_bf1975,2))
plot(density(A.1_bf2000,2))

#perform the Ripley's K tests, one at a time
plot(envelope(A.1_bf1800, Kest))
plot(envelope(A.1_bf1825, Kest))
plot(envelope(A.1_bf1850, Kest))
plot(envelope(A.1_bf1875, Kest))
plot(envelope(A.1_bf1900, Kest))
plot(envelope(A.1_bf1925, Kest))
plot(envelope(A.1_bf1950, Kest))
plot(envelope(A.1_bf1975, Kest))
plot(envelope(A.1_bf2000, Kest))

#now looking at cohorts this will be in your own hands. here is how you would go about making the cohort from 1800 to 1825
A1_1800_to_1825<-subset(A1_all_csv,A1_all_csv$Germ_Year>1799&A1_all_csv,A1_all_csv$Germ_Year<=1824)

#now please make the other cohorts and look at their distribution and K tests.

#The overall direction is looking at whether each cohort is clustered or anti clustered to the previous cohort, and to the previously existing trees. --- going back each timelag.

Subsets:

A1_1800_to_1824<-subset(A1_all_csv,A1_all_csv$Germ_Year>1799&A1_all_csv$Germ_Year<=1824)
A1_1825_to_1849<-subset(A1_all_csv,A1_all_csv$Germ_Year>1824&A1_all_csv$Germ_Year<=1849)
A1_1850_to_1874<-subset(A1_all_csv,A1_all_csv$Germ_Year>1849&A1_all_csv$Germ_Year<=1874)
A1_1875_to_1899<-subset(A1_all_csv,A1_all_csv$Germ_Year>1874&A1_all_csv$Germ_Year<=1899)
A1_1900_to_1924<-subset(A1_all_csv,A1_all_csv$Germ_Year>1899&A1_all_csv$Germ_Year<=1924)
A1_1925_to_1949<-subset(A1_all_csv,A1_all_csv$Germ_Year>1924&A1_all_csv$Germ_Year<=1949)
A1_1950_to_1974<-subset(A1_all_csv,A1_all_csv$Germ_Year>1949&A1_all_csv$Germ_Year<=1974)
A1_1975_to_1999<-subset(A1_all_csv,A1_all_csv$Germ_Year>1974&A1_all_csv$Germ_Year<=1999)
A1_2000_to_2013<-subset(A1_all_csv,A1_all_csv$Germ_Year>1999&A1_all_csv$Germ_Year<=2013)

PPP:

A.1_1800_to_1824<- ppp(A1_1800_to_1824$X, A1_1800_to_1824$Y, window=P)
A.1_1825_to_1849<- ppp(A1_1825_to_1849$X, A1_1825_to_1849$Y, window=P)
A.1_1850_to_1874<- ppp(A1_1850_to_1874$X, A1_1850_to_1874$Y, window=P)
A.1_1875_to_1899<- ppp(A1_1875_to_1899$X, A1_1875_to_1899$Y, window=P)
A.1_1900_to_1924<- ppp(A1_1900_to_1924$X, A1_1900_to_1924$Y, window=P)
A.1_1925_to_1949<- ppp(A1_1925_to_1949$X, A1_1925_to_1949$Y, window=P)
A.1_1950_to_1974<- ppp(A1_1950_to_1974$X, A1_1950_to_1974$Y, window=P)
A.1_1975_to_1999<- ppp(A1_1975_to_1999$X, A1_1975_to_1999$Y, window=P)
A.1_2000_to_2013<- ppp(A1_2000_to_2013$X, A1_2000_to_2013$Y, window=P)

Plot (one at a time):

plot(A.1_1800_to_1824)
plot(A.1_1825_to_1849)
plot(A.1_1850_to_1874)
plot(A.1_1875_to_1899)
plot(A.1_1900_to_1924)
plot(A.1_1925_to_1949)
plot(A.1_1950_to_1974)
plot(A.1_1975_to_1999)
plot(A.1_2000_to_2013)

Ripley's K, 99 (one at a time):

plot(envelope(A.1_1800_to_1824, Kest))
plot(envelope(A.1_1825_to_1849, Kest))
plot(envelope(A.1_1850_to_1874, Kest))
plot(envelope(A.1_1875_to_1899, Kest))
plot(envelope(A.1_1900_to_1924, Kest))
plot(envelope(A.1_1925_to_1949, Kest))
plot(envelope(A.1_1950_to_1974, Kest))
plot(envelope(A.1_1975_to_1999, Kest))
plot(envelope(A.1_2000_to_2013, Kest))

-----------------------
A2/Q6:

Choose File:
A2_all_csv<-read.csv(file=file.choose(), header=TRUE)

View File:
A2_all_csv

Set Window Polygon (vertices counterclockwise):
P2 <- owin(poly = list(x = c(492095.73021, 492136.86403, 492104.72465, 492063.59083), y = c(4301242.97391, 4301277.48928, 4301315.79150, 4301281.27613)))

Make PPP:
A2_all<- ppp(A2_all_csv$X, A2_all_csv$Y, window=P2)

Plot:
plot(A2_all)

Summary:
summary(A2_all)

Cumulative Subsets (all at once):
A2_bf1800<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1799)
A2_bf1825<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1824)
A2_bf1850<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1849)
A2_bf1875<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1874)
A2_bf1900<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1899)
A2_bf1925<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1924)
A2_bf1950<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1949)
A2_bf1975<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1974)
A2_bf2000<-subset(A2_all_csv,A2_all_csv$Germ_Year<=1999)

Make PPP Cumulative (all at once):
A.2_bf1800<- ppp(A2_bf1800$X, A2_bf1800$Y, window=P2)
A.2_bf1825<- ppp(A2_bf1825$X, A2_bf1825$Y, window=P2)
A.2_bf1850<- ppp(A2_bf1850$X, A2_bf1850$Y, window=P2)
A.2_bf1875<- ppp(A2_bf1875$X, A2_bf1875$Y, window=P2)
A.2_bf1900<- ppp(A2_bf1900$X, A2_bf1900$Y, window=P2)
A.2_bf1925<- ppp(A2_bf1925$X, A2_bf1925$Y, window=P2)
A.2_bf1950<- ppp(A2_bf1950$X, A2_bf1950$Y, window=P2)
A.2_bf1975<- ppp(A2_bf1975$X, A2_bf1975$Y, window=P2)
A.2_bf2000<- ppp(A2_bf2000$X, A2_bf2000$Y, window=P2)

Plot Cumulative (one at a time):
plot(A.2_bf1800) - NA
plot(A.2_bf1825) - NA
plot(A.2_bf1850) - NA
plot(A.2_bf1875) - NA
plot(A.2_bf1900)
plot(A.2_bf1925)
plot(A.2_bf1950)
plot(A.2_bf1975)
plot(A.2_bf2000)

Kest Cumulative (one at a time):
plot(envelope(A.2_bf1800, Kest)) - NA
plot(envelope(A.2_bf1825, Kest)) - NA
plot(envelope(A.2_bf1850, Kest)) - NA
plot(envelope(A.2_bf1875, Kest)) - NA
plot(envelope(A.2_bf1900, Kest)) - NA
plot(envelope(A.2_bf1925, Kest))
plot(envelope(A.2_bf1950, Kest))
plot(envelope(A.2_bf1975, Kest))
plot(envelope(A.2_bf2000, Kest))

Age Cohort Subsets (all at once):
A2_1800_to_1824<-subset(A2_all_csv,A2_all_csv$Germ_Year>1799&A2_all_csv$Germ_Year<=1824)
A2_1825_to_1849<-subset(A2_all_csv,A2_all_csv$Germ_Year>1824&A2_all_csv$Germ_Year<=1849)
A2_1850_to_1874<-subset(A2_all_csv,A2_all_csv$Germ_Year>1849&A2_all_csv$Germ_Year<=1874)
A2_1875_to_1899<-subset(A2_all_csv,A2_all_csv$Germ_Year>1874&A2_all_csv$Germ_Year<=1899)
A2_1900_to_1924<-subset(A2_all_csv,A2_all_csv$Germ_Year>1899&A2_all_csv$Germ_Year<=1924)
A2_1925_to_1949<-subset(A2_all_csv,A2_all_csv$Germ_Year>1924&A2_all_csv$Germ_Year<=1949)
A2_1950_to_1974<-subset(A2_all_csv,A2_all_csv$Germ_Year>1949&A2_all_csv$Germ_Year<=1974)
A2_1975_to_1999<-subset(A2_all_csv,A2_all_csv$Germ_Year>1974&A2_all_csv$Germ_Year<=1999)
A2_2000_to_2013<-subset(A2_all_csv,A2_all_csv$Germ_Year>1999&A2_all_csv$Germ_Year<=2013)

PPP Age Cohort (all at once):
A.2_1800_to_1824<- ppp(A2_1800_to_1824$X, A2_1800_to_1824$Y, window=P2)
A.2_1825_to_1849<- ppp(A2_1825_to_1849$X, A2_1825_to_1849$Y, window=P2)
A.2_1850_to_1874<- ppp(A2_1850_to_1874$X, A2_1850_to_1874$Y, window=P2)
A.2_1875_to_1899<- ppp(A2_1875_to_1899$X, A2_1875_to_1899$Y, window=P2)
A.2_1900_to_1924<- ppp(A2_1900_to_1924$X, A2_1900_to_1924$Y, window=P2)
A.2_1925_to_1949<- ppp(A2_1925_to_1949$X, A2_1925_to_1949$Y, window=P2)
A.2_1950_to_1974<- ppp(A2_1950_to_1974$X, A2_1950_to_1974$Y, window=P2)
A.2_1975_to_1999<- ppp(A2_1975_to_1999$X, A2_1975_to_1999$Y, window=P2)
A.2_2000_to_2013<- ppp(A2_2000_to_2013$X, A2_2000_to_2013$Y, window=P2)

Plot Age Cohort (one at a time):
plot(A.2_1800_to_1824) - NA
plot(A.2_1825_to_1849) - NA
plot(A.2_1850_to_1874) - NA
plot(A.2_1875_to_1899)
plot(A.2_1900_to_1924)
plot(A.2_1925_to_1949)
plot(A.2_1950_to_1974)
plot(A.2_1975_to_1999)
plot(A.2_2000_to_2013)

Ripley's K, 99, Age Cohort (one at a time):
plot(envelope(A.2_1800_to_1824, Kest)) - NA
plot(envelope(A.2_1825_to_1849, Kest)) - NA
plot(envelope(A.2_1850_to_1874, Kest)) - NA
plot(envelope(A.2_1875_to_1899, Kest)) - NA
plot(envelope(A.2_1900_to_1924, Kest))
plot(envelope(A.2_1925_to_1949, Kest))
plot(envelope(A.2_1950_to_1974, Kest))
plot(envelope(A.2_1975_to_1999, Kest))
plot(envelope(A.2_2000_to_2013, Kest))


Optional: Density Maps (one at a time)

Cumulative
plot(density(A.2_bf1800,2))- NA
plot(density(A.2_bf1825,2))- NA
plot(density(A.2_bf1850,2))- NA
plot(density(A.2_bf1875,2))- NA
plot(density(A.2_bf1900,2))
plot(density(A.2_bf1925,2))
plot(density(A.2_bf1950,2))
plot(density(A.2_bf1975,2))
plot(density(A.2_bf2000,2))

Cohorts
plot(density(A.2_1800_to_1824,2)) - NA
plot(density(A.2_1825_to_1849,2)) - NA
plot(density(A.2_1850_to_1874,2)) - NA
plot(density(A.2_1875_to_1899,2))
plot(density(A.2_1900_to_1924,2))
plot(density(A.2_1925_to_1949,2))
plot(density(A.2_1950_to_1974,2))
plot(density(A.2_1975_to_1999,2))
plot(density(A.2_2000_to_2013,2))


----------------------- 
A3/Q5:

Choose File:
A3_all_csv<-read.csv(file=file.choose(), header=TRUE)

View File:
A3_all_csv

Set Window Polygon (vertices counterclockwise):
P3 <- owin(poly = list(x = c(492127.86959, 492169.00341, 492136.86403, 492095.73021), y = c(4301204.67169, 4301239.18706, 4301277.48928, 4301242.97391)))

View Polygon:
plot(P3)

Make PPP (3 values to change):
A3_all<- ppp(A3_all_csv$X, A3_all_csv$Y, window=P3)

Plot:
plot(A3_all)

Summary:
summary(A3_all)

Cumulative Subsets (all at once) (3 values to change):
A3_bf1800<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1799)
A3_bf1825<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1824)
A3_bf1850<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1849)
A3_bf1875<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1874)
A3_bf1900<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1899)
A3_bf1925<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1924)
A3_bf1950<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1949)
A3_bf1975<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1974)
A3_bf2000<-subset(A3_all_csv,A3_all_csv$Germ_Year<=1999)

Make PPP Cumulative (all at once):
A.3_bf1800<- ppp(A3_bf1800$X, A3_bf1800$Y, window=P3)
A.3_bf1825<- ppp(A3_bf1825$X, A3_bf1825$Y, window=P3)
A.3_bf1850<- ppp(A3_bf1850$X, A3_bf1850$Y, window=P3)
A.3_bf1875<- ppp(A3_bf1875$X, A3_bf1875$Y, window=P3)
A.3_bf1900<- ppp(A3_bf1900$X, A3_bf1900$Y, window=P3)
A.3_bf1925<- ppp(A3_bf1925$X, A3_bf1925$Y, window=P3)
A.3_bf1950<- ppp(A3_bf1950$X, A3_bf1950$Y, window=P3)
A.3_bf1975<- ppp(A3_bf1975$X, A3_bf1975$Y, window=P3)
A.3_bf2000<- ppp(A3_bf2000$X, A3_bf2000$Y, window=P3)

Plot Cumulative (one at a time):
plot(A.3_bf1800) - NA
plot(A.3_bf1825) - NA
plot(A.3_bf1850) - NA
plot(A.3_bf1875) - NA
plot(A.3_bf1900)
plot(A.3_bf1925)
plot(A.3_bf1950)
plot(A.3_bf1975)
plot(A.3_bf2000)

Kest Cumulative (one at a time):
plot(envelope(A.3_bf1800, Kest)) - NA
plot(envelope(A.3_bf1825, Kest)) - NA
plot(envelope(A.3_bf1850, Kest)) - NA
plot(envelope(A.3_bf1875, Kest)) - NA
plot(envelope(A.3_bf1900, Kest))
plot(envelope(A.3_bf1925, Kest))
plot(envelope(A.3_bf1950, Kest))
plot(envelope(A.3_bf1975, Kest))
plot(envelope(A.3_bf2000, Kest))

Age Cohort Subsets (all at once) (4 values to change):
A3_1800_to_1824<-subset(A3_all_csv,A3_all_csv$Germ_Year>1799&A3_all_csv$Germ_Year<=1824) 
A3_1825_to_1849<-subset(A3_all_csv,A3_all_csv$Germ_Year>1824&A3_all_csv$Germ_Year<=1849)
A3_1850_to_1874<-subset(A3_all_csv,A3_all_csv$Germ_Year>1849&A3_all_csv$Germ_Year<=1874)
A3_1875_to_1899<-subset(A3_all_csv,A3_all_csv$Germ_Year>1874&A3_all_csv$Germ_Year<=1899)
A3_1900_to_1924<-subset(A3_all_csv,A3_all_csv$Germ_Year>1899&A3_all_csv$Germ_Year<=1924)
A3_1925_to_1949<-subset(A3_all_csv,A3_all_csv$Germ_Year>1924&A3_all_csv$Germ_Year<=1949)
A3_1950_to_1974<-subset(A3_all_csv,A3_all_csv$Germ_Year>1949&A3_all_csv$Germ_Year<=1974)
A3_1975_to_1999<-subset(A3_all_csv,A3_all_csv$Germ_Year>1974&A3_all_csv$Germ_Year<=1999)
A3_2000_to_2013<-subset(A3_all_csv,A3_all_csv$Germ_Year>1999&A3_all_csv$Germ_Year<=2013)

PPP Age Cohort (all at once) (4 values to change):
A.3_1800_to_1824<- ppp(A3_1800_to_1824$X, A3_1800_to_1824$Y, window=P3) 
A.3_1825_to_1849<- ppp(A3_1825_to_1849$X, A3_1825_to_1849$Y, window=P3) 
A.3_1850_to_1874<- ppp(A3_1850_to_1874$X, A3_1850_to_1874$Y, window=P3) 
A.3_1875_to_1899<- ppp(A3_1875_to_1899$X, A3_1875_to_1899$Y, window=P3) 
A.3_1900_to_1924<- ppp(A3_1900_to_1924$X, A3_1900_to_1924$Y, window=P3)
A.3_1925_to_1949<- ppp(A3_1925_to_1949$X, A3_1925_to_1949$Y, window=P3)
A.3_1950_to_1974<- ppp(A3_1950_to_1974$X, A3_1950_to_1974$Y, window=P3)
A.3_1975_to_1999<- ppp(A3_1975_to_1999$X, A3_1975_to_1999$Y, window=P3)
A.3_2000_to_2013<- ppp(A3_2000_to_2013$X, A3_2000_to_2013$Y, window=P3)

Plot Age Cohort (one at a time) (one value to change):
plot(A.3_1800_to_1824) - NA
plot(A.3_1825_to_1849) - NA
plot(A.3_1850_to_1874) - NA
plot(A.3_1875_to_1899)
plot(A.3_1900_to_1924)
plot(A.3_1925_to_1949)
plot(A.3_1950_to_1974)
plot(A.3_1975_to_1999)
plot(A.3_2000_to_2013)

Ripley's K, 99, Age Cohort (one at a time) (one value to change):
plot(envelope(A.3_1800_to_1824, Kest)) - NA
plot(envelope(A.3_1825_to_1849, Kest)) - NA
plot(envelope(A.3_1850_to_1874, Kest)) - NA
plot(envelope(A.3_1875_to_1899, Kest)) - NA
plot(envelope(A.3_1900_to_1924, Kest))
plot(envelope(A.3_1925_to_1949, Kest))
plot(envelope(A.3_1950_to_1974, Kest))
plot(envelope(A.3_1975_to_1999, Kest))
plot(envelope(A.3_2000_to_2013, Kest))


Density Maps (one at a time) (one value to change, each):

Cumulative
plot(density(A.3_bf1800,2)) - NA
plot(density(A.3_bf1825,2)) - NA
plot(density(A.3_bf1850,2)) - NA
plot(density(A.3_bf1875,2)) - NA
plot(density(A.3_bf1900,2))
plot(density(A.3_bf1925,2))
plot(density(A.3_bf1950,2))
plot(density(A.3_bf1975,2))
plot(density(A.3_bf2000,2))

Cohorts
plot(density(A.3_1800_to_1824,2)) - NA
plot(density(A.3_1825_to_1849,2)) - NA
plot(density(A.3_1850_to_1874,2)) - NA
plot(density(A.3_1875_to_1899,2))
plot(density(A.3_1900_to_1924,2))
plot(density(A.3_1925_to_1949,2))
plot(density(A.3_1950_to_1974,2))
plot(density(A.3_1975_to_1999,2))
plot(density(A.3_2000_to_2013,2))

----------------------- 
A4/Q4:

Choose File:
A4_all_csv<-read.csv(file=file.choose(), header=TRUE)

View File:
A4_all_csv

Minimum Germ_Year:1936

Set Window Polygon (vertices counterclockwise):
P4 <- owin(poly = list(x = c(492160.00897, 492201.14279, 492169.00341, 492127.86959), y = c(4301166.36946, 4301200.88484, 4301239.18706, 4301204.67169)))

View Polygon:
plot(P4)

Make PPP (3 values to change):
A4_all<- ppp(A4_all_csv$X, A4_all_csv$Y, window=P4)

Plot:
plot(A4_all)

Summary:
summary(A4_all)

Cumulative Subsets (all at once) (3 values to change):
A4_bf1800<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1799)
A4_bf1825<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1824)
A4_bf1850<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1849)
A4_bf1875<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1874)
A4_bf1900<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1899)
A4_bf1925<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1924)
A4_bf1950<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1949)
A4_bf1975<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1974)
A4_bf2000<-subset(A4_all_csv,A4_all_csv$Germ_Year<=1999)

Make PPP Cumulative (all at once)(4 values to change):
A.4_bf1800<- ppp(A4_bf1800$X, A4_bf1800$Y, window=P4)
A.4_bf1825<- ppp(A4_bf1825$X, A4_bf1825$Y, window=P4)
A.4_bf1850<- ppp(A4_bf1850$X, A4_bf1850$Y, window=P4)
A.4_bf1875<- ppp(A4_bf1875$X, A4_bf1875$Y, window=P4)
A.4_bf1900<- ppp(A4_bf1900$X, A4_bf1900$Y, window=P4)
A.4_bf1925<- ppp(A4_bf1925$X, A4_bf1925$Y, window=P4)
A.4_bf1950<- ppp(A4_bf1950$X, A4_bf1950$Y, window=P4)
A.4_bf1975<- ppp(A4_bf1975$X, A4_bf1975$Y, window=P4)
A.4_bf2000<- ppp(A4_bf2000$X, A4_bf2000$Y, window=P4)

Plot Cumulative (one at a time):
plot(A.4_bf1800) - NA
plot(A.4_bf1825) - NA
plot(A.4_bf1850) - NA
plot(A.4_bf1875) - NA
plot(A.4_bf1900) - NA
plot(A.4_bf1925) - NA
plot(A.4_bf1950)
plot(A.4_bf1975)
plot(A.4_bf2000)

Kest Cumulative (one at a time):
plot(envelope(A.4_bf1800, Kest)) - NA
plot(envelope(A.4_bf1825, Kest)) - NA
plot(envelope(A.4_bf1850, Kest)) - NA
plot(envelope(A.4_bf1875, Kest)) - NA
plot(envelope(A.4_bf1900, Kest)) - NA
plot(envelope(A.4_bf1925, Kest)) - NA
plot(envelope(A.4_bf1950, Kest))
plot(envelope(A.4_bf1975, Kest))
plot(envelope(A.4_bf2000, Kest))

Age Cohort Subsets (all at once) (4 values to change):
A4_1800_to_1824<-subset(A4_all_csv,A4_all_csv$Germ_Year>1799&A4_all_csv$Germ_Year<=1824) 
A4_1825_to_1849<-subset(A4_all_csv,A4_all_csv$Germ_Year>1824&A4_all_csv$Germ_Year<=1849)
A4_1850_to_1874<-subset(A4_all_csv,A4_all_csv$Germ_Year>1849&A4_all_csv$Germ_Year<=1874)
A4_1875_to_1899<-subset(A4_all_csv,A4_all_csv$Germ_Year>1874&A4_all_csv$Germ_Year<=1899)
A4_1900_to_1924<-subset(A4_all_csv,A4_all_csv$Germ_Year>1899&A4_all_csv$Germ_Year<=1924)
A4_1925_to_1949<-subset(A4_all_csv,A4_all_csv$Germ_Year>1924&A4_all_csv$Germ_Year<=1949)
A4_1950_to_1974<-subset(A4_all_csv,A4_all_csv$Germ_Year>1949&A4_all_csv$Germ_Year<=1974)
A4_1975_to_1999<-subset(A4_all_csv,A4_all_csv$Germ_Year>1974&A4_all_csv$Germ_Year<=1999)
A4_2000_to_2013<-subset(A4_all_csv,A4_all_csv$Germ_Year>1999&A4_all_csv$Germ_Year<=2013)

PPP Age Cohort (all at once) (4 values to change):
A.4_1800_to_1824<- ppp(A4_1800_to_1824$X, A4_1800_to_1824$Y, window=P4) 
A.4_1825_to_1849<- ppp(A4_1825_to_1849$X, A4_1825_to_1849$Y, window=P4) 
A.4_1850_to_1874<- ppp(A4_1850_to_1874$X, A4_1850_to_1874$Y, window=P4) 
A.4_1875_to_1899<- ppp(A4_1875_to_1899$X, A4_1875_to_1899$Y, window=P4) 
A.4_1900_to_1924<- ppp(A4_1900_to_1924$X, A4_1900_to_1924$Y, window=P4)
A.4_1925_to_1949<- ppp(A4_1925_to_1949$X, A4_1925_to_1949$Y, window=P4)
A.4_1950_to_1974<- ppp(A4_1950_to_1974$X, A4_1950_to_1974$Y, window=P4)
A.4_1975_to_1999<- ppp(A4_1975_to_1999$X, A4_1975_to_1999$Y, window=P4)
A.4_2000_to_2013<- ppp(A4_2000_to_2013$X, A4_2000_to_2013$Y, window=P4)

Plot Age Cohort (one at a time) (one value to change):
plot(A.4_1800_to_1824) - NA
plot(A.4_1825_to_1849) - NA
plot(A.4_1850_to_1874) - NA
plot(A.4_1875_to_1899) - NA
plot(A.4_1900_to_1924) - NA
plot(A.4_1925_to_1949)
plot(A.4_1950_to_1974)
plot(A.4_1975_to_1999)
plot(A.4_2000_to_2013)

Ripley's K, 99, Age Cohort (one at a time) (one value to change):
plot(envelope(A.4_1800_to_1824, Kest)) - NA
plot(envelope(A.4_1825_to_1849, Kest)) - NA
plot(envelope(A.4_1850_to_1874, Kest)) - NA
plot(envelope(A.4_1875_to_1899, Kest)) - NA
plot(envelope(A.4_1900_to_1924, Kest)) - NA
plot(envelope(A.4_1925_to_1949, Kest))
plot(envelope(A.4_1950_to_1974, Kest))
plot(envelope(A.4_1975_to_1999, Kest))
plot(envelope(A.4_2000_to_2013, Kest))


Density Maps (one at a time) (one value to change, each):

Cumulative
plot(density(A.4_bf1800,2)) - NA
plot(density(A.4_bf1825,2)) - NA
plot(density(A.4_bf1850,2)) - NA
plot(density(A.4_bf1875,2)) - NA
plot(density(A.4_bf1900,2)) - NA
plot(density(A.4_bf1925,2)) - NA
plot(density(A.4_bf1950,2))
plot(density(A.4_bf1975,2))
plot(density(A.4_bf2000,2))

Cohorts
plot(density(A.4_1800_to_1824,2)) - NA
plot(density(A.4_1825_to_1849,2)) - NA
plot(density(A.4_1850_to_1874,2)) - NA
plot(density(A.4_1875_to_1899,2)) - NA
plot(density(A.4_1900_to_1924,2)) - NA
plot(density(A.4_1925_to_1949,2))
plot(density(A.4_1950_to_1974,2))
plot(density(A.4_1975_to_1999,2))
plot(density(A.4_2000_to_2013,2))



----------------------- 




A5/Q3:

Choose File:
A5_all_csv<-read.csv(file=file.choose(), header=TRUE)

View File:
A5_all_csv

Minimum Germ_Year: 1923
min(A5_all_csv$Germ_Year)

Set Window Polygon (vertices counterclockwise):
P5 <- owin(poly = list(x = c(492192.14835, 492233.28217, 492201.14279, 492160.00897), y = c(4301128.06724, 4301162.58262, 4301200.88484, 4301166.36946)))

View Polygon:
plot(P5)

Make PPP (4 values to change):
A5_all<- ppp(A5_all_csv$X, A5_all_csv$Y, window=P5)

Plot:
plot(A5_all)

Summary:
summary(A5_all)

Cumulative Subsets (all at once) (3 values to change):
A5_bf1800<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1799)
A5_bf1825<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1824)
A5_bf1850<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1849)
A5_bf1875<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1874)
A5_bf1900<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1899)
A5_bf1925<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1924)
A5_bf1950<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1949)
A5_bf1975<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1974)
A5_bf2000<-subset(A5_all_csv,A5_all_csv$Germ_Year<=1999)

Make PPP Cumulative (all at once)(4 values to change):
A.5_bf1800<- ppp(A5_bf1800$X, A5_bf1800$Y, window=P5)
A.5_bf1825<- ppp(A5_bf1825$X, A5_bf1825$Y, window=P5)
A.5_bf1850<- ppp(A5_bf1850$X, A5_bf1850$Y, window=P5)
A.5_bf1875<- ppp(A5_bf1875$X, A5_bf1875$Y, window=P5)
A.5_bf1900<- ppp(A5_bf1900$X, A5_bf1900$Y, window=P5)
A.5_bf1925<- ppp(A5_bf1925$X, A5_bf1925$Y, window=P5)
A.5_bf1950<- ppp(A5_bf1950$X, A5_bf1950$Y, window=P5)
A.5_bf1975<- ppp(A5_bf1975$X, A5_bf1975$Y, window=P5)
A.5_bf2000<- ppp(A5_bf2000$X, A5_bf2000$Y, window=P5)

Plot Cumulative (one at a time):
plot(A.5_bf1800) - NA
plot(A.5_bf1825) - NA
plot(A.5_bf1850) - NA
plot(A.5_bf1875) - NA
plot(A.5_bf1900) - NA
plot(A.5_bf1925)
plot(A.5_bf1950)
plot(A.5_bf1975)
plot(A.5_bf2000)

Kest Cumulative (one at a time):
plot(envelope(A.5_bf1800, Kest)) - NA
plot(envelope(A.5_bf1825, Kest)) - NA
plot(envelope(A.5_bf1850, Kest)) - NA
plot(envelope(A.5_bf1875, Kest)) - NA
plot(envelope(A.5_bf1900, Kest)) - NA
plot(envelope(A.5_bf1925, Kest)) - NA
plot(envelope(A.5_bf1950, Kest))
plot(envelope(A.5_bf1975, Kest))
plot(envelope(A.5_bf2000, Kest))

Age Cohort Subsets (all at once) (4 values to change):
A5_1800_to_1824<-subset(A5_all_csv,A5_all_csv$Germ_Year>1799&A5_all_csv$Germ_Year<=1824) 
A5_1825_to_1849<-subset(A5_all_csv,A5_all_csv$Germ_Year>1824&A5_all_csv$Germ_Year<=1849)
A5_1850_to_1874<-subset(A5_all_csv,A5_all_csv$Germ_Year>1849&A5_all_csv$Germ_Year<=1874)
A5_1875_to_1899<-subset(A5_all_csv,A5_all_csv$Germ_Year>1874&A5_all_csv$Germ_Year<=1899)
A5_1900_to_1924<-subset(A5_all_csv,A5_all_csv$Germ_Year>1899&A5_all_csv$Germ_Year<=1924)
A5_1925_to_1949<-subset(A5_all_csv,A5_all_csv$Germ_Year>1924&A5_all_csv$Germ_Year<=1949)
A5_1950_to_1974<-subset(A5_all_csv,A5_all_csv$Germ_Year>1949&A5_all_csv$Germ_Year<=1974)
A5_1975_to_1999<-subset(A5_all_csv,A5_all_csv$Germ_Year>1974&A5_all_csv$Germ_Year<=1999)
A5_2000_to_2013<-subset(A5_all_csv,A5_all_csv$Germ_Year>1999&A5_all_csv$Germ_Year<=2013)

PPP Age Cohort (all at once) (4 values to change):
A.5_1800_to_1824<- ppp(A5_1800_to_1824$X, A5_1800_to_1824$Y, window=P5) 
A.5_1825_to_1849<- ppp(A5_1825_to_1849$X, A5_1825_to_1849$Y, window=P5) 
A.5_1850_to_1874<- ppp(A5_1850_to_1874$X, A5_1850_to_1874$Y, window=P5) 
A.5_1875_to_1899<- ppp(A5_1875_to_1899$X, A5_1875_to_1899$Y, window=P5) 
A.5_1900_to_1924<- ppp(A5_1900_to_1924$X, A5_1900_to_1924$Y, window=P5)
A.5_1925_to_1949<- ppp(A5_1925_to_1949$X, A5_1925_to_1949$Y, window=P5)
A.5_1950_to_1974<- ppp(A5_1950_to_1974$X, A5_1950_to_1974$Y, window=P5)
A.5_1975_to_1999<- ppp(A5_1975_to_1999$X, A5_1975_to_1999$Y, window=P5)
A.5_2000_to_2013<- ppp(A5_2000_to_2013$X, A5_2000_to_2013$Y, window=P5)

Plot Age Cohort (one at a time) (one value to change):
plot(A.5_1800_to_1824) - NA
plot(A.5_1825_to_1849) - NA
plot(A.5_1850_to_1874) - NA
plot(A.5_1875_to_1899) - NA
plot(A.5_1900_to_1924)
plot(A.5_1925_to_1949)
plot(A.5_1950_to_1974)
plot(A.5_1975_to_1999)
plot(A.5_2000_to_2013)

Ripley's K, 99, Age Cohort (one at a time) (one value to change):
plot(envelope(A.5_1800_to_1824, Kest)) - NA
plot(envelope(A.5_1825_to_1849, Kest)) - NA
plot(envelope(A.5_1850_to_1874, Kest)) - NA
plot(envelope(A.5_1875_to_1899, Kest)) - NA
plot(envelope(A.5_1900_to_1924, Kest)) - NA
plot(envelope(A.5_1925_to_1949, Kest)) - NA
plot(envelope(A.5_1950_to_1974, Kest))
plot(envelope(A.5_1975_to_1999, Kest))
plot(envelope(A.5_2000_to_2013, Kest)) - NA


Density Maps (one at a time) (one value to change, each):

Cumulative
plot(density(A.5_bf1800,2)) - NA
plot(density(A.5_bf1825,2)) - NA
plot(density(A.5_bf1850,2)) - NA
plot(density(A.5_bf1875,2)) - NA
plot(density(A.5_bf1900,2)) - NA
plot(density(A.5_bf1925,2))
plot(density(A.5_bf1950,2))
plot(density(A.5_bf1975,2))
plot(density(A.5_bf2000,2))

Cohorts
plot(density(A.5_1800_to_1824,2)) - NA
plot(density(A.5_1825_to_1849,2)) - NA
plot(density(A.5_1850_to_1874,2)) - NA
plot(density(A.5_1875_to_1899,2)) - NA
plot(density(A.5_1900_to_1924,2))
plot(density(A.5_1925_to_1949,2))
plot(density(A.5_1950_to_1974,2))
plot(density(A.5_1975_to_1999,2))
plot(density(A.5_2000_to_2013,2)) - NA


END A5/Q3

--------------------------------

A6/Q2:

Choose File:
A6_all_csv<-read.csv(file=file.choose(), header=TRUE)

View File:
A6_all_csv

Minimum Germ_Year:1939
min(A6_all_csv$Germ_Year)

Set Window Polygon (vertices counterclockwise):
P6 <- owin(poly = list(x = c(492224.28773, 492265.42155, 492233.28217, 492192.14835), y = c(4301089.76502, 4301124.28039, 4301162.58262, 4301128.06724)))

View Polygon:
plot(P6)

Make PPP (4 values to change):
A6_all<- ppp(A6_all_csv$X, A6_all_csv$Y, window=P6)

Plot:
plot(A6_all)

Summary:
summary(A6_all)

Cumulative Subsets (all at once) (3 values to change):
A6_bf1800<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1799)
A6_bf1825<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1824)
A6_bf1850<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1849)
A6_bf1875<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1874)
A6_bf1900<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1899)
A6_bf1925<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1924)
A6_bf1950<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1949)
A6_bf1975<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1974)
A6_bf2000<-subset(A6_all_csv,A6_all_csv$Germ_Year<=1999)

Make PPP Cumulative (all at once)(4 values to change):
A.6_bf1800<- ppp(A6_bf1800$X, A6_bf1800$Y, window=P6)
A.6_bf1825<- ppp(A6_bf1825$X, A6_bf1825$Y, window=P6)
A.6_bf1850<- ppp(A6_bf1850$X, A6_bf1850$Y, window=P6)
A.6_bf1875<- ppp(A6_bf1875$X, A6_bf1875$Y, window=P6)
A.6_bf1900<- ppp(A6_bf1900$X, A6_bf1900$Y, window=P6)
A.6_bf1925<- ppp(A6_bf1925$X, A6_bf1925$Y, window=P6)
A.6_bf1950<- ppp(A6_bf1950$X, A6_bf1950$Y, window=P6)
A.6_bf1975<- ppp(A6_bf1975$X, A6_bf1975$Y, window=P6)
A.6_bf2000<- ppp(A6_bf2000$X, A6_bf2000$Y, window=P6)

Plot Cumulative (one at a time):
plot(A.6_bf1800) - NA
plot(A.6_bf1825) - NA
plot(A.6_bf1850) - NA
plot(A.6_bf1875) - NA
plot(A.6_bf1900) - NA
plot(A.6_bf1925) - NA
plot(A.6_bf1950)
plot(A.6_bf1975)
plot(A.6_bf2000)

Kest Cumulative (one at a time):
plot(envelope(A.6_bf1800, Kest)) - NA
plot(envelope(A.6_bf1825, Kest)) - NA
plot(envelope(A.6_bf1850, Kest)) - NA
plot(envelope(A.6_bf1875, Kest)) - NA
plot(envelope(A.6_bf1900, Kest)) - NA
plot(envelope(A.6_bf1925, Kest)) - NA
plot(envelope(A.6_bf1950, Kest))
plot(envelope(A.6_bf1975, Kest))
plot(envelope(A.6_bf2000, Kest))

Age Cohort Subsets (all at once) (4 values to change):
A6_1800_to_1824<-subset(A6_all_csv,A6_all_csv$Germ_Year>1799&A6_all_csv$Germ_Year<=1824) 
A6_1825_to_1849<-subset(A6_all_csv,A6_all_csv$Germ_Year>1824&A6_all_csv$Germ_Year<=1849)
A6_1850_to_1874<-subset(A6_all_csv,A6_all_csv$Germ_Year>1849&A6_all_csv$Germ_Year<=1874)
A6_1875_to_1899<-subset(A6_all_csv,A6_all_csv$Germ_Year>1874&A6_all_csv$Germ_Year<=1899)
A6_1900_to_1924<-subset(A6_all_csv,A6_all_csv$Germ_Year>1899&A6_all_csv$Germ_Year<=1924)
A6_1925_to_1949<-subset(A6_all_csv,A6_all_csv$Germ_Year>1924&A6_all_csv$Germ_Year<=1949)
A6_1950_to_1974<-subset(A6_all_csv,A6_all_csv$Germ_Year>1949&A6_all_csv$Germ_Year<=1974)
A6_1975_to_1999<-subset(A6_all_csv,A6_all_csv$Germ_Year>1974&A6_all_csv$Germ_Year<=1999)
A6_2000_to_2013<-subset(A6_all_csv,A6_all_csv$Germ_Year>1999&A6_all_csv$Germ_Year<=2013)

PPP Age Cohort (all at once) (4 values to change):
A.6_1800_to_1824<- ppp(A6_1800_to_1824$X, A6_1800_to_1824$Y, window=P6) 
A.6_1825_to_1849<- ppp(A6_1825_to_1849$X, A6_1825_to_1849$Y, window=P6) 
A.6_1850_to_1874<- ppp(A6_1850_to_1874$X, A6_1850_to_1874$Y, window=P6) 
A.6_1875_to_1899<- ppp(A6_1875_to_1899$X, A6_1875_to_1899$Y, window=P6) 
A.6_1900_to_1924<- ppp(A6_1900_to_1924$X, A6_1900_to_1924$Y, window=P6)
A.6_1925_to_1949<- ppp(A6_1925_to_1949$X, A6_1925_to_1949$Y, window=P6)
A.6_1950_to_1974<- ppp(A6_1950_to_1974$X, A6_1950_to_1974$Y, window=P6)
A.6_1975_to_1999<- ppp(A6_1975_to_1999$X, A6_1975_to_1999$Y, window=P6)
A.6_2000_to_2013<- ppp(A6_2000_to_2013$X, A6_2000_to_2013$Y, window=P6)

Plot Age Cohort (one at a time) (one value to change):
plot(A.6_1800_to_1824) - NA
plot(A.6_1825_to_1849) - NA
plot(A.6_1850_to_1874) - NA
plot(A.6_1875_to_1899) - NA
plot(A.6_1900_to_1924) - NA
plot(A.6_1925_to_1949)
plot(A.6_1950_to_1974)
plot(A.6_1975_to_1999)
plot(A.6_2000_to_2013)

Ripley's K, 99, Age Cohort (one at a time) (one value to change):
plot(envelope(A.6_1800_to_1824, Kest)) - NA
plot(envelope(A.6_1825_to_1849, Kest)) - NA
plot(envelope(A.6_1850_to_1874, Kest)) - NA
plot(envelope(A.6_1875_to_1899, Kest)) - NA
plot(envelope(A.6_1900_to_1924, Kest)) - NA
plot(envelope(A.6_1925_to_1949, Kest))
plot(envelope(A.6_1950_to_1974, Kest))
plot(envelope(A.6_1975_to_1999, Kest))
plot(envelope(A.6_2000_to_2013, Kest))


Density Maps (one at a time) (one value to change, each):

Cumulative
plot(density(A.6_bf1800,2)) - NA
plot(density(A.6_bf1825,2)) - NA
plot(density(A.6_bf1850,2)) - NA
plot(density(A.6_bf1875,2)) - NA
plot(density(A.6_bf1900,2)) - NA
plot(density(A.6_bf1925,2)) - NA
plot(density(A.6_bf1950,2))
plot(density(A.6_bf1975,2))
plot(density(A.6_bf2000,2))

Cohorts
plot(density(A.6_1800_to_1824,2)) - NA
plot(density(A.6_1825_to_1849,2)) - NA
plot(density(A.6_1850_to_1874,2)) - NA
plot(density(A.6_1875_to_1899,2)) - NA
plot(density(A.6_1900_to_1924,2)) - NA
plot(density(A.6_1925_to_1949,2))
plot(density(A.6_1950_to_1974,2))
plot(density(A.6_1975_to_1999,2))
plot(density(A.6_2000_to_2013,2))


END A6/Q2

------------------------------------------



A7/Q1:

Choose File:
A7_all_csv<-read.csv(file=file.choose(), header=TRUE)

View File:
A7_all_csv

Minimum Germ_Year: 1982
min(A7_all_csv$Germ_Year)

Set Window Polygon (vertices counterclockwise):
P7 <- owin(poly = list(x = c(492256.42711, 492297.56093, 492265.42155, 492224.28773), y = c(4301051.46280, 4301085.97817, 4301124.28039, 4301089.76502)))

View Polygon:
plot(P7)

Make PPP (4 values to change):
A7_all<- ppp(A7_all_csv$X, A7_all_csv$Y, window=P7)

Plot:
plot(A7_all)

Summary:
summary(A7_all)

Cumulative Subsets (all at once) (3 values to change):
A7_bf1800<-subset(A7_all_csv,A7_all_csv$Germ_Year<=1799)
A7_bf1825<-subset(A7_all_csv,A7_all_csv$Germ_Year<=1824)
A7_bf1850<-subset(A7_all_csv,A7_all_csv$Germ_Year<=1849)
A7_bf1875<-subset(A7_all_csv,A_all_csv$Germ_Year<=1874)
A7_bf1900<-subset(A7_all_csv,A7_all_csv$Germ_Year<=1899)
A7_bf1925<-subset(A7_all_csv,A7_all_csv$Germ_Year<=1924)
A7_bf1950<-subset(A7_all_csv,A7_all_csv$Germ_Year<=1949)
A7_bf1975<-subset(A7_all_csv,A7_all_csv$Germ_Year<=1974)
A7_bf2000<-subset(A7_all_csv,A7_all_csv$Germ_Year<=1999)

Make PPP Cumulative (all at once)(4 values to change):
A.7_bf1800<- ppp(A7_bf1800$X, A7_bf1800$Y, window=P7)
A.7_bf1825<- ppp(A7_bf1825$X, A7_bf1825$Y, window=P7)
A.7_bf1850<- ppp(A7_bf1850$X, A7_bf1850$Y, window=P7)
A.7_bf1875<- ppp(A7_bf1875$X, A7_bf1875$Y, window=P7)
A.7_bf1900<- ppp(A7_bf1900$X, A7_bf1900$Y, window=P7)
A.7_bf1925<- ppp(A7_bf1925$X, A7_bf1925$Y, window=P7)
A.7_bf1950<- ppp(A7_bf1950$X, A7_bf1950$Y, window=P7)
A.7_bf1975<- ppp(A7_bf1975$X, A7_bf1975$Y, window=P7)
A.7_bf2000<- ppp(A7_bf2000$X, A7_bf2000$Y, window=P7)

Plot Cumulative (one at a time):
plot(A.7_bf1800) - NA
plot(A.7_bf1825) - NA
plot(A.7_bf1850) - NA
plot(A.7_bf1875) - NA
plot(A.7_bf1900) - NA
plot(A.7_bf1925) - NA
plot(A.7_bf1950) - NA
plot(A.7_bf1975) - NA
plot(A.7_bf2000)

Kest Cumulative (one at a time):
plot(envelope(A.7_bf1800, Kest)) - NA
plot(envelope(A.7_bf1825, Kest)) - NA
plot(envelope(A.7_bf1850, Kest)) - NA
plot(envelope(A.7_bf1875, Kest)) - NA
plot(envelope(A.7_bf1900, Kest)) - NA
plot(envelope(A.7_bf1925, Kest)) - NA
plot(envelope(A.7_bf1950, Kest)) - NA
plot(envelope(A.7_bf1975, Kest)) - NA
plot(envelope(A.7_bf2000, Kest))

Age Cohort Subsets (all at once) (4 values to change):
A7_1800_to_1824<-subset(A7_all_csv,A7_all_csv$Germ_Year>1799&A7_all_csv$Germ_Year<=1824) 
A7_1825_to_1849<-subset(A7_all_csv,A7_all_csv$Germ_Year>1824&A7_all_csv$Germ_Year<=1849)
A7_1850_to_1874<-subset(A7_all_csv,A7_all_csv$Germ_Year>1849&A7_all_csv$Germ_Year<=1874)
A7_1875_to_1899<-subset(A7_all_csv,A7_all_csv$Germ_Year>1874&A7_all_csv$Germ_Year<=1899)
A7_1900_to_1924<-subset(A7_all_csv,A7_all_csv$Germ_Year>1899&A7_all_csv$Germ_Year<=1924)
A7_1925_to_1949<-subset(A7_all_csv,A7_all_csv$Germ_Year>1924&A7_all_csv$Germ_Year<=1949)
A7_1950_to_1974<-subset(A7_all_csv,A7_all_csv$Germ_Year>1949&A7_all_csv$Germ_Year<=1974)
A7_1975_to_1999<-subset(A7_all_csv,A7_all_csv$Germ_Year>1974&A7_all_csv$Germ_Year<=1999)
A7_2000_to_2013<-subset(A7_all_csv,A7_all_csv$Germ_Year>1999&A7_all_csv$Germ_Year<=2013)

PPP Age Cohort (all at once) (4 values to change):
A.7_1800_to_1824<- ppp(A7_1800_to_1824$X, A7_1800_to_1824$Y, window=P7) 
A.7_1825_to_1849<- ppp(A7_1825_to_1849$X, A7_1825_to_1849$Y, window=P7) 
A.7_1850_to_1874<- ppp(A7_1850_to_1874$X, A7_1850_to_1874$Y, window=P7) 
A.7_1875_to_1899<- ppp(A7_1875_to_1899$X, A7_1875_to_1899$Y, window=P7) 
A.7_1900_to_1924<- ppp(A7_1900_to_1924$X, A7_1900_to_1924$Y, window=P7)
A.7_1925_to_1949<- ppp(A7_1925_to_1949$X, A7_1925_to_1949$Y, window=P7)
A.7_1950_to_1974<- ppp(A7_1950_to_1974$X, A7_1950_to_1974$Y, window=P7)
A.7_1975_to_1999<- ppp(A7_1975_to_1999$X, A7_1975_to_1999$Y, window=P7)
A.7_2000_to_2013<- ppp(A7_2000_to_2013$X, A7_2000_to_2013$Y, window=P7)

Plot Age Cohort (one at a time) (one value to change):
plot(A.7_1800_to_1824) - NA
plot(A.7_1825_to_1849) - NA
plot(A.7_1850_to_1874) - NA
plot(A.7_1875_to_1899) - NA
plot(A.7_1900_to_1924) - NA
plot(A.7_1925_to_1949) - NA
plot(A.7_1950_to_1974) - NA
plot(A.7_1975_to_1999)
plot(A.7_2000_to_2013) - NA

Ripley's K, 99, Age Cohort (one at a time) (one value to change):
plot(envelope(A.7_1800_to_1824, Kest)) - NA
plot(envelope(A.7_1825_to_1849, Kest)) - NA
plot(envelope(A.7_1850_to_1874, Kest)) - NA
plot(envelope(A.7_1875_to_1899, Kest)) - NA
plot(envelope(A.7_1900_to_1924, Kest)) - NA
plot(envelope(A.7_1925_to_1949, Kest)) - NA
plot(envelope(A.7_1950_to_1974, Kest)) - NA
plot(envelope(A.7_1975_to_1999, Kest))
plot(envelope(A.7_2000_to_2013, Kest)) - NA


Density Maps (one at a time) (one value to change, each):

Cumulative
plot(density(A.7_bf1800,2)) - NA
plot(density(A.7_bf1825,2)) - NA
plot(density(A.7_bf1850,2)) - NA
plot(density(A.7_bf1875,2)) - NA
plot(density(A.7_bf1900,2)) - NA
plot(density(A.7_bf1925,2)) - NA
plot(density(A.7_bf1950,2)) - NA
plot(density(A.7_bf1975,2)) - NA
plot(density(A.7_bf2000,2))

Cohorts
plot(density(A.7_1800_to_1824,2)) - NA
plot(density(A.7_1825_to_1849,2)) - NA
plot(density(A.7_1850_to_1874,2)) - NA
plot(density(A.7_1875_to_1899,2)) - NA
plot(density(A.7_1900_to_1924,2)) - NA
plot(density(A.7_1925_to_1949,2)) - NA
plot(density(A.7_1950_to_1974,2)) - NA
plot(density(A.7_1975_to_1999,2))
plot(density(A.7_2000_to_2013,2)) - NA


END A7/Q1

------------------------------------------


Min/Max Germ_Year and Number Values per Quadrant per Cohort:

Summary Stats:

summary(A1_all_csv)
summary(A2_all_csv)
summary(A3_all_csv)
summary(A4_all_csv)
summary(A5_all_csv)
summary(A6_all_csv)
summary(A7_all_csv)

Number of Trees per Cohort:

nrow(A1_bf1800)
nrow(A1_1800_to_1824)
nrow(A1_1825_to_1849)
nrow(A1_1850_to_1874)
nrow(A1_1875_to_1899)
nrow(A1_1900_to_1924)
nrow(A1_1925_to_1949)
nrow(A1_1950_to_1974)
nrow(A1_1975_to_1999)
nrow(A1_2000_to_2013)

nrow(A2_bf1800)
nrow(A2_1800_to_1824)
nrow(A2_1825_to_1849)
nrow(A2_1850_to_1874)
nrow(A2_1875_to_1899)
nrow(A2_1900_to_1924)
nrow(A2_1925_to_1949)
nrow(A2_1950_to_1974)
nrow(A2_1975_to_1999)
nrow(A2_2000_to_2013)

nrow(A3_bf1800)
nrow(A3_1800_to_1824)
nrow(A3_1825_to_1849)
nrow(A3_1850_to_1874)
nrow(A3_1875_to_1899)
nrow(A3_1900_to_1924)
nrow(A3_1925_to_1949)
nrow(A3_1950_to_1974)
nrow(A3_1975_to_1999)
nrow(A3_2000_to_2013)

nrow(A4_bf1800)
nrow(A4_1800_to_1824)
nrow(A4_1825_to_1849)
nrow(A4_1850_to_1874)
nrow(A4_1875_to_1899)
nrow(A4_1900_to_1924)
nrow(A4_1925_to_1949)
nrow(A4_1950_to_1974)
nrow(A4_1975_to_1999)
nrow(A4_2000_to_2013)

nrow(A5_bf1800)
nrow(A5_1800_to_1824)
nrow(A5_1825_to_1849)
nrow(A5_1850_to_1874)
nrow(A5_1875_to_1899)
nrow(A5_1900_to_1924)
nrow(A5_1925_to_1949)
nrow(A5_1950_to_1974)
nrow(A5_1975_to_1999)
nrow(A5_2000_to_2013)

nrow(A6_bf1800)
nrow(A6_1800_to_1824)
nrow(A6_1825_to_1849)
nrow(A6_1850_to_1874)
nrow(A6_1875_to_1899)
nrow(A6_1900_to_1924)
nrow(A6_1925_to_1949)
nrow(A6_1950_to_1974)
nrow(A6_1975_to_1999)
nrow(A6_2000_to_2013)

nrow(A7_bf1800)
nrow(A7_1800_to_1824)
nrow(A7_1825_to_1849)
nrow(A7_1850_to_1874)
nrow(A7_1875_to_1899)
nrow(A7_1900_to_1924)
nrow(A7_1925_to_1949)
nrow(A7_1950_to_1974)
nrow(A7_1975_to_1999)
nrow(A7_2000_to_2013)


---------------------------------------------

*QUADRAT COUNTS

Cumulative:

quadrat.test(A.1_bf1800, nx=3, ny=3)
quadrat.test(A.1_bf1825, nx=3, ny=3)
quadrat.test(A.1_bf1850, nx=3, ny=3)
quadrat.test(A.1_bf1875, nx=3, ny=3)
quadrat.test(A.1_bf1900, nx=3, ny=3)
quadrat.test(A.1_bf1925, nx=3, ny=3)
quadrat.test(A.1_bf1950, nx=3, ny=3)
quadrat.test(A.1_bf1975, nx=3, ny=3)
quadrat.test(A.1_bf2000, nx=3, ny=3)

quadrat.test(A.2_bf1800, nx=3, ny=3)
quadrat.test(A.2_bf1825, nx=3, ny=3)
quadrat.test(A.2_bf1850, nx=3, ny=3)
quadrat.test(A.2_bf1875, nx=3, ny=3)
quadrat.test(A.2_bf1900, nx=3, ny=3)
quadrat.test(A.2_bf1925, nx=3, ny=3)
quadrat.test(A.2_bf1950, nx=3, ny=3)
quadrat.test(A.2_bf1975, nx=3, ny=3)
quadrat.test(A.2_bf2000, nx=3, ny=3)

quadrat.test(A.3_bf1800, nx=3, ny=3)
quadrat.test(A.3_bf1825, nx=3, ny=3)
quadrat.test(A.3_bf1850, nx=3, ny=3)
quadrat.test(A.3_bf1875, nx=3, ny=3)
quadrat.test(A.3_bf1900, nx=3, ny=3)
quadrat.test(A.3_bf1925, nx=3, ny=3)
quadrat.test(A.3_bf1950, nx=3, ny=3)
quadrat.test(A.3_bf1975, nx=3, ny=3)
quadrat.test(A.3_bf2000, nx=3, ny=3)

quadrat.test(A.4_bf1800, nx=3, ny=3)
quadrat.test(A.4_bf1825, nx=3, ny=3)
quadrat.test(A.4_bf1850, nx=3, ny=3)
quadrat.test(A.4_bf1875, nx=3, ny=3)
quadrat.test(A.4_bf1900, nx=3, ny=3)
quadrat.test(A.4_bf1925, nx=3, ny=3)
quadrat.test(A.4_bf1950, nx=3, ny=3)
quadrat.test(A.4_bf1975, nx=3, ny=3)
quadrat.test(A.4_bf2000, nx=3, ny=3)

quadrat.test(A.5_bf1800, nx=3, ny=3)
quadrat.test(A.5_bf1825, nx=3, ny=3)
quadrat.test(A.5_bf1850, nx=3, ny=3)
quadrat.test(A.5_bf1875, nx=3, ny=3)
quadrat.test(A.5_bf1900, nx=3, ny=3)
quadrat.test(A.5_bf1925, nx=3, ny=3)
quadrat.test(A.5_bf1950, nx=3, ny=3)
quadrat.test(A.5_bf1975, nx=3, ny=3)
quadrat.test(A.5_bf2000, nx=3, ny=3)

quadrat.test(A.6_bf1800, nx=3, ny=3)
quadrat.test(A.6_bf1825, nx=3, ny=3)
quadrat.test(A.6_bf1850, nx=3, ny=3)
quadrat.test(A.6_bf1875, nx=3, ny=3)
quadrat.test(A.6_bf1900, nx=3, ny=3)
quadrat.test(A.6_bf1925, nx=3, ny=3)
quadrat.test(A.6_bf1950, nx=3, ny=3)
quadrat.test(A.6_bf1975, nx=3, ny=3)
quadrat.test(A.6_bf2000, nx=3, ny=3)

quadrat.test(A.7_bf1800, nx=3, ny=3)
quadrat.test(A.7_bf1825, nx=3, ny=3)
quadrat.test(A.7_bf1850, nx=3, ny=3)
quadrat.test(A.7_bf1875, nx=3, ny=3)
quadrat.test(A.7_bf1900, nx=3, ny=3)
quadrat.test(A.7_bf1925, nx=3, ny=3)
quadrat.test(A.7_bf1950, nx=3, ny=3)
quadrat.test(A.7_bf1975, nx=3, ny=3)
quadrat.test(A.7_bf2000, nx=3, ny=3)

Cohorts:

quadrat.test(A.1_bf1800, nx=3, ny=3)
quadrat.test(A.1_1800_to_1824, nx=3, ny=3)
quadrat.test(A.1_1825_to_1849, nx=3, ny=3)
quadrat.test(A.1_1850_to_1874, nx=3, ny=3)
quadrat.test(A.1_1875_to_1899, nx=3, ny=3)
quadrat.test(A.1_1900_to_1924, nx=3, ny=3)
quadrat.test(A.1_1925_to_1949, nx=3, ny=3)
quadrat.test(A.1_1950_to_1974, nx=3, ny=3)
quadrat.test(A.1_1975_to_1999, nx=3, ny=3)

quadrat.test(A.2_1800_to_1824, nx=3, ny=3)
quadrat.test(A.2_1825_to_1849, nx=3, ny=3)
quadrat.test(A.2_1850_to_1874, nx=3, ny=3)
quadrat.test(A.2_1875_to_1899, nx=3, ny=3)
quadrat.test(A.2_1900_to_1924, nx=3, ny=3)
quadrat.test(A.2_1925_to_1949, nx=3, ny=3)
quadrat.test(A.2_1950_to_1974, nx=3, ny=3)
quadrat.test(A.2_1975_to_1999, nx=3, ny=3)

quadrat.test(A.3_1800_to_1824, nx=3, ny=3)
quadrat.test(A.3_1825_to_1849, nx=3, ny=3)
quadrat.test(A.3_1850_to_1874, nx=3, ny=3)
quadrat.test(A.3_1875_to_1899, nx=3, ny=3)
quadrat.test(A.3_1900_to_1924, nx=3, ny=3)
quadrat.test(A.3_1925_to_1949, nx=3, ny=3)
quadrat.test(A.3_1950_to_1974, nx=3, ny=3)
quadrat.test(A.3_1975_to_1999, nx=3, ny=3)

quadrat.test(A.4_1800_to_1824, nx=3, ny=3)
quadrat.test(A.4_1825_to_1849, nx=3, ny=3)
quadrat.test(A.4_1850_to_1874, nx=3, ny=3)
quadrat.test(A.4_1875_to_1899, nx=3, ny=3)
quadrat.test(A.4_1900_to_1924, nx=3, ny=3)
quadrat.test(A.4_1925_to_1949, nx=3, ny=3)
quadrat.test(A.4_1950_to_1974, nx=3, ny=3)
quadrat.test(A.4_1975_to_1999, nx=3, ny=3)

quadrat.test(A.5_1800_to_1824, nx=3, ny=3)
quadrat.test(A.5_1825_to_1849, nx=3, ny=3)
quadrat.test(A.5_1850_to_1874, nx=3, ny=3)
quadrat.test(A.5_1875_to_1899, nx=3, ny=3)
quadrat.test(A.5_1900_to_1924, nx=3, ny=3)
quadrat.test(A.5_1925_to_1949, nx=3, ny=3)
quadrat.test(A.5_1950_to_1974, nx=3, ny=3)
quadrat.test(A.5_1975_to_1999, nx=3, ny=3)

quadrat.test(A.6_1800_to_1824, nx=3, ny=3)
quadrat.test(A.6_1825_to_1849, nx=3, ny=3)
quadrat.test(A.6_1850_to_1874, nx=3, ny=3)
quadrat.test(A.6_1875_to_1899, nx=3, ny=3)
quadrat.test(A.6_1900_to_1924, nx=3, ny=3)
quadrat.test(A.6_1925_to_1949, nx=3, ny=3)
quadrat.test(A.6_1950_to_1974, nx=3, ny=3)
quadrat.test(A.6_1975_to_1999, nx=3, ny=3)

quadrat.test(A.7_1800_to_1824, nx=3, ny=3)
quadrat.test(A.7_1825_to_1849, nx=3, ny=3)
quadrat.test(A.7_1850_to_1874, nx=3, ny=3)
quadrat.test(A.7_1875_to_1899, nx=3, ny=3)
quadrat.test(A.7_1900_to_1924, nx=3, ny=3)
quadrat.test(A.7_1925_to_1949, nx=3, ny=3)
quadrat.test(A.7_1950_to_1974, nx=3, ny=3)
quadrat.test(A.7_1975_to_1999, nx=3, ny=3)


