#202080200 yasemin aylin akturk hw1

#create data set ----

set.seed(1234567)

dpoint = 1000

dim_1 = runif(dpoint, min = -1 , max = 1)

mydata= data.frame(dim_1)

dim= 15
colno=dim-1

for(i in 1:colno) {                                   
  new <- runif(dpoint, min = -1 , max = 1)                      # Create new column
  mydata[ , ncol(mydata) + 1] <- new                       # Append new column
  colnames(mydata)[ncol(mydata)] <- paste0("dim_", i+1)    # Rename column name
}


#show sample data
mydata[1:3,1:15]


#plot some of the rows to see the distribution----

#dev.off()
par(mfrow=c(2,2))

plot(mydata[,3])
plot(mydata[,6])
plot(mydata[,9])
plot(mydata[,12])

#dev.off()

#calculate Eucl. distance to origin----

#create empty plot
plot(0,0,ylim= c(0,1),xlim= c(0,15),col="white",main= "fraction vs dimension", xlab = "dimension", ylab = "fraction" )

for (spcdim in 1:15 ) {
  
origin <- rep(0, spcdim)

origin

#calculate distance bw points and org

distlist <- c()

dist= 0

for (i in 1: dpoint) {

for (j in 1:spcdim) {


  dist = dist + (mydata[i,j]-origin[j])^2
  

} #end j
  
  distlist <- append(distlist, sqrt(dist))
  dist=0

} #end i


#how many within 1?

counter=0
for (i in 1:dpoint) 
  {
  if (distlist[i] <= 1) { counter = counter +1}
  fraction = counter/ dpoint
 
}

#keep fractions for 2d and 3d
if (spcdim==2) {fraction2d = fraction}
if (spcdim==3) {fraction3d = fraction}


#add poinsts
points(spcdim,fraction,col= 5*spcdim)

} #end spcdim

#estimate pi for 2d and 3d----

#n/N = fraction2d = circle area / 2by2 square area =  pi.r^2 / (2*2) where r=1  

# pi estimate =   fraction2d * 4

pi2d = fraction2d * 4
pi2d
 
#n/N = fraction3d = sphere volume / 2by2by2 cube volume area = 4/3 * pi.r^2 / (2*2*2)  where r=1

# pi estimate =   fraction3d * 6

pi3d = fraction3d * 6
pi3d

###############################################################larger data----


#create data set ----

pilist2 <- c()
pilist3 <- c()
datapoint <-   c(1000,5000,10000,25000,50000,100000)

for (dpoint in   c(1000,5000,10000,25000,50000,100000)) {

dim_1 = runif(dpoint, min = -1 , max = 1)

mydata= data.frame(dim_1)

dim =3

for(i in 1:3) {                                   
  new <- runif(dpoint, min = -1 , max = 1)                      
  mydata[ , ncol(mydata) + 1] <- new                      
  colnames(mydata)[ncol(mydata)] <- paste0("dim_", i+1)    
              }



#calculate Eucl. distance to origin----



for (spcdim in 1:3) {
  
  
  origin <- rep(0, spcdim)
  

  #calculate distance bw points and org
  
  distlist <- c()
  
  dist= 0
  
  for (i in 1: dpoint) 
    
  {
    
    for (j in 1:spcdim)
      
    {
     
    dist = dist + (mydata[i,j]-origin[j])^2
      
    } #end j
    
  distlist <- append(distlist, sqrt(dist))
  dist=0
    
  } #end i
  
  
  #how many within 1?
  
  counter=0
  for (i in 1:dpoint) 
  {
  if (distlist[i] <= 1) { counter = counter +1}
  fraction = counter/ dpoint
    
  }
  
  #keep fractions for 2d and 3d
  
 
  
  if (spcdim==2)
    
  {fraction2d = fraction
  
  
  #estimate pi for 2d and 3d----
  
  #n/N = fraction2d = circle area / 2by2 square area =  pi.r^2 / (2*2) where r=1  
  
  # pi estimate =   fraction2d * 4
  
  pi2d = fraction2d * 4
  
  
  
  pilist2 <- append(pilist2 , pi2d )
  
    }
  
  
  if (spcdim==3) 
    
   {fraction3d = fraction

  
  #n/N = fraction3d = sphere volume / 2by2by2 cube volume area = 4/3 * pi.r^2 / (2*2*2)  where r=1
  
  # pi estimate =   fraction3d * 6
  
  pi3d = fraction3d * 6
  
  pilist3 <- append(pilist3 , pi3d )
  
  }
  
 
} #end spcdim



} #end dpoint

########show pi estimates and plot them----
pilist2
pilist3

par(mfrow=c(2,2))

plot(datapoint,pilist2, xlim=c(1000,100000), ylab = "estimated pi",xlab = "#ofdatapoints", main ="estimated pi vs #datapoint on 2D" , col = "red")

plot(datapoint,pilist3, xlim=c(1000,100000), ylab = "estimated pi", xlab = "#ofdatapoints", main ="estimated pi vs #datapoint on 3D", col= "blue")

############nearest point----

dpoint = 1000 #datapoint
tpoint = 100   #test

dim_1 = runif(dpoint, min = -1 , max = 1)
dimt_1 = runif(tpoint, min = -1 , max = 1)

myfulldata= data.frame(dim_1)
testdata =data.frame(dimt_1)

dim= 15
colno=dim-1

#create 1000x15 dataframe
for(i in 1:colno) {                                   
  new <- runif(dpoint, min = -1 , max = 1)                      
  myfulldata[ , ncol(myfulldata) + 1] <- new                       
  colnames(myfulldata)[ncol(myfulldata)] <- paste0("dim_", i+1)    
  }

#create 100x15 test data
for(i in 1:colno) {                                   
  newtest <- runif(tpoint, min = -1 , max = 1)                    
  testdata[ , ncol(testdata) + 1] <- newtest                      
  colnames(testdata)[ncol(testdata)] <- paste0("dimt_", i+1)     
}

#show data
myfulldata[1:3,1:15]
testdata[1:3,1:15]


#calculate Eucl. distance by points and find min----


#calculate distance bw points and test instance
  
 
  avelist <- c()
  
  dist= 0
  
 
  
for (spcdim in 1:15) {
  
  distlist <- c()
  mindistlist <-c()
  
  for (t in 1: tpoint)  #for each test point
    
  {
    
  for (i in 1: dpoint) 
    
  {
    
    for (j in 1:spcdim)
      
    {
      
      dist = dist + (myfulldata[i,j]-testdata[t,j])^2
      
    } #end j
    
    distlist <- append(distlist, sqrt(dist))  # list of distances form one test instance to all the data (list size 1000)
    dist=0
    minofdist = min(distlist) #min of distances for that test point, test instances nearest neighbor distance
    
    
  } #end i
  
    mindistlist <- append(mindistlist, minofdist)
 
} #end t points
  
  avelist <- append(avelist, mean(mindistlist))
  
  
  

} #end for each dim

par(mfrow=c(1,2))
plot (c(1:15),avelist,xlim=c(1,15),main= "Avg. dist. to test instances’ nearest neighbors vs Dim", xlab = "dimension", ylab = "avg distance",cex = 1.3,col='blue')
axis(1, seq(1,15,1))

plot (c(1:15),avelist,xlim=c(1,15),main= "Avg. dist. to test instances’ nearest neighbors vs Dim", xlab = "dimension", ylab = "avg distance" ,type="l")
axis(1, seq(1,15,1))


avelist


