#final project ITC 22 
# Instructor: Asadullad Jawid ------------ Hosai Said

# To initiate this project we have three variables. 
#1. whether or not a customer makes a purchase, Y~Bin with p=0.30
#2. Whether the customer is married, X1~Bin with p=0.2
#3. Customers age with X2~N(31,5)

### Customer age = Mean: 31 and STD: 5 

# Using rbinom and rnorm, we generate random data and utilize it as a sample of our population:
set.seed(123)
customer_making_purchase = rbinom (1000,1, prob = 0.30)
Customer_gender = rbinom (1000,1, prob = 0.2)
age=rnorm(1000,31,5)
p= 0.30 
## Now we have denoted Customer making Purchase as Y for easing the work.

 y= sample(c("yes", "no"), size= 1000, replace = TRUE, prob = c(p, 1-p)) 
 head(y)
#### for variable X1 whether or not a married we are simplifying the term as X1
 p1= 0.2
 X1= sample(c("Married", "single"), size= 1000, replace = TRUE, prob = c(p1, 1-p1))
 head(X1)
 ## for variable X2 age we are simplifying the term as X2 we also used rnrom
 X2= abs( round(rnorm(1000,31,5),0))
 


#graph

#visualizing whether or not a customer makes a purchase, Y~Bin with p=0.30 (y)

fdtP= table(y)
fdtP= as.data.frame(fdtP)
head(fdtP)
colnames(fdtP)= c("Purchase","Count")

# creating a pie chart for this distribution

library(ggplot2)

g0=ggplot(fdtP, aes(x="", y=Count, fill=Purchase))
g0+geom_col()+
  coord_polar(theta = "y")+
  theme_void()+
  theme(plot.title = element_text(colour = "#1c0f5c",
                                  size = 14, 
                                  face = "bold", 
                                  hjust = .5))+
  ggtitle('Customers Making Purchase')+
  geom_text(aes(label=Count), 
            position = position_stack(vjust = .5))+
  scale_fill_manual(values = c('#F52413', '#EEAD0C'))+
  theme(legend.position = 'bottom')

ggsave("Customermakingpurchase.png")

getwd()


#Visualizing whether or not a customer married, X1~Bin with p=0.2

fdtM= table(X1)
fdtM= as.data.frame(fdtM)
head(fdtM)
colnames(fdtM)= c("Mstatus","Count")

# making piechart for this distribution

g1=ggplot(fdtM, aes(x="", y=Count, fill=Mstatus))
g1+geom_col()+
  coord_polar(theta = "y")+
  theme_void()+
  theme(plot.title = element_text(colour = "#1c0f5c",
                                  size = 14, 
                                  face = "bold", 
                                  hjust = .5))+
  ggtitle('Marriage Status')+
  geom_text(aes(label=Count), 
            position = position_stack(vjust = .5))+
  scale_fill_manual(values = c('#EEDD0C', '#0CEBEE'))+
  theme(legend.position = 'bottom')

ggsave("Customermarriagestatus.png")

getwd()


#Visualizing customer age with X2~N(31,5)

fdtA= as.data.frame(X2)
head(fdtA)
colnames(fdtA)= c("Age")

## now we are making a density plot for this distribution

g2=ggplot(fdtA, aes(x=Age))
g2+geom_density()+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Age Distribution')+
  xlab('Age')+
  ylab('density')
  
ggsave('agedis.png')
### now we are finding the mean and median of the distribution.
####the mean and median is  aaprox eqaul which shows that is a normal distribution. 
median(X2)
mean(X2)
#### Now we are finding the regression of the distributions. 
### the given distribution is multiple regression.
 
Buying status of customer
ycode=ifelse(y=="yes",1,0)
ycode

# marriage status of customer 
mcode=ifelse(X1== "Married",1,0)
mcode 
X1

multireg=lm(ycode~mcode+X2)
summary(multireg)
##


