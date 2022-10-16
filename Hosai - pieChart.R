#Hosai Said
#46204

library("wooldridge")
library("dplyr")
library("ggplot2")

View(fertil1)
#I have used the variable Black which is Qual binary variable
View(as.data.frame(fertil1$black))

#changing 0 to "White" and 1 to "Black"
skinC=c()
for (k in 1:length(fertil1$black)) {
  if(fertil1$black[k]==0){
    skinC[k]="White"
  } else {
    skinC[k]="Black"
  }
}
skinColor=cbind(fertil1$black, skinC)
View(skinColor)

#creating an fdt before the pieChart
skinColor2=as.data.frame(skinColor)
fdtColor=table(skinColor2$skinC)
fdtColor=as.data.frame(fdtColor)
colnames(fdtColor)=c("Color","Count")
View(fdtColor)

#pieChart
g0=ggplot(fdtColor, aes(x="", y=Count, fill=Color))
g1
g1=g0+geom_col()+
  coord_polar(theta = "y")+
  theme_void()+
  theme(plot.title = element_text(colour = "#1c0f5c",
                                  size = 14, 
                                  face = "bold", 
                                  hjust = .5))+
  ggtitle('Skin Color')+
  geom_text(aes(label=Count), 
            position = position_stack(vjust = .5))+
  scale_fill_manual(values = c('#4295f4', '#6042f4'))+
  theme(legend.position = 'bottom')
g1