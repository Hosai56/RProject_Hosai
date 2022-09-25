






############### FDT Assignment

######################uploading the dataset

DsIris = read.csv(url('https://gist.githubusercontent.com/curran/a08a1080b88344b0c8a7/raw/0e7a9b0a5d22642a06d3d5b9bcbad9890c8ee534/iris.csv'))
names(DsIris)

#######################Sepal_Width FDT 

FDTis = function(h){
  absloutFrequency = table(h)
  relativeFrequency = round(prop.table(absloutFrequency)*100,2)
  cumlativeFrequency = cumsum(relativeFrequency)
  
  IrisFDT= cbind(absloutFrequency,relativeFrequency,cumlativeFrequency)
  
  
  return(IrisFDT)
}
# Viewing

FDTis(DsIris$sepal_width)


#numerical variable

NewCat = c ()
summary(DsIris)

###########Categorizing

for(h in 1:length(DsIris$sepal_width) ) {
  if(DsIris$sepal_width[h]<=2){
    NewCat[h]="Small"
  }else if(DsIris$sepal_width[h]>2 &  DsIris$sepal_width[h]<=3){
    NewCat[a]="medium "
  }else {
    NewCat[a]="large"
  }
}

NewVar = cbind(DsIris$petal_length,NewCat)
head(DsIris)
View(NewVar)
FDTis(NewCat)
for(h in 1:length(DsIris$petal_length) ) {
  if(DsIris$sepal_width[h]<=2){
    NewCat[h]="Small"
  }else if(DsIris$sepal_width[h]>2 &  DsIris$sepal_width[h]<=3){
    NewCat[h]="medium "
  }else {
    NewCat[h]="large"
  }
}

newVar = cbind(DsIris$petal_length,newCat)
head(DsIris)
View(NewVar)
FDTis(NewCat)




























