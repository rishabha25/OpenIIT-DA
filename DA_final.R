#Final Code

#Please put csv files into the working directory.


#Data pre-processing
#Read all the files there are in the directory
slope<-function(p)
{
        
        x=p[,3]
        len=length(x)
        minm=11
        maxm=11
        y<-numeric()
        l<-numeric()
        for (i in 1:(len-10)) 
        {
                
                y[i]<-(x[i+10]-x[1])/(i+10-1)
                
        }
        
        maxslope<-max(y)
        for (i in 1:(len-10)) 
        {
                if (y[i]<=(maxslope-4.8))
                {
                        minm<-i+10
                        break
                }
        }
        
        
        for (i in 1:(len- 10)) 
        {
                
                y[i]<-(x[len-9-i]-x[len])/(i+9)
                
        }
        maxm<-which.max(y)
        maxm<-len-maxm-9
        
        lis=list(minm,maxm)
        z=data.frame(x1=numeric(),x2=numeric())
        for (i in minm:maxm)
        {
                z=rbind(z,p[i,])
                
                
        }
        return( c(mean(z$X0),mean(z$X0.1)) )
}

temp=list.files(pattern="*.csv")
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

#create an empty matrix to concatenate all the data into
df=matrix(data=NA,nrow=0,ncol=4)
colnames(df)=c("drillcol","holecol","X0","X0.1")

#iterate over the entire set and combine them into a single dataframe
for (i in 1:length(temp)){
        rowlength=length(dim(eval(as.name(temp[i])))[1])
        drillname=strsplit(strsplit(strsplit(temp[i],"\\.")[[1]][1],'h')[[1]][1],'d')[[1]][2]
        holename=strsplit(strsplit(temp[i],"\\.")[[1]][1],'h')[[1]][2]
        drillcol=matrix(data=as.numeric(drillname),nrow=rowlength,ncol=1)
        holecol=matrix(data=as.numeric(holename),nrow=rowlength,ncol=1)
        df.temp=cbind(drillcol,holecol,eval(as.name(temp[i])))
        df=rbind(df,df.temp)
}

#create final dataframe to train the model upon
df.final=matrix(data=NA,nrow=0,ncol=6)
colnames(df.final)=c("Drill.Number","Hole.present","Thrust.Mean","Torque.Mean","Row.Number","RUL")

#iterating over the df to create the final data set
maxdrill=max(as.numeric(df$drillcol))
mindrill=min(as.numeric(df$drillcol))
for (i in mindrill:maxdrill){
        df.drill=subset(df,df$drillcol==i)
        maxhole=max(as.numeric(df.drill$holecol))
        for (j in 1:maxhole){
                # Each iteration will generate a row for the data set
                df.temp=subset(df.drill,df.drill$holecol==j)
                Row.Number.temp=dim(df.temp)[1]
                Drill.Number=i
                Hole.Present.temp=j
                RUL.temp=maxhole-j
                Mean.temp=slope(na.omit(df.temp))
                Thrust.Mean.temp=Mean.temp[1]
                Torque.Mean.temp=Mean.temp[2]
                row.temp=t(matrix(data=c("Drill.Number"=Drill.Number,
                                         "Hole.present"=Hole.Present.temp,
                                         "Thrust.Mean"=Thrust.Mean.temp,
                                         "Torque.Mean"=Torque.Mean.temp,
                                         "Row.Number"=Row.Number.temp,
                                         "RUL"=RUL.temp)))
                df.final=rbind(df.final,row.temp)
        }
}
original=as.data.frame(df.final)

#Add new variables to original
original$sqrt.thrust.mean=((original$Thrust.Mean)^0.5)
original$sqrt.torque.mean=(original$Torque.Mean)^0.5
original$percent=original$Hole.present/(original$RUL+original$Hole.present)

#Load our model
load(file="RUL_model.RDA")

#predict percent using our model
result=predict(model1,newdata=original)

#Get RUL from percent
RUL=(original$Hole.present/result)-original$Hole.present

#make negative values zero
# source(negative_zeros.R)
# RUL=negative_zeros(RUL)
for(i in 1:length(RUL))
{
        if(RUL[i]<0)
        {
                RUL[i]=0
        }
}
#Round off RUL
RUL=round(RUL,digits=0)

#error
error=sum(abs(RUL-original$RUL))
error=error/(nrow(original))
error

#Final prediction  
final_sheet=cbind(original$RUL,RUL)
final_sheet=as.data.frame(final_sheet)
names(final_sheet)=c("Actual_RUL","Predicted_RUL")
write.csv(final_sheet,"RUL_prediction.csv")


