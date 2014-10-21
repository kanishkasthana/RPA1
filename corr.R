corr <- function(directory,threshold=0)
{
    appendfunction<- function(x)
    {
        if(x>=1 && x<=9)
        {
            return(paste("00",x,sep=""))    
        }
        
        else if(x>=10 && x<=99)
        {
            return(paste("0",x,sep=""))
        }
        
        else
        {
            return(paste("",x ,sep=""))
        }
        
        return(Na)
    } 
    
    new_id=lapply(1:332,appendfunction)
    filenames=paste(paste(getwd(),directory,new_id,sep="/"),".csv",sep="")
    
    # Cleans and returns a vector with no na values for a given pollutant
    getCleanData<- function(data,threshold){
        new_data=data[complete.cases(data),]
        pollutant_data=new_data[c('sulfate','nitrate')]
        if(nrow(pollutant_data)>threshold){
            cor(unlist(pollutant_data['sulfate']),unlist(pollutant_data['nitrate']))
        }
    }
    
    all_data=lapply(filenames,read.csv)
    clean_data=sapply(all_data,getCleanData,threshold)
    clean_data=clean_data[!is.na(clean_data)]
    unlist(clean_data)
        
}
