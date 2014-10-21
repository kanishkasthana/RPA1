complete <- function(directory,id=1:332)
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
    
    new_id=lapply(id,appendfunction)
    filenames=paste(paste(getwd(),directory,new_id,sep="/"),".csv",sep="")
    
    # Cleans and returns a vector with no na values for a given pollutant
    getCleanData<- function(data){
        return(data[complete.cases(data),])
    }
    
    all_data=lapply(filenames,read.csv)
    clean_data=lapply(all_data,getCleanData)
    nobs=sapply(clean_data,nrow)
    output=data.frame(id,nobs)

}
