pollutantmean <- function(directory,pollutant,id=1:332)
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
    getPollutantData<- function(data,pollutant){
         return(na.omit(data[pollutant]))
    }    
    
    all_data=lapply(filenames,read.csv)
    pollutant_data=lapply(all_data,getPollutantData,pollutant)
    mean((unlist(pollutant_data)))
}