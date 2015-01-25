source("check_packages.R")
check_packages(c("httr","XML"))


N=51
states=c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
         "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
         "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
         "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

#transform XML to data frame
data=NULL
for(i in 1:N){
  xml_data<-xmlTreeParse(paste("dennys/",states[i],".html",sep=""))
  xmltop = xmlRoot(xml_data)
  xmlfile <- xmlSApply(xmltop[[1]], function(x) xmlSApply(x, xmlValue))
  tempdata<-data.frame(t(xmlfile),row.names=NULL)
  data<-rbind(data,tempdata)
}

l=length(data[,1])

#test whether address2 is necassory
for(j in 1:l){
  if(length(unlist(data$address2[j]))!=0){
    print("address is not always empty")
  }
}


#select variables we actually need
temp_data<-data[,c(3,5,12,13,17,18,21,24)]

dennys=as.data.frame(matrix(NA,nrow=l,ncol=8))

#There are some Denny's without phone number
for(i in 1:8)
{
  for(j in 1:l)
  {
    if(length(unlist(temp_data[j,i]))==0){
      dennys[j,i]="N/A"
    }else{
      dennys[j,i]=unlist(temp_data[j,i])
    }
  }  
}

#add column names
colnames(dennys)=c("address","city","latitude","longitude","phone","zip","state","uid")


dennys$latitude=as.numeric(dennys$latitude)
dennys$longitude=as.numeric(dennys$longitude)

#save file
save(dennys, file="dennys/dennys_data.Rdata")

