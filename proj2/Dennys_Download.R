source("check_packages.R")
check_packages(c("httr","XML","stringr","plyr","rvest"))


dir.create("dennys/", showWarnings = FALSE)

states=c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
         "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
         "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
         "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY") 

#Record all the 50 states and DC so we can insert the states' names to look up for the data.
#set all the characters we need to rename the files.
file.names=NULL
for(i in 1:length(states))
{
  file.names=cbind(file.names,paste("dennys/",states[i],".html",sep=""))
}

N=51
for (i in 1:N){
  url=paste("http://hosted.where2getit.com/dennys/2014/ajax?&xml_request=%3Crequest%3E%3Cappkey%3E8D6F0428-F3A9-11DD-8BF2-659237ABAA09%3C%2Fappkey%3E%3Cgeoip%3E1%3C%2Fgeoip%3E%3Cformdata+id%3D%22getlist%22%3E%3Corder%3Ecity%3C%2Forder%3E%3Cobjectname%3EStoreLocator%3C%2Fobjectname%3E%3Cwhere%3E%3Cuid%3E%3Ceq%3E%3C%2Feq%3E%3C%2Fuid%3E%3Cstate%3E%3Ceq%3E",states[i],"%3C%2Feq%3E%3C%2Fstate%3E%3C%2Fwhere%3E%3C%2Fformdata%3E%3C%2Frequest%3E",sep="")
  write(content(GET(url), as="text"), file="dennys/denny.html")
  file.rename("dennys/denny.html",file.names[i])
}
