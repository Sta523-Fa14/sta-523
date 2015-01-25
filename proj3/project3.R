

source("check_packages.R")

check_packages(c("devtools","dplyr","data.table","lubridate","bit64","stringr","rgdal","rgeos","geosphere","sp","raster","e1071","sqldf"))

base = '/home/vis/cr173/Sta523/data/parking'

park = tbl_df(fread(paste0(base,"/NYParkingViolations.csv"),stringsAsFactors=FALSE))


#eliminate space in column name
colnames(park)<-str_replace_all(colnames(park)," ",".")
#change the format of Issue.Date
park$Issue.Date = mdy(park$Issue.Date)
#set the beginning date and ending date
start = mdy("8/1/2013")
end = mdy("6/30/2014")
#catch the address corresponding to a Violation.Precinct interval and a Issue.Date interval.

#That's all address we need.
addr = filter(park[,-1], (Violation.Precinct %in% c(1,5,6,7,9,10,13,14,17,18,19,20,22,23,24,25,26,28,30,32,33,34) & Issue.Date  >= start & Issue.Date <= end)) %>%
  mutate(House.Number = str_trim(House.Number), Street.Name = str_trim(Street.Name)) %>%
  filter(House.Number != "" & Street.Name != "") %>%
  filter(str_detect(House.Number,"[0-9]+")) %>%
  transmute(Violation.Precinct = Violation.Precinct, addr = paste(House.Number, Street.Name)) %>%
  mutate(addr = tolower(addr))



pl = readOGR(paste0(base,"/pluto/Manhattan/"),"MNMapPLUTO")

pt = gCentroid(pl,byid=TRUE)

tax = cbind(data.frame(pt@coords), tolower(as.character(pl@data$Address)))
colnames(tax)=c("longitude","latitude","addr")



tax$addr<-str_replace_all(tax$addr,"street","st")
tax$addr<-str_replace_all(tax$addr,"west","w")
tax$addr<-str_replace_all(tax$addr,"east","e")
tax$addr<-str_replace_all(tax$addr,"south","s")
tax$addr<-str_replace_all(tax$addr,"north","n")
tax$addr<-str_replace_all(tax$addr,"avenue","ave")
tax$addr<-str_replace_all(tax$addr,"lane","ln")
tax$addr<-str_replace_all(tax$addr, "place", "pl")
tax$addr<-str_replace_all(tax$addr, "drive", "dr")

addr$addr<-str_replace_all(addr$addr, "th ", " ")
addr$addr<-str_replace_all(addr$addr, "3rd ", "3 ")
addr$addr<-str_replace_all(addr$addr, "2nd ", "2 ")
addr$addr<-str_replace_all(addr$addr, "1st ", "1 ")
addr$addr<-str_replace_all(addr$addr,"avenue","ave")
tax$addr<-gsub(" bl"," blv",tax$addr,fixed=T)


# data after merge park addr and pluto
data = unique(inner_join(addr, tax))
colnames(data)=c("addr","Precinct","Longitude","Latitude")


#Since the data size is too large, we subset the data randomly
set.seed (7)
n = nrow(data)
n.sub=round(0.15*n)
data_sub=data[sample(1:n, n.sub),2:4]



colnames(data_sub)=c("Precinct","x","y")

data_sub=data.frame(data_sub)
data_sub$Precinct = as.integer(as.character(data_sub$Precinct))
data_sub=na.omit(data_sub)

svm_data=svm(as.factor(Precinct)~.,data=data_sub,cross=10)



ny = readOGR(paste0(base,"/nybb/"),"nybb")
manh=ny[ny@data$BoroName=="Manhattan",]
r = rasterize(manh, raster(ncols=500,nrows=1000,ext=extent(bbox(manh))))

cells = which(!is.na(r[]))
crds = xyFromCell(r,cells)

pred = predict(svm_data,crds)

r[cells] = as.numeric(as.character(pred))

dist = sort(unique(data_sub$Precinct))

index=which(!(dist %in% r[]))
dist=dist[-index]

l=list()
for(i in seq_along(dist))
{
  l[[i]] = rasterToPolygons(r, function(x) x==dist[i], dissolve=TRUE)
  l[[i]]@polygons[[1]]@ID = as.character(dist[i])
  rownames(l[[i]]@data) = dist[i]
  colnames(l[[i]]@data) = "Precinct"
}

pd = do.call(rbind, l)



writeGeoJSON = function(sp, file)
{
  stopifnot(class(sp) == "SpatialPolygonsDataFrame")
  stopifnot(!missing(sp))
  
  sp = createSPComment(sp)
  
  poly_json = function(x)
  {
    owners = as.integer(str_split(comment(x)," ")[[1]])
    
    paste("[",
          paste(
            sapply(which(owners == 0), function(i)
            {
              res = "[ ["
              
              res = paste(res, paste("[", apply(x@Polygons[[i]]@coords, 1, paste, collapse=", "), "]", collapse=", "))
              
              for (j in which(i %in% owners))
              { 
                res = paste(res, "], [")
                
                res = paste(res, paste("[", apply(x@Polygons[[j]]@coords, 1, paste, collapse=", "), "]", collapse=", "))
              }
              
              res = paste(res, "] ]")
            }),
            collapse = ", "
          ),
          "]")
  }
  qt = function(x) paste0('"',x,'"')
  
  res = paste('{',
              '"type": "FeatureCollection",',
              '"features": [',
              paste(
                sapply(1:nrow(sp), function(i)
                {
                  paste('{ "type": "Feature",',
                        '"properties": { ',
                        paste(qt(names(sp)), sp@data[i,], sep=": ", collapse=", "),
                        ' },',
                        '"geometry": {',
                        '    "type": "MultiPolygon",',
                        '    "coordinates": ',
                        poly_json(sp@polygons[[i]]),
                        '} }',
                        sep="\n")
                }),
                collapse=",\n"
              ),
              '] }')
  
  cat(length(res),"\n\n")
  
  write(res, file = file)
}


writeGeoJSON(pd,"./precinct.json")





