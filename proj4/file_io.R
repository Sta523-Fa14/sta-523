source("predicates.R")

write_graph = function(g, file){
  if(is_valid(g)){
    stopifnot(!file.exists(file))
    stopifnot(file.exists(dirname(file)))
    txt=NULL
    weights=NULL
    for (i in 1:length(g)){
      from=names(g)[i]
      if(grepl(" ", from)){
        from=paste('"',from,'"',sep="")
      }
      if(length(g[[i]]$edges)>0){
        for (j in 1:length(g[[i]]$edges)){
          to=names(g)[g[[i]]$edges[j]]
          if(grepl(" ", to)){
            to=paste('"',to,'"',sep="")
          }
          txt=paste(txt,from,sep="")
          txt=paste(txt,"->",to)
          if(length(g[[i]]$weights[j])>0){
            w=g[[i]]$weights[j]
            weights=paste("[weight=",w,"]",sep="")
            txt=paste(txt,weights)
            txt=paste(txt,";","\n",sep="")
          } 
          else{txt=paste(txt,";","\n",sep="")}
        }
      }
      else{txt=paste(txt,from,";","\n",sep="")}
    }
    writeLines(txt, file)
  }
  else{stop("error")}
}


read_graph = function(file)
{
  library(stringr)
  stopifnot(file.exists(file))
  g0=readLines(file)
  graph0=unlist(str_match_all(g0, "^(?:[A-Z]+)?(?:\"(?:.*)\")?(?:\\ ->\\ (?:[A-Z]+)?(?:\"(?:.*)\")?)?(?:\\ ->\\ (?:[A-Z]+)?(?:\"(?:.*)\")?\\ \\[weight=[0-9]+\\.?[0-9]*(?:[Ee]\\ *-?\\ *[0-9]+)?\\])?;"))
  if(length(graph0)==0){stop("Bad graph file")}
  graph=as.matrix(read.table(file, fill=TRUE),ncol=4)
  graph=str_replace_all(graph,";","")
  graph=str_replace_all(graph,"\\[weight=","")
  graph=str_replace_all(graph,"\\]","")
  if(dim(graph)[2]<4){
    graph=t(apply(graph,1,function(x){length(x)=4;x}))
    graph[is.na(graph)]=""
  }
  
  vertex=c(graph[,1],graph[,3])
  vertex=unique(sort(vertex[vertex!=""]))
  
  g=list()
  for (v in vertex){
    g[[v]]=list(edges=integer(),weights=numeric())
  }
  
  for (i in 1:dim(graph)[1]){
    edge=integer()
    weight=numeric()
    index=match(graph[i,1],vertex)
    if(graph[i,3]!=""){
      edge=match(graph[i,3],vertex)
    }
    g[[index]]$edges=c(g[[index]]$edges,edge)
    if(graph[i,4]!=""){
      weight=as.numeric(graph[i,4])
    }else if(graph[i,3]!=""&graph[i,4]==""){
      weight=1
    }
    g[[index]]$weights=c(g[[index]]$weights,weight)
  }      
  return(g)
}