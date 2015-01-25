is_valid = function(g) 
{
  if(!all( c(class(g), sapply(g, class)) == "list" )|length(g)==0){return(FALSE)}
  for(i in 1: length(g)){
    if(is.null(g[[i]]$weights)|is.null(g[[i]]$edges)){
      return(FALSE)
    }
    if(class(g[[i]]$edges)!="integer"|
         class(g[[i]]$weights)!="numeric"|
         any(is.na(g[[i]]$weights))|
         any(is.na(g[[i]]$edges))|         
         any(duplicated(g[[i]]$edges))|
         any(g[[i]]$weights<=0)|
         any(length(g[[i]]$edges)!=length(g[[i]]$weights))|
         if(length(g[[i]]$edges)>0){max(g[[i]]$edges)>length(g)}else{F}
         ){
      return(FALSE)
    }
  }
  all(!duplicated(names(g))) 
}



is_undirected = function(g)
{ 
  if(is_valid(g)) 
  {
    M = matrix(nrow=length(g),ncol=length(g))    
    for(x in 1:length(g))                   # Creating a matrix to plot graph fixture and weights 
{
  edges_counter = length(g[[x]]$edges) 
  for(q in 1: edges_counter) 
  {  
    y = g[[x]]$edges[q]
    M[x,y] = g[[x]]$weights[q]
  }
}   

M[is.na(M)] <- -999999  # A default value -999999 is placed as weight between 2 vertices, when there is no edge between them 

for(i in 1: length(g)) 
{  
  for(j in 1: length(g)) 
  {
    if (M[i,j] != -999999) # Avoiding the matrix non-existent edges
{
  if(M[i,j] != M[j,i]) # Checking whether all directed edges have a complementary directed edge with the same weight in the opposite direction
  return(FALSE) 
} 
  }
} 
return(TRUE) 
  }

else 
{ 
  stop("Invalid graph") 
}
} 



is_isomorphic = function(g1, g2)
{
  stopifnot(is_valid(g1)&is_valid(g2))
  n1=sort(names(g1))
  n2=sort(names(g2))
  if (any(n1!=n2)){return(FALSE)}
  for (i in 1:length(g1)){
    v1=sort(names(g1)[g1[[i]]$edges])
    w1=g1[[i]]$weights
    j=match(names(g1)[i],names(g2))
    v2=sort(names(g2)[g2[[j]]$edges])
    w2=g2[[j]]$weights
    if(any(sapply(list(v1,v2,w1,w2),length)==0)){
      return(all(sapply(list(v1,v2,w1,w2),length)==0))
    }else if (any(!(v1%in%v2))|any(!(v2%in%v1))|any(!(w1%in%w2))|any(!(w1%in%w2)))
    {return(FALSE)
    }
  }
  return(TRUE)
}


is_connected = function(g, v1, v2)
{
  if(is.list(g)){
    if(is_valid(g)){
      match=c(names(g),seq(1,length(g),by=1))
      if(v1 %in% match & v2 %in% match){
        if (length(c(g[[v1]]$edges)) == 0){return(FALSE)}
        else {
          if (is.character(v1)){
            v1=match(v1,names(g))
          }
          path=c(g[[v1]]$edges)
          name=NULL
          for (i in 1:length(g)){
            name=c(name,names(g)[path[i]])
            path=c(path,g[[path[i]]]$edges) 
          }
          if(v2 %in% path|v2 %in% name){return(TRUE)} 
        }
        return(FALSE)
      }
      else {stop("error")}
    }
    stop("error")
  }
  stop("error")
}

