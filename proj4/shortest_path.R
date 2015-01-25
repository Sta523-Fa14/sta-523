source("predicates.R")
shortest_path = function(g, v1, v2)
{
  stopifnot(is_valid(g))
  sp=NULL
  
  traverse=function(v,vst=character())
  {
    vst_tmp=c(vst, v)
    linki=sapply(g,function(x) any(names(g)[x$edges]==v)) & !(names(g) %in% vst)
    if(v==v1){
      dis_min=0
      sp_tmp=NULL
    }else if(any(linki)){
      tmp=g[linki]
      dis=sapply(names(tmp),function(z) traverse(z,vst_tmp)$dis_min)+
        sapply(tmp,function(y) y$weights[names(g)[y$edges]==v])
      dis_min=min(dis)
      min_node=names(tmp)[which.min(dis)]
      sp_tmp=c(traverse(min_node,vst_tmp)$sp,min_node)
    }else{
      dis_min=Inf
      sp_tmp=NULL
    }
    return(list(dis_min=dis_min,sp=sp_tmp))
  }
  
  if(is.null(v1)|is.null(v2)){
    return(sp)
  }
  
  if(class(v1) %in% c("integer","numeric")){
    v1=names(g)[v1]
  }
  if(class(v2) %in% c("integer","numeric")){
    v2=names(g)[v2]
  }
  
  stopifnot(any(names(g)==v1))
  stopifnot(any(names(g)==v2))
  if(any(names(g)[g[[v1]]$edges]==v2)){
    return(c(v1,v2))
  }else if(is.null(traverse(v2)$sp)){
    return(NULL)
  }else if(traverse(v2)$sp[1]==v1){
    return(c(traverse(v2)$sp,v2))
  }else{
    return(sp)
  }
}

