source("predicates.R")

min_span_tree = function(g)
{
  if(is_valid(g) | is_undirected(g)) 
  {
    M = matrix(nrow=length(g),ncol=length(g))    
    Vertix_Names = names(g)
    
    for(x in 1:length(g))  # Creating an adjacency matrix from graph
    {
      edges_counter = length(g[[x]]$edges) 
      for(q in 1: edges_counter) 
      {  
        y = g[[x]]$edges[q]
        M[x,y] = g[[x]]$weights[q]
      }
    }   
    
    M[is.na(M)] <- 0 # A default value '0' is placed as weight between 2 vertices, when there is no edge between them 
    
    weight <- M
    p <- array(data = 0, dim = nrow(weight), dimnames = NULL)
    visited <- array(data = 0, dim = nrow(weight), dimnames = NULL)
    d <- array(data = 32767, dim = nrow(weight), dimnames = NULL)
    
    current=1
    d[current]=0
    visited[current]=1
    total=1
    v = nrow(weight)
    
    while(total != v)
    {
      for (i in 1:v)
      {  
        if((weight[current,i] != 0) & (visited[i]!=1))
          if(weight[current,i] < d[i])
          {
            d[i]=weight[current,i]
            p[i]=current
          }
      }
      
      mincost=32767
      for (i in 1:v)
      {
        if((visited[i]!=1) & (d[i]<mincost))
        {
          mincost=d[i]
          current=i
        }
      }
      
      visited[current]=1
      total = total+1
    }
    
    mincost=0
    for(i in 1:v)
      mincost = mincost+d[i]
    
    N <- matrix(nrow=length(g),ncol=length(g))  # Adjacency Matrix for Min Spanning Tree (MST)
    for(i in 1:v) 
    {
      N[i,p[i]]=d[i]
    }
    
    N[is.na(N)] <- 0 # Replacing NA values in matrix with '0'
    
    Z <- N   # Making an undirected MST graph from a directed MST graph
    for(i in 1: nrow(Z)) 
    {  
      for(j in 1: ncol(Z)) 
      {
        if (Z[i,j] != 0) # Avoiding the undirected MST graph's non-existent edges
        {
          Z[j,i] = Z[i,j] 
        } 
      }
    } 
    
    g_output=g
    Vertix_Names = names(g)
    for(u in 1: nrow(Z)) 
    {
      count = 0
      edges_temp=NULL
      weights_temp=NULL
      for(v in 1: ncol(Z)) 
      {
        if (Z[u,v] != 0) # Avoiding the undirected MST graph's non-existent edges
        {
          edges_temp=c(edges_temp,as.integer(v))
          weights_temp=c(weights_temp,Z[u,v])
        }
        g_output[[u]]$edges=edges_temp
        g_output[[u]]$weights=weights_temp
      }
    }
    return(g_output)
 }else { 
  stop("Invalid graph") 
 }
}