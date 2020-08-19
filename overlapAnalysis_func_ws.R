## Read your data as a 2-column dataset ----
dt = read.table("sample_data.txt",header=T,sep=",",stringsAsFactors = F)

dt=dt[1:100,]

## Create a matrix with all relations ----
rmat = as.matrix(table(dt$category,dt$item_code))
dim(rmat)

## This is a function to compute all overlaps and subsuming relations.
overlaps = function(ta,row=T,sub=F){ ## Takes a dichotomous table and computes the overlap
  
  ## init variables ----
  if(row==F){ta=t(ta)}
  ncas = dim(ta)[1]
  outmat = matrix(0,nrow=ncas,ncol=ncas)
  
  ilist = list()
  slist = list()
  ## loop through rmat to establish equality & subsumation ----
  for(n1 in 1:ncas){View
    ilist[[n1]]=c(NA)
    slist[[n1]]=c(NA)
    for(n2 in 1:ncas){
      if(n1!=n2){
        d = ta[n1,]-ta[n2,]
        if(min(d)==max(d)){
          outmat[n1,n2]=1 ## Equality
          t_list<- c(ilist[[n1]],n2,n1)
          ilist[[n1]]= t_list[which.min(t_list)]
        }else if(min(d)==0 & max(ta[n2,]>0)){
          outmat[n1,n2]=2 ## Subsuming
          slist[[n1]]=c(slist[[n1]],n2)
        } 
      }
    }
  }
  
  rownames(outmat)=colnames(outmat)=rownames(ta) #headers
  
  ## loop through slist to remove trivial relationships and identify equality through subsumed categories ----
  if(!sub){ ## remove indirectly subsumed categories (ie. trivial relationship A -> C in A -> B -> C)
    
    ta_agg <- matrix(0, nrow=1,ncol=dim(ta)[2]) ## setup matrix for agg rmat of each item in list
    item_list = list() ## capture item_code not overlapped in a category
    for(i in 1:length(slist)){
      
      item_list[[i]]=c(NA)
      so = c()
      for(s in slist[[i]]){
        if (!is.na(s)){ ## disregard NAs in list
          so = c(so,slist[[s]])
          ta_agg = ta_agg + ta[rownames(ta)[s],] ## agg rmat for each subsumed item
        }
      }
      
      ta_agg[ta_agg > 1] <- 1 ## replace values > 1 in agg matrix to 1
      d_s = ta[rownames(ta)[i],] - ta_agg
      catsub=F
      if(min(d_s)==max(d_s)){
        catsub = T ## boolean variable to avoid resolving checks on each subsequent for loops 
      } else {
        item_list[[i]] = colnames(ta)[d_s[1,] >= 1] ## retrieve residual item_codes not covered by subsumed categories
        ## not correct. if category is subsumed by another, then item_code is overlapped.
      }
      
      for(s in slist[[i]]){
        if (!is.na(s)){
          if(s %in% so){
            outmat[i,s]=0
          } else if(catsub) {
            outmat[i,s]=3 ## Equality via aggregate of subsumed items
          }
          
          ## loop to remove subsumed identical relationships
          if (!is.na(ilist[[s]])) {
            if (s != ilist[[s]]){
              outmat[i,s] = 0
            } 
          }
        }
      }
      
      ## if category is identical and not oldest/smallest category, remove all non identical relationship
      if(!is.na(ilist[[i]])){
        if (i != ilist[[i]]){
        outmat[i,][outmat[i,] > 1] <- 0
        }
      }
      
    }
  }
  return(outmat)
}

## Plot graph ----

library(igraph)

adjmat = overlaps(rmat,sub=F) ## Create an adjacency matrix with the option that subsuming edges are removed

g = graph_from_adjacency_matrix(adjmat, mode="directed",weighted=T) ## Create a graph
g = delete.edges(g,E(g)[E(g)$weight==0]) ## Remove all edges with weight zero (no link)

## Graph cosmetics
E(g)$arrow.size=.5
E(g)$width = 1
E(g)$color = "#30a030"
E(g)$color[E(g)$weight==3] = "#575fff"
E(g)$color[E(g)$weight==2] = "#f08080"
V(g)$color = c("#eeeeff")
##V(g)$color <- as.factor(E(g)$weight))
V(g)$frame.color=NA
V(g)$label.family="Arial"
V(g)$label.cex=.8
V(g)$shape="circle"
V(g)$size=20

chief = rownames(adjmat)[degree(g,mode="in")==0] ## Make the nodes which are not subsumed the root of the tree
l = layout_as_tree(g,root=chief) ## Construct the tree
plot(g, layout=l) ## Plot the tree
# plot(g, layout=layout_in_circle(g))