# Libraries
library(ggraph)
library(igraph)
#library(tidyverse)

### MEG with 68 node plot###########################################################################

setwd("U:/Documents/ACE_Data/Thesis_Analysis/brain_visualisation/ordinary_order")
connect <- connect_unscale <- connect_filter <- NULL

#Change these 
type <- 'Y' #Where is brain data? 
brain_type <- 'MEG' #'MRI or MEG
load_ext <- 'load' # What are the loadings called? E.g. load or loadings_load
ncomp <- 1
perc<- 0.25 #Percentage of connections to keep in thresholded version
if(brain_type == 'MEG'){
  datadir <- "U:/Documents/ACE_Data/Thesis_Analysis/MI_PLS_MEG/results_non_sparse"
  node_vals <- c(68,85) #68 or 85 (do both for MEG)
  connect_file <- 'connections_68.csv' 
  A <- 'Questions' #'Connection.strength' #'s_beta_nets_LASSO_envPartialCorrelationRegularized'
  B <- 'beta_nets_LASSO_envPartialCorrelationRegularized_z'
  C <- '' #if there is a C block add '_' to end of the name
}
if(brain_type == 'MRI'){
  datadir <- 'U:/Documents/ACE_Data/Thesis_Analysis/MI_PLS_MRI/results_non_sparse/Questions-MRI-Outcomes' 
  node_vals <- 85 
  connect_file <- 'connections_85.csv'
  A <- 'Questions'
  B <- 'Connection.strength'
  C <- 'Academic_' #if there is a C block add '_' to end of the name
}
for(comp in 1:ncomp){
  for(nodes in node_vals){
    connect <- read.csv(connect_file) #always connections_68.csv for MEG
    if(nodes==68){cols <- c('#FFFFFF','#53B400','#00B6EB','#FB61D7','#E38900','#2ff0f0','#A58AFF','#FFFFFF','#FFFFFF')}
    if(nodes==85){cols <- c('#FFFFFF','#53B400','#00B6EB','#FB61D7','#E38900','#2ff0f0','#A58AFF','#3366cc','#3366cc')}
    lobe_labs <- c('Origin', 'Frontal','Parietal','Temporal','Occipital','Cingulate','Insula','Subcortical', 'Subcortical')
    
    
    names <- paste(A, '_', B, '_', C, sep='')
    fname <- paste(datadir,'/', names, comp, '_boot_', type,'_', load_ext, '.csv', sep='' )
    X_load <- read.csv(fname)
    #Get avergae group correlations 
    connect$value <- X_load$loading*X_load$reliable
    connect <- connect[connect$value!=0,]
    connect[connect$value==0]<- NA
    connect_unscale <- connect
    mid_range01 <- function(x){(-min(x))/(max(x)-min(x))}
    mid_point <- mid_range01(connect$value)
    range01 <- function(x){(x-min(x))/(max(x)-min(x))}
    connect$value <- range01(connect$value)
    #Get edges and vertices
    edges <- read.csv(paste('edges_', as.character(nodes), '_split_hemi_extra_space.csv', sep='')) 
    edges <- edges[order(edges$order),]
    
    vertices = data.frame(
      name = unique(c(as.character(edges$from), as.character(edges$to))) 
    ) 
    vertices$group = edges$from[ match( vertices$name, edges$to ) ]
    
    vertices$id=NA
    myleaves=which(is.na( match(vertices$name, edges$from) ))
    nleaves=length(myleaves)
    #vertices <- vertices[order(vertices$group),]
    vertices$id[ myleaves ] = seq(1:nleaves)
    vertices$angle= 90 - 360 * vertices$id / nleaves
    # calculate the alignment of labels: right or left
    # If I am on the left part of the plot, my labels have currently an angle < -90
    vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
    # flip angle BY to make them readable
    vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
    #Remove dummy
    new_names <- vertices$name
    new_names[grepl('dum.', new_names)]<- NA
    #Set node colours
    node_col <- as.factor(c(0,edges$node.col))
    #col_pal <-distinctColorPalette(k = length(unique(node_col))-1)
    
    
    # Create a graph object
    mygraph <- graph_from_data_frame( edges[,2:3], vertices=vertices )
    
    ###Plot all connections ###
    connect_pos <- connect[which(connect_unscale$value>=0),]
    if(nrow(connect_pos)==0){
      connect_pos <- connect[1,]
      connect_pos$from <- 'origin'
      connect_pos$to <- 'origin'
      connect_pos$value <- mid_point
    }
    from_pos = match( connect_pos$from, vertices$name)
    to_pos = match( connect_pos$to, vertices$name)
    connect_neg <- connect[which(-1*connect_unscale$value>=0),]
    if(nrow(connect_neg)==0){
      connect_neg <- connect[1,]
      connect_neg$from <- 'origin'
      connect_neg$to <- 'origin'
      connect_neg$value <- mid_point
    }
    from_neg = match( connect_neg$from, vertices$name)
    to_neg = match( connect_neg$to, vertices$name)
    
    fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes.pdf', sep='' )
    pdf(fname)
    print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
      geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
      #scale_colour_manual(values=c('#FFFFFF',col_pal))+
      scale_colour_manual(values=cols, labels=lobe_labs)+
      geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=as.numeric(connect_pos$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
      scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
      geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=as.numeric(connect_neg$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
      scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
      geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
      theme_void() +
      theme(
        legend.position="right",
        plot.margin=unit(c(0,0,0,0),"cm") ) +
      expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
    })
    dev.off()
    
    ### top percentage of connections ###
    connect_thres <- abs(connect_unscale[order(abs(connect_unscale$value), decreasing = TRUE),][perc*nrow(connect_unscale),]$value) 
    if(length(connect_thres)==0){
      connect_thres <- max(connect_unscale$value)-0.01#Or set manually
    }
    #connect_thres <- 0.06#Or set manually
    connect_orig <- connect[1,]
    connect_orig$from <- 'origin'
    connect_orig$to <- 'origin'
    connect_orig$value <- mid_point
    connect_pos <- connect[which(connect_unscale$value>=connect_thres),]
    if(nrow(connect_pos)>0){
      connect_pos <- rbind(connect_orig,connect_pos)
    }
    if(nrow(connect_pos)==0){
      connect_pos <- connect[1,]
      connect_pos$from <- 'origin'
      connect_pos$to <- 'origin'
      connect_pos$value <- mid_point
    }
    from_pos = match( connect_pos$from, vertices$name)
    to_pos = match( connect_pos$to, vertices$name)
    connect_neg <- connect[which(-1*connect_unscale$value>=connect_thres),]
    if(nrow(connect_neg)>0){
      connect_neg <- rbind(connect_orig,connect_neg)
    }
    if(nrow(connect_neg)==0){
      connect_neg <- connect[1,]
      connect_neg$from <- 'origin'
      connect_neg$to <- 'origin'
      connect_neg$value <- mid_point
    }
    from_neg = match( connect_neg$from, vertices$name)
    to_neg = match( connect_neg$to, vertices$name)
    
    fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_load_val.pdf', sep='' )
    pdf(fname)
    print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
      geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
      #scale_colour_manual(values=c('#FFFFFF',col_pal))+
      scale_colour_manual(values=cols, labels=lobe_labs)+
      geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=as.numeric(connect_pos$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
      scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
      geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=as.numeric(connect_neg$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
      scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
      geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
      theme_void() +
      theme(
        legend.position="none",
        plot.margin=unit(c(0,0,0,0),"cm") ) +
      expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
    })
    dev.off()
    
    ### Postive only ####
    fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_postive_load_val.pdf', sep='' )
    pdf(fname)
    print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
      geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
      #scale_colour_manual(values=c('#FFFFFF',col_pal))+
      scale_colour_manual(values=cols, labels=lobe_labs)+
      geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=connect_pos$value), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
      scale_edge_colour_gradient2( mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
      geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
      theme_void() +
      theme(
        legend.position="none",
        plot.margin=unit(c(0,0,0,0),"cm") ) +
      expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
    })
    dev.off()
    
    ### Negative only ###
    fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_negative_load_val.pdf', sep='' )
    pdf(fname)
    print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
      geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
      #scale_colour_manual(values=c('#FFFFFF',col_pal))+
      scale_colour_manual(values=cols, labels=lobe_labs)+
      geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=connect_neg$value), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
      scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', midpoint=mid_point,na.value ="grey")+
      geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
      theme_void() +
      theme(
        legend.position="none",
        plot.margin=unit(c(0,0,0,0),"cm") ) +
      expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
    })
    dev.off()

  }
}


#################### MEG multiplied by average correlation strength across participants ############

setwd("U:/Documents/ACE_Data/Thesis_Analysis/brain_visualisation/ordinary_order")

#Get connection strength
connect <- connect_unscale <- connect_filter <- NULL
connect <- read.csv('connections_68.csv')
datadir <- "U:/Documents/ACE_Data/Thesis_Analysis/MI_PLS_MEG/results_sparse/over_5min"
type <- 'X' #Where is brain data? 
comp <- 1

B <- 'Questions'
A <- 's_beta_nets_LASSO_envPartialCorrelationRegularized'
names <- paste(A, '_', B, '_', sep='')

#C <- 'Behaviour'
#names <- paste(A, '_', B, '_', C, '_', sep='')

fname <- paste(datadir,'/', names, comp, '_boot_', type,'_', load_ext, '.csv', sep='' )


X_load <- read.csv(fname)
#Get avergae group correlations 
av_nets<- read.csv("U:/Documents/ACE_Data/MI_FA/MI_PLS_MEG/beta_nets_LASSO/envPartialCorrelationRegularized_z_group_av_sample_used.csv")
connect$value <- sign(X_load$loading)*X_load$reliable*av_nets[,2]
connect <- connect[connect$value!=0,]
connect[connect$value==0]<- NA
connect_unscale <- connect
mid_range01 <- function(x){(-min(x))/(max(x)-min(x))}
mid_point <- mid_range01(connect$value)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
connect$value <- range01(connect$value)
#Get edges and vertices
edges <- read.csv(paste('edges_',  as.character(nodes), '_split_hemi_extra_space.csv', sep='') ) 
edges <- edges[order(edges$order),]

vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) 
) 
vertices$group = edges$from[ match( vertices$name, edges$to ) ]

vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
#vertices <- vertices[order(vertices$group),]
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
#Remove dummy
new_names <- vertices$name
new_names[grepl('dum.', new_names)]<- NA
#Set node colours
node_col <- as.factor(c(0,edges$node.col))
#col_pal <-distinctColorPalette(k = length(unique(node_col))-1)


# Create a graph object
mygraph <- graph_from_data_frame( edges[,2:3], vertices=vertices )

###Plot all connections ###
connect_pos <- connect[which(connect_unscale$value>=0),]
from_pos = match( connect_pos$from, vertices$name)
to_pos = match( connect_pos$to, vertices$name)
connect_neg <- connect[which(-1*connect_unscale$value>=0),]
from_neg = match( connect_neg$from, vertices$name)
to_neg = match( connect_neg$to, vertices$name)

fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=as.numeric(connect_pos$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=as.numeric(connect_neg$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
  
})
dev.off()

### top percentage of connections ###
perc<- 0.05 #Percentage of connections to keep
connect_thres <- abs(connect_unscale[order(abs(connect_unscale$value), decreasing = TRUE),][perc*nrow(connect_unscale),]$value) 
#connect_thres <- 0.06#Or set manually
connect_pos <- connect[which(connect_unscale$value>=connect_thres),]
from_pos = match( connect_pos$from, vertices$name)
to_pos = match( connect_pos$to, vertices$name)
connect_neg <- connect[which(-1*connect_unscale$value>=connect_thres),]
from_neg = match( connect_neg$from, vertices$name)
to_neg = match( connect_neg$to, vertices$name)

fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_load_val.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=as.numeric(connect_pos$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=as.numeric(connect_neg$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
  
})
dev.off()

### Postive only ####
fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_postive_load_val.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=connect_pos$value), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2( mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
  
})
dev.off()

### Negative only ###
fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_negative_load_val.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=connect_neg$value), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
  
})
dev.off()


### MRI version#####################################################################################
setwd("U:/Documents/ACE_Data/Thesis_Analysis/brain_visualisation/ordinary_order")

#Get connection strength
connect <- connect_unscale <- connect_filter <- NULL
connect <- read.csv('connections_85.csv')
datadir <- "U:/Documents/ACE_Data/Thesis_Analysis/MI_PLS_MRI/results_non_sparse/Questions-MRI"

A <- 'connection.strength'
B <- 'Questions'
C <- ''

comp <- 1
fname <- paste(datadir,'/', names, '_', comp, '_boot_X_load.csv', sep='' )
X_load <- read.csv(fname)
connect$value <- X_load$loading*X_load$reliable
connect <- connect[connect$value!=0,]
connect[connect$value==0]<- NA
connect_unscale <- connect
mid_range01 <- function(x){(-min(x))/(max(x)-min(x))}
mid_point <- mid_range01(connect$value)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
connect$value <- range01(connect$value)
#Get edges and vertices
edges <- read.csv('edges_85_split_hemi_extra_space.csv')
edges[31:nrow(edges),] <- edges[31:nrow(edges),][order(edges[31:nrow(edges),]$order),]

vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) 
) 
vertices$group = edges$from[ match( vertices$name, edges$to ) ]

vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
#vertices <- vertices[order(vertices$group),]
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
#Remove dummy
new_names <- vertices$name
new_names[grepl('dum.', new_names)]<- NA
new_names[72:91] <- NA
#Set node colours
node_col <- as.factor(c(0,edges$node.col))
col_pal <-distinctColorPalette(k = length(unique(node_col))-1)
col_pal[8]<- col_pal[7]


# Create a graph object
mygraph <- graph_from_data_frame( edges[,2:3], vertices=vertices )


# The connection object must refer to the ids of the leaves:
connect_filter <- connect
from = match( connect_filter$from, vertices$name)
to = match( connect_filter$to, vertices$name)


print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from, to = to, value=connect$value), alpha=0.6, aes(colour=value), width=0.8, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
})


fname <- paste(datadir,'/', names, '_', comp, '_brain_loads.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from, to = to, value=connect$value), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
})
dev.off()

# The connection object must refer to the ids of the leaves:
perc<- 0.1 #Percentage of connections to keep
connect_thres <- abs(connect_unscale[order(abs(connect_unscale$value), decreasing = TRUE),][perc*nrow(connect_unscale),]$value) 
#connect_thres <- 0.06#Or set manually
connect_filter <- connect[which(abs(connect_unscale$value)>=connect_thres),]
#Rescale to between 0 and 1
mid_point <- mid_range01(connect_filter$value)
connect_filter$value <- range01(connect_filter$value)
from = match( connect_filter$from, vertices$name)
to = match( connect_filter$to, vertices$name)

fname <- paste(datadir,'/', names, '_', comp, '_brain_loads_85_nodes_10perc_thresh_',as.character(connect_thres), '_load_val.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from, to = to, value=connect$value), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
})
dev.off()





### MEG with 68 node plot###########################################################################

setwd("U:/Documents/ACE_Data/Thesis_Analysis/brain_visualisation/ordinary_order")
connect <- connect_unscale <- connect_filter <- NULL

#Change these 
type <- 'X' #Where is brain data? 
datadir <- "U:/Documents/ACE_Data/Thesis_Analysis/MI_PLS_MEG/results_sparse/over_5min"
nodes <- 85 #68 or 85 (do both for MEG)
connect <- read.csv('connections_85.csv') #connections_68.csv
A <- 'Connection.strength' #'s_beta_nets_LASSO_envPartialCorrelationRegularized'

B <- 'Questions'
C <- ''
comp <- 1

names <- paste(A, '_', B, '_', C, sep='')
fname <- paste(datadir,'/', names, comp, '_boot_', type,'_', load_ext, '.csv', sep='' )


X_load <- read.csv(fname)
#Get avergae group correlations 
connect$value <- X_load$loading*X_load$reliable
connect <- connect[connect$value!=0,]
connect[connect$value==0]<- NA
connect_unscale <- connect
mid_range01 <- function(x){(-min(x))/(max(x)-min(x))}
mid_point <- mid_range01(connect$value)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
connect$value <- range01(connect$value)
#Get edges and vertices
edges <- read.csv(paste('edges_', as.character(nodes), '_split_hemi_extra_space.csv', sep='')) 
edges <- edges[order(edges$order),]

vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) 
) 
vertices$group = edges$from[ match( vertices$name, edges$to ) ]

vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
#vertices <- vertices[order(vertices$group),]
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
#Remove dummy
new_names <- vertices$name
new_names[grepl('dum.', new_names)]<- NA
#Set node colours
node_col <- as.factor(c(0,edges$node.col))
#col_pal <-distinctColorPalette(k = length(unique(node_col))-1)


# Create a graph object
mygraph <- graph_from_data_frame( edges[,2:3], vertices=vertices )

###Plot all connections ###
connect_pos <- connect[which(connect_unscale$value>=0),]
from_pos = match( connect_pos$from, vertices$name)
to_pos = match( connect_pos$to, vertices$name)
connect_neg <- connect[which(-1*connect_unscale$value>=0),]
from_neg = match( connect_neg$from, vertices$name)
to_neg = match( connect_neg$to, vertices$name)

fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=as.numeric(connect_pos$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=as.numeric(connect_neg$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
})
dev.off()

### top percentage of connections ###
perc<- 0.05 #Percentage of connections to keep
connect_thres <- abs(connect_unscale[order(abs(connect_unscale$value), decreasing = TRUE),][perc*nrow(connect_unscale),]$value) 
#connect_thres <- 0.06#Or set manually
connect_pos <- connect[which(connect_unscale$value>=connect_thres),]
from_pos = match( connect_pos$from, vertices$name)
to_pos = match( connect_pos$to, vertices$name)
connect_neg <- connect[which(-1*connect_unscale$value>=connect_thres),]
from_neg = match( connect_neg$from, vertices$name)
to_neg = match( connect_neg$to, vertices$name)

fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_load_val.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=as.numeric(connect_pos$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  # geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=as.numeric(connect_neg$value)), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  # scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
})
dev.off()

### Postive only ####
fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_postive_load_val.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from_pos, to = to_pos, value=connect_pos$value), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2( mid='#FFFFFF', high="#ec6c20", midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
})
dev.off()

### Negative only ###
fname <- paste(datadir,'/', names, comp, '_brain_loads_',  as.character(nodes), '_nodes_',as.character(perc), 'perc_',as.character(connect_thres), 'thresh_negative_load_val.pdf', sep='' )
pdf(fname)
print({ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=node_col), size=3) +
  #scale_colour_manual(values=c('#FFFFFF',col_pal))+
  scale_colour_manual(values=cols)+
  geom_conn_bundle(data = get_con(from = from_neg, to = to_neg, value=connect_neg$value), alpha=0.6, aes(colour=value), width=0.7, tension=0.8) +
  scale_edge_colour_gradient2(low="#0b81cc", mid='#FFFFFF', midpoint=mid_point,na.value ="grey")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=new_names, angle = angle, hjust=hjust), size=3, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm") ) +
  expand_limits(x = c(-1.6, 1.6), y = c(-1.6, 1.6))
})
dev.off()
