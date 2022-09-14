# Read in Dataset
gf_tweet <- read_json("collected_twts/search_2-2/00_search2-2_tweet_geo_vax_NY.json", simplifyDataFrame=TRUE)
gf_twt_conv <- read_json("collected_twts/search_2-2/00_search2-2_tweet_conversation_448958.json", simplifyDataFrame=TRUE)

# Prediction NY
tt <- read.csv("collected_twts/Labeled_1/Labeled_2/Labeled_3/RESULT_NYS.csv", colClasses = c("character"), header = TRUE)[c("id","pred")]
tt <- pred_split(tt)
r_pred_NY <- left_join(gf_tweet,tt)

# Prediction Conversation 
tt <- read.csv("collected_twts/Labeled_1/Labeled_2/Labeled_3/RESULT_CONV.csv", colClasses = c("character"), header = TRUE)[c("id","pred")]
tt <- pred_split(tt)
r_pred_CONV <- left_join(gf_twt_conv,tt)

# Users 
data <- r_pred_CONV
# users living in NYS 
tt <- data[c("author_id","geo.place_id")]
tt[!tt$geo.place_id %in% NY_spatial$geo.place_id,]$geo.place_id <- NA
tt <- na.omit(tt)
tt$freq <- 1
tt <- aggregate(freq~author_id+geo.place_id, data=tt, FUN = sum)
tt <- tt[order(tt$author_id, tt$freq,decreasing = T),]
tt <- tt[!duplicated(tt$author_id),]
tt <- tt[c("author_id","geo.place_id")]
rownames(tt) <- NULL

# authors / replies in cyber space 
aa <- data.frame(author_id=c(data$author_id,data$in_reply_to_user_id),
                 geo.place_id=NA) %>% 
  distinct(author_id, .keep_all= TRUE)
aa <- aa[!aa$author_id %in% tt$author_id,]

tt <- rbind(tt,aa)
tt <- left_join(tt, NY_spatial)
tt$state <- NA
tt[tt$geo.place_id %in% NY_spatial$geo.place_id,]$state <- "NYS"

rf_users <- tt   

# position 
data <- r_pred_CONV
tt <- data[c("author_id", "pro","anti","neutral")]
tt <- aggregate(cbind(pro,anti,neutral) ~author_id, data = tt, FUN = sum)
tt <- user_position_maj(tt)
rf_users <- left_join(rf_users,tt[c("author_id","position")])
rf_users[is.na(rf_users)] <- -999
table(rf_users$position)

# SECTION 1: Relational Space
# Prepare relational space prediction results: r_pred_Relational
ttt <- r_pred_CONV
tt <- ttt[ttt$geo.place_id %in% gf_NYBbox$id,]            # physical space

# distinguish physical space users from relational space users   
temp <- rf_users[rf_users$state =="NYS",]
temp$to_reply <- 0
temp$replied <- 0
temp[temp$author_id %in% unique(tt[!is.na(tt$in_reply_to_user_id),]$author_id),]$to_reply <- 1
temp[temp$author_id %in% unique(ttt$in_reply_to_user_id),]$replied <- 1
temp$interact <- temp$to_reply + temp$replied
temp$location <- "physical"
temp[temp$interact>0,]$location <- "relational"
rf_users_rela_phy <- temp

# Relational Space tweet dataset 
r_pred_Relational <- ttt[(ttt$author_id %in% temp[temp$location=="relational",]$author_id | 
                          ttt$in_reply_to_user_id %in% temp[temp$location=="relational",]$author_id),]

## Labeling Results in 3 Spaces
print(paste("######################  ","Cyber space", sep = ""))
get_pro_anti_proportion(r_pred_CONV$pred)

print(paste("######################  ","Relational space", sep = ""))
get_pro_anti_proportion(r_pred_Relational$pred)

print(paste("######################  ","Physical space", sep = ""))
data <- r_pred_CONV[r_pred_CONV$geo.place_id %in% NY_spatial$geo.place_id,]
get_pro_anti_proportion(data$pred)

## preparing network data
# Create a dataframe of Nodes and Edges by removing inactive users (total degree < 2)
data <- r_pred_Relational
data <- data[c("author_id", "in_reply_to_user_id")] %>% setnames(., new = c("Source","Target"))
data <- na.omit(data)
data$Type <- "Directed"
data$Weight <- 1
data <- aggregate(Weight ~ Source + Target + Type, data = data, FUN = sum)

# inactive user degree < 2
cc <- data.frame(author_id=c(data$Source, data$Target), degree = 1)
cc <- aggregate(degree ~ author_id, data=cc, FUN=sum)
inactive_user <- cc[cc$degree ==1,]
# remove inactive user
data <- data[!data$Source %in% inactive_user$author_id,]
data <- data[!data$Target %in% inactive_user$author_id,]

edge <- data
node <- rf_users[rf_users$author_id %in% c(edge$Source,edge$Target),c("author_id","state","position","geo.place_id")] %>% setnames(., new = c("Id","state","position","geo.place_id"))
node <- left_join(node, gf_NYBbox["id"], by=c("geo.place_id"="id")) %>% st_as_sf() %>% st_centroid()

# plot
tm_shape(NY_region) + tm_polygons(alpha = 0) + 
  tm_shape(node) + tm_dots(col = "red")

# unlist coordinates to long and lat 
node <- node %>% mutate(long = unlist(map(node$geometry,1)), lat = unlist(map(node$geometry,2)))
st_geometry(node) <- NULL

aa <- 0.015
n <- node[node$state =="NYS",] %>% nrow()
node[node$state =="NYS",]$long <- node[node$state =="NYS",]$long + runif(n, min = -aa, max = aa)
node[node$state =="NYS",]$lat <- node[node$state =="NYS",]$lat + runif(n, min = -aa, max = aa)

write.csv(node,"network/search_2-2/new_relational_space/relational_nodes_revision_3.csv", row.names = FALSE)
write.csv(edge,"network/search_2-2/new_relational_space/relational_edges_revision_3.csv", row.names = FALSE)


# Read in Community Detection results - Relational Space
path <-"network/search_2-2/new_relational_space/mod3_relational_communityDetection.csv"
data <- read.csv(path, colClasses = c("character"), header = TRUE)[c("Id","modularity_class")] %>% 
  setnames(.,new = c("author_id","class"))
data <- left_join(data, rf_users[c("author_id","position","region")])
data <- user_region_split(data)
data <- position_split(data)

data$n_member <- 1
data <- data[, !names(data)%in% c("author_id" ,"region","position")]
data <- aggregate(.~ class, data = data, FUN = sum)
data$p_number <- round(data$n_member / sum(data$n_member), 4)
data <- data[order(data$n_member, decreasing = T),]
rownames(data) <- NULL
# count users with no opinions
data$noopinion <- data$n_member - data$pro - data$anti - data$neutral
# save result to a dataframe
r_mod3_Relational <- data

# extract only large communities (>1%)
df <- r_mod3_Relational[r_mod3_Relational$p_number > 0.01,]
df$class <- c("(a)","(b)" ,"(c)" ,"(d)" ,"(e)" ,"(f)" ,"(g)" ,"(h)")
cols <- names(df)[!names(df) %in% c("class","n_member","p_number")]
df[cols] <- round(df[cols] / df$n_member,3) * 100

# data for sentiment plot
lt <- c("class","noopinion","pro","anti","neutral","n_member")
data <- df[lt] %>% 
  setnames(.,old = "noopinion", new = "aa_no_opinion") %>% 
  pivot_longer(cols = 2:length(df[lt]), names_to = "Sentiment", values_to ="Percent")

# SENTIMENT plot 
col = tmaptools::get_brewer_pal("Pastel1", n = 3)     # color palette
col <- c("#EBEBEB", col)

# without n_member
ggplot(data = filter(data, Sentiment %in% c("pro","anti","neutral","aa_no_opinion")), aes(fill=Sentiment, y=Percent, x=class, label = Percent)) +
  geom_bar(data = filter(data, Sentiment %in% c("pro","anti","neutral","aa_no_opinion")),
           position="stack", stat="identity", color = "black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_bw()+
  scale_fill_manual(values = col) +
  ylab("Percent of users in each community (%)") +
  xlab("Communities in Relational Space")

# data for location plot 
lt <- c("class","Cyber","Long_Island","New_York_City","Lower_Hudson_Valley","Capital_Region","Eastern_Adirondacks",
        "Western_Adirondacks","Central_New_York","Western_Finger_Lakes", "Western_New_York" )
data <- df[lt] %>% 
  setnames(., old="Cyber", new = "aa_cyber") %>% 
  pivot_longer(cols = 2:length(df[lt]), names_to = "Location", values_to ="Percent")

data$Location <- gsub("_"," ", data$Location)
col = tmaptools::get_brewer_pal("Pastel1", n = 9)     # color palette
col <- c("#F9F9F9", col)

ggplot(data, aes(fill=Location, y=Percent, x=class, label = Percent)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  theme_bw()+ 
  scale_fill_manual(values = col) + 
  scale_y_break(c(55,95)) + 
  ylab("Percent of users in communities (%)") + 
  xlab("Communities")

## Map User Locations
# Large Communities (>1%)
lt_clas <- r_mod3_Relational[r_mod3_Relational$p_number > 0.01,"class"]

# Map users locations in relational space communities 
path <-"network/search_2-2/new_relational_space/mod3_relational_communityDetection.csv"
data <- read.csv(path, colClasses = c("character"), header = TRUE)[c("Id","modularity_class")] %>% 
  setnames(.,new = c("author_id","class"))
data <- left_join(data, rf_users[c("author_id","geo.place_id","region")])
data <- data[(data$class %in% lt_clas & data$region!= -999),]
data <- left_join(data, gf_NYBbox["id"], by=c("geo.place_id"="id")) %>% st_as_sf() %>% st_centroid()

aa <- 0.02
data <- data %>% mutate(long = unlist(map(data$geometry,1)), lat = unlist(map(data$geometry,2)))
st_geometry(data) <- NULL
data$long <- data$long + runif(nrow(data), -aa, aa)
data$lat <- data$lat+ runif(nrow(data), -aa, aa)
data <- st_as_sf(data,coords = c("long", "lat"))
st_crs(data) <- 4326

cols <- c("#6E7ACA", "#5BC344", "#D64E2B", "#C94992", "#C78A1C", "#C465D6", "#D14058", "#699025","#38C9FF")
lt_label <- c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)")
pp <- list()
for (i in 1:length(lt_clas)) {
  df <- data[data$class==lt_clas[[i]],]
  pp[[i]] <- tm_shape(NY_region) + tm_polygons(col = "white", border.col = "black") +
    tm_shape(df) + tm_dots(col = "#E41A1C", size = 0.08, alpha = 0.3) +
    tm_layout(title = lt_label[[i]])
}
# PLOT
tmap_arrange(pp, ncol = 4)


# SECTION 2: Physical Space
## Sentiment Plot
df <- rf_users[rf_users$state=="NYS",c("author_id","position","region")] %>% mutate(n_member = 1)
df <- position_split(df)
df <- df[c("region","n_member","pro","anti","neutral")]
df <- aggregate(.~ region, data = df, FUN = sum)
df[c("pro","anti","neutral")] <- round(df[c("pro","anti","neutral")] / df$n_member,3) * 100

lt <- c("region","n_member","pro","anti","neutral")
data <- df[lt] %>% pivot_longer(cols = 2:length(df[lt]), names_to = "Sentiment", values_to = "Percent")
colr = tmaptools::get_brewer_pal("Pastel1", n = 3) 

ggplot(data = filter(data, Sentiment %in% c("pro","anti","neutral")), aes(fill=Sentiment, y=Percent, x=region, label = Percent)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_bw()+ 
  scale_fill_manual(values = colr)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y.right = element_text(angle = 90)) +
  ylab("Percent of users in each community (%)") +
  xlab("Communities in Physical Space") 

# SECTION 3: Cyber Space 
#### CONVERSATION NETWORK 
#DIRECTED network edge with all NYS users 
data <- pred_CONV
tt <- data[c("author_id", "in_reply_to_user_id")] %>% setnames(., new = c("Source","Target"))
tt <- na.omit(tt)
tt$Type <- "Directed"
tt$Weight <- 1
tt <- aggregate(Weight ~ Source + Target + Type, data = tt, FUN = sum)

# inactive user degree <= 1  
cc <- data.frame(author_id=c(tt$Source, tt$Target), degree = 1)
cc <- aggregate(degree ~ author_id, data=cc, FUN=sum)
inactive_user <- cc[cc$degree ==1,]

# remove inactive user
tt <- tt[! tt$Source %in% inactive_user$author_id,]
tt <- tt[!tt$Target %in% inactive_user$author_id,]

edges_CONV <- tt
data <- rf_users
nodes_CONV <- data[data$author_id %in% c(edges_CONV$Source,edges_CONV$Target) ,]
names(nodes_CONV) <- c("Id","geo.place_id","geo.place_name", "county_id","county","region_id","region","state","position"   )

write.csv(nodes_CONV,"network/search_2-2/CONV_nodes_1.csv", row.names = FALSE)
write.csv(edges_CONV,"network/search_2-2/CONV_edges_1.csv", row.names = FALSE)

# Cyber space community detection results 
path <-"network/search_2-2/mod3_communityDetection.csv"
data <- read.csv(path, colClasses = c("character"), header = TRUE)[c("Id","modularity_class")] %>% 
  setnames(.,new = c("author_id","class"))
data <- left_join(data, rf_users[c("author_id","position","region")])
data <- user_region_split(data)
data <- position_split(data)

data$Relational <- 0
data[data$author_id %in% c(r_pred_Relational$author_id, r_pred_Relational$in_reply_to_user_id), "Cyber"] <- 0
data[data$Cyber == 0, "Relational"] <- 1
data$n_member <- 1
data <- data[, !names(data)%in% c("author_id" ,"region","position")]
data <- aggregate(.~ class, data = data, FUN = sum)
data$p_number <- round(data$n_member / sum(data$n_member), 4)
data <- data[order(data$n_member, decreasing = T),]
rownames(data) <- NULL
# count users with no opinions
data$noopinion <- data$n_member - data$pro - data$anti - data$neutral

# save result to a dataframe
r_mod3_Cyber <- data

# extract only large communities (>1%)
df <- r_mod3_Cyber[r_mod3_Cyber$p_number > 0.01,]
df$class <- c("(I)","(II)" ,"(III)" ,"(IV)" ,"(V)" ,"(VI)")

cols <- names(df)[!names(df) %in% c("class","n_member","p_number")]
df[cols] <- round(df[cols] / df$n_member,3) * 100

# sentiment plot
lt <- c("class","noopinion","pro","anti","neutral")
data <- df[lt] %>% 
  setnames(.,old = "noopinion", new = "aa_no_opinion") %>% 
  pivot_longer(cols = 2:length(df[lt]), names_to = "Sentiment", values_to ="Percent")

# plot 
col = tmaptools::get_brewer_pal("Pastel1", n = 3)     # color palette
col <- c("#EBEBEB", col)

ggplot(data = filter(data, Sentiment %in% c("pro","anti","neutral","aa_no_opinion")), aes(fill=Sentiment, y=Percent, x=class, label = Percent)) +
  geom_bar(data = filter(data, Sentiment %in% c("pro","anti","neutral","aa_no_opinion")),
           position="stack", stat="identity", color = "black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme_bw()+
  scale_fill_manual(values = col) +
  ylab("Percent of users in each community (%)") +
  xlab("Communities in Cyber Space")

# Location Plot 
lt <- c("class","Cyber","Relational",lt_regions)
data <- df[lt] %>% 
  pivot_longer(cols = 2:length(df[lt]), names_to = "Location", values_to ="Percent")

data[!data$Location %in% c("Cyber","Relational"),"Percent"] <- data[!data$Location %in% c("Cyber","Relational"),"Percent"]

data$Location <- gsub("_"," ", data$Location)
col = tmaptools::get_brewer_pal("Pastel1", n = 9)     # color palette
col <- c("#FBB4AE", "#C1C8D8", col)
col2 = tmaptools::get_brewer_pal("Pastel2", n = 3)[1:2]   # color palette

p1 = ggplot(data = filter(data, Location %in% c("Cyber","Relational")), aes(fill=Location, y=Percent, x=class, label = Percent)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  theme_bw()+ 
  scale_fill_manual(values = col2) +
  ylab("Percent of users in communities (%)") + 
  xlab("Communities")

p2 = ggplot(data = filter(data, !Location %in% c("Cyber","Relational")), aes(fill=Location, y=Percent, x=class, label = Percent)) +
  geom_bar(position="stack", stat="identity", color = "black") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  theme_bw()+
  scale_fill_manual(values = col[3:12]) +
  ylab("Percent of users in communities (%)") +
  xlab("Communities")

ggarrange(p1, p2, ncol = 1, align = "v")

# SECTION 4: Hybrid Space Network - Relational - Cyber
ttt <- r_pred_CONV
data <- ttt[c("author_id", "in_reply_to_user_id", "pro", "anti", "neutral")]
df <- rf_users[c("author_id")] %>% left_join(., rf_users_rela_phy[c("author_id","location")])

df[df$author_id %in% c(r_pred_Relational$author_id, r_pred_Relational$in_reply_to_user_id), "location"] <- "relational"
df[is.na(df)] <- "cyber"

data <- left_join(data, df) %>% setnames(., old = "location", new = "Source")
data <- left_join(data, df, by=c("in_reply_to_user_id" = "author_id")) %>% setnames(., old = "location", new = "Target")
data <- data[c("Source","Target","pro","anti","neutral" )]

data$Source <- gsub("physical","relational", data$Source)

data <- aggregate(cbind(pro,anti,neutral)~Source+Target, data = data, FUN = sum) %>% mutate(., sum = pro + anti + neutral, type = "directed")

g_edge <- data
g_node <- aggregate(cbind(pro,anti,neutral,sum)~Source, data=g_edge[c("Source","pro","anti","neutral","sum")], FUN=sum) %>% 
  setnames(., old="Source", new = "label") %>% 
  mutate(., id=rownames(.))

coef_v = 0.0005
coef_e = 1 / 30000
#     Anti 
g <- graph_from_data_frame(d = g_edge, vertices = g_node, directed = TRUE)
plot_cyber_sum_relational(g,coef_v,coef_e,layout_on_sphere(g))
plot_cyber_pro_relational(g,coef_v,coef_e,layout_on_sphere(g))
plot_cyber_anti_relational(g,coef_v,coef_e, layout_on_sphere(g))

# SECTION 5: Hybrid Space Network - Relational - Physical 

#EDGES
data <- r_pred_Relational[c("author_id", "in_reply_to_user_id", "pro", "anti", "neutral")]
df <- rf_users[c("author_id","region")] 
df[(df$region == "-999" & df$author_id %in% c(r_pred_Relational$author_id, r_pred_Relational$in_reply_to_user_id)),"region"] <- "relational"

data <- left_join(data, df) %>% setnames(., old = "region", new = "Source")
data <- left_join(data, df, by=c("in_reply_to_user_id" = "author_id")) %>% setnames(., old = "region", new = "Target")
data <- data[c("Source","Target","pro","anti","neutral" )]

data <- aggregate(cbind(pro,anti,neutral)~Source+Target, data = data, FUN = sum) 
data$from_und <- mapply(min, data$Source, data$Target)
data$to_und <- mapply(max, data$Source, data$Target)
data <- data[c("from_und","to_und","pro","anti","neutral")] %>% setnames(., old = c("from_und","to_und"), new = c("Source","Target"))

data <- aggregate(cbind(pro,anti,neutral)~Source+Target, data = data, FUN = sum) %>% mutate(., sum = pro + anti + neutral, type = "undirected")
g_edge <- data

#Node
g_node <- g_edge[c("Source","pro","anti","neutral","sum")] %>% setnames(., old = "Source", new = "label")
bb <- g_edge[c("Target","pro","anti","neutral","sum")] %>% setnames(., old = "Target", new = "label")
g_node <- rbind(g_node,bb)
g_node <- aggregate(cbind(pro,anti,neutral,sum)~label, data=g_node, FUN=sum) %>% 
  mutate(., id=rownames(.))

# prepare the coordinates of region centroids 
aa <- st_centroid(NY_region) %>% st_as_sf(.)
aa <- aa %>% mutate(lat = unlist(map(aa$geometry,1)), long = unlist(map(aa$geometry,2)))
st_geometry(aa) <- NULL

g_node <- left_join(g_node, aa[c("Region.name","lat","long")],by=c("label"="Region.name"))
g_node[g_node$label=="relational",c("lat","long")] <- c(-75.23019,42.17946)
g_node[g_node$label=="Lower Hudson Valley",c("lat","long")] <- c(-73.5,41.67946)
g_node[g_node$label=="Long Island",c("lat","long")] <- c(-73.03611,41.18701)
g_node[g_node$label=="Central New York",c("lat","long")] <- c(-76.25862, 43.13888)
g_node[g_node$label=="Western New York",c("lat","long")] <- c(-77.65932, 41.45)

# PLOT
coef_v = 0.002
coef_e = 0.002
lo <- layout.norm(as.matrix( g_node[c("lat","long")] ))
g <- graph_from_data_frame(d = g_edge, vertices = g_node, directed = FALSE)

plot_cyber_sum_relational(g,coef_v,coef_e,lo, edge_curve = 0)
plot_cyber_pro_relational(g,coef_v,coef_e,lo, edge_curve = 0)
plot_cyber_anti_relational(g,coef_v,coef_e,lo, edge_curve = 0)


# Map Study Area 
# Street Network 
pth <- "StreetSegment.shp"
NY_street <- st_read(pth)
NY_street <- data[(data$FCC > "A1" & data$FCC < "A29"),]

# Collected NYS Tweets 
df <- r_pred_CONV[r_pred_CONV$geo.place_id %in% NY_spatial$geo.place_id,]
df <- left_join(df, gf_NYBbox["id"], by=c("geo.place_id"="id")) %>% st_as_sf() %>% st_centroid()
aa <- 0.015
df <- df %>% mutate(long = unlist(map(df$geometry,1)), lat = unlist(map(df$geometry,2)))
st_geometry(df) <- NULL
df$long <- df$long + runif(nrow(df), -aa, aa)
df$lat <- df$lat+ runif(nrow(df), -aa, aa)
df <- st_as_sf(df,coords = c("long", "lat"))
st_crs(df) <- 4326
df <- st_transform(df, crs = st_crs(NY_state))

# plot
pdf(loc, width = 12, height = 8)  # width = 350, height = 350
tm_shape(NY_state) + tm_polygons(alpha = 0, lwd =2) + 
  tm_shape(NY_street) + tm_lines() + 
  tm_shape(df) + tm_dots(col = "#E41A1C", size = 0.03, alpha = 0.3)