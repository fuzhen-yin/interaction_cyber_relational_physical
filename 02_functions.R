# CLEAN TWEET TEXTS: remove: mentions, emoji icons, url,escape sequences (i.e. &amp;), symbols/punctuations ("#", "@")
clean_twt <- function(texttoclean) {
  # convert to lower case 
  texttoclean <- tolower(texttoclean)  
  texttoclean <- gsub("@\\w+", "", texttoclean)         # @mention
  texttoclean <- gsub("^rt", "", texttoclean)         # retweet
  texttoclean <- gsub("&amp;", "and", texttoclean)         # &amp;
  texttoclean <- gsub("’", "'", texttoclean)
  
  texttoclean <- gsub("(f|ht)tp(s?)://\\S+", "", texttoclean, perl = T)   # url 
  texttoclean <- gsub("[^\x01-\x7F]", "", texttoclean)  # emojis
  texttoclean <- gsub("(.*?)($|'|[^[:punct:]]+?)(.*?)", "\\2", texttoclean)   # remove puncuation except for 'apostrophe'
  
  # Remove spaces and newlines
  texttoclean <- gsub("\n", " ", texttoclean)
  texttoclean <- gsub("^\\s+", "", texttoclean)
  texttoclean <- gsub("\\s+$", "", texttoclean)
  texttoclean <- gsub("[ |\t]+", " ", texttoclean)  
  return(texttoclean)
}

# calculate the # and % of pro, anti, neutral tweets 
get_pro_anti_proportion <- function(col){
  totl <- length(col)
  npro <- length(col[col ==1])
  nanti <- length(col[col == -1])
  nneutral <- length(col[col == 0])
  
  print("total tweets n: ")
  print(totl)
  print("-----------------")
  
  print("pro vaccine tweets n: ")
  print(npro)
  print(npro / totl)
  print("-----------------")
  
  print("anti vaccine tweets n: ")
  print(nanti)
  print(nanti / totl)
  print("-----------------")
  
  print("neutral vaccine tweets n: ")
  print(nneutral)
  print(nneutral / totl)
  print("-----------------")
}


# filter out tweets based on key words 
lt_vax_keyword = "vax|vacc|vaccine|vaccination|moderna|pfizer|astrazeneca|johnson&|j&j|cov19|covid|pandemic|corona|rona|shot|jab|dose|variant|delta|booster"

filter_twt_keywords <- function(df){
  before = nrow(df)
  df$lower_text <- tolower(df$text)
  df <- df[str_detect(df$lower_text, lt_vax_keyword),]
  after = nrow(df)
  print("Number of tweets removed:")
  print(before - after)
  return(df[1:length(df)-1])
}


# Split Function - convert prediction to three columns: pro, anti, neutral
pred_split <- function(tt){
  tt$pro <- 0
  tt$anti <- 0
  tt$neutral <- 0
  tt[tt$pred == 1,]$pro <- 1
  tt[tt$pred == -1,]$anti <- 1
  tt[tt$pred == 0,]$neutral <- 1
  return(tt)
}

position_split <- function(tt){
  tt$pro <- 0
  tt$anti <- 0
  tt$neutral <- 0
  tt[tt$position == 1,]$pro <- 1
  tt[tt$position == -1,]$anti <- 1
  tt[tt$position == 0,]$neutral <- 1
  return(tt)
}

user_state_split <- function(aa){
  aa$Cyber <- 0
  aa$NYS <- 0
  aa[aa$state == "-999",]$Cyber =1
  aa[aa$state == "NYS",]$NYS =1
  
  return(aa)
}

user_region_split <- function(aa){
  aa$Cyber <- 0
  aa$Long_Island <- 0
  aa$New_York_City <- 0
  aa$Lower_Hudson_Valley <- 0
  aa$Capital_Region <- 0
  aa$Eastern_Adirondacks <- 0
  aa$Western_Adirondacks <- 0
  aa$Central_New_York <- 0
  aa$Western_Finger_Lakes <- 0
  aa$Western_New_York <- 0
  
  aa[aa$region == "-999",]$Cyber =1
  aa[aa$region == "Long Island",]$Long_Island =1
  aa[aa$region == "New York City",]$New_York_City =1
  aa[aa$region == "Lower Hudson Valley",]$Lower_Hudson_Valley =1
  aa[aa$region == "Capital Region",]$Capital_Region =1
  aa[aa$region == "Eastern Adirondacks",]$Eastern_Adirondacks =1
  aa[aa$region == "Western Adirondacks",]$Western_Adirondacks =1
  aa[aa$region == "Central New York",]$Central_New_York =1
  aa[aa$region == "Western Finger Lakes",]$Western_Finger_Lakes =1
  aa[aa$region == "Western New York",]$Western_New_York =1
  
  return(aa)
}

# User Labeling - Majority Weighting Rule
user_position_maj <- function(aa){
  aa$position <- NA
  aa[(aa$pro > aa$anti & aa$pro > aa$neutral), ]$position <- 1
  aa[(aa$anti > aa$pro & aa$anti > aa$neutral), ]$position <- -1
  aa[(aa$neutral >= aa$pro & aa$neutral >= aa$anti), ]$position <- 0
  aa[aa$pro == aa$anti,]$position <- 0
  return(aa)
}


# produce hybrid space graph 
plot_cyber_sum_relational <- function(g,coef_v,coef_e,g_layout, edge_curve = 1){
  print("sum")
  V(g)$size <- V(g)$sum * coef_v
  V(g)$label <- NA
  E(g)$width <- E(g)$sum * coef_e
  E(g)$color <- "black"
  plot(g, edge.arrow.size = 0.3, edge.curved=edge_curve,layout = g_layout, main = "Total")  
 
}

plot_cyber_pro_relational <- function(g,coef_v,coef_e,g_layout, edge_curve = 0.5){
   # pro 
  print("pro")
  V(g)$size <- V(g)$pro * coef_v
  V(g)$label <- NA
  E(g)$width <- E(g)$pro * coef_e
  E(g)$color <- "black"
  plot(g, edge.arrow.size = 0.3, edge.curved=edge_curve,layout = g_layout, main = "Pro")
 
}

plot_cyber_anti_relational <- function(g,coef_v,coef_e,g_layout, edge_curve = 0.7){
   # anti 
  print("anti")
  V(g)$size <- V(g)$anti * coef_v
  V(g)$label <- NA
  E(g)$width <- E(g)$anti * coef_e
  E(g)$color <- "black"
  plot(g, edge.arrow.size = 0.03, edge.curved=edge_curve,layout = g_layout, main = "Anti")
 
}


# cohen's kappa statistics 
cohen_kappa <- function(tble){
  ttl_pred <- sum(tble)
  correct_pred <- sum(diag(tble))
  expected_pred <- (sum(tble[1,]) * sum(tble[,1]) + 
                      sum(tble[2,]) * sum(tble[,2]) + 
                      sum(tble[3,]) * sum(tble[,3])) / sum(tble)
  re <- (correct_pred - expected_pred) / (ttl_pred - expected_pred)
  return(re)
}
