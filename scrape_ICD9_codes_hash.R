library(rvest)
library(stringr)
library(foreach)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

ICD_9 <- read_html("http://icd9.chrisendres.com/index.php?action=contents")

all_groups <- c()
for( i in html_nodes(ICD_9, "a")){
  if (i %>% html_children() %>% html_attr("class") %in% c("chapter", "tabular")){
    group <- read_html(paste("http://icd9.chrisendres.com/", i %>% html_attr("href"), sep=""))
    all_groups <- c(all_groups, html_text(html_nodes(group, "a div")))
  }
  
}

groups <- data.frame("group" = all_groups[1:148], "range"=0)

get_range <- function(x){
  y <- str_replace_all(str_split(gsub(".*\\((.*)\\).*", "\\1", x[1]), "-")[[1]], "[VEve]", "")
  if(length(y) == 2){
    return(as.numeric(y[1]:y[2]))
  }
  else{return(as.numeric(y[1]))}
}

get_range_single <- function(x){
  return(substr(x[1],1,3))
}

groups$range[c(1:32,43:96,116:128,139:148)] <- apply(groups[c(1:32,43:96,116:128,139:148),], 1, get_range)
groups$range[-c(1:32,43:96,116:128,139:148)] <- apply(groups[-c(1:32,43:96,116:128,139:148),], 1, get_range_single)

groups_v <- data.frame("group" = all_groups[149:163], "range"=0)
groups_v$range <- apply(groups_v, 1, get_range)

groups_e <- data.frame("group" = all_groups[164:186], "range"=0)
groups_e$range <- apply(groups_e, 1, get_range)

groups_hash <- data.frame("num"=0, "group"=0)
for(i in 1:nrow(groups)){
  for(j in groups$range[i]){
    groups_hash <- rbind(groups_hash, data.frame("num"=j, "group"=groups$group[i]))
  }
}
groups_hash <- groups_hash[-1,]

groups_e_hash <- data.frame("num"=0, "group"=0)
for(i in 1:nrow(groups_e)){
  for(j in groups_e$range[i]){
    groups_e_hash <- rbind(groups_e_hash, data.frame("num"=j, "group"=groups_e$group[i]))
  }
}
groups_e_hash <- groups_e_hash[-1,]

groups_v_hash <- data.frame("num"=0, "group"=0)
for(i in 1:nrow(groups_v)){
  for(j in groups_v$range[i]){
    groups_v_hash <- rbind(groups_v_hash, data.frame("num"=j, "group"=groups_v$group[i]))
  }
}
groups_v_hash <- groups_v_hash[-1,]

patients <- readRDS("final_data_wo_labs.RDS")

for(i in all_groups){
  patients[,i] <- 0
}

start = proc.time()
foreach(patient = 1:100)%dopar%{
  foreach( i = colnames(patients[185+which(patients[patient,186:5531] == 1)]))%dopar%{
    code <- substr(str_trim(str_split(i, "-")[[1]][1]), 1, 4)
    if( tolower(substr(code,1,1)) == "e"){
      patients[patient, as.character(groups_e_hash$group[groups_e_hash$num == substr(code,2,4)])] <- 1
    }
    else if( tolower(substr(code,1,1)) == "v"){
      patients[patient, as.character(groups_v_hash$group[groups_v_hash$num == substr(code,2,3)])] <- 1
    }
    else{
      patients[patient, as.character(groups_hash$group[groups_hash$num == substr(code,1,3)])] <- 1
    }
  }
  if(patient%%1000 == 0){print(patient)}
  
}
proc.time()-start
p=1
for (p in (1:100)){
  d <- X_diagnosis[p,]
  c <- 1:ncol(d)
  c <- rownames(c)[d[d==1]]
  d <- as.numeric(rownames(d))
  d <- as.numeric(rownames(d)[d==1])
  codes <- lapply(d,function(i) {substr(str_trim(str_split(i, "-")[[1]][1]), 1, 4)})
  for(g in groups){
    
  }
  code = codes[1]
  count = 0
  if(tolower(substr(code,1,1)) == "e"){
    
    patients[patient, as.character(groups_e_hash$group[groups_e_hash$num == substr(code,2,4)])] <- 1
  }
  else if( tolower(substr(code,1,1)) == "v"){
    patients[patient, as.character(groups_v_hash$group[groups_v_hash$num == substr(code,2,3)])] <- 1
  }
  else{
    groups_hash[]
    patients[patient, as.character(groups_hash$group[(groups_hash$num) == substr(code,1,3)])] <- 1
  }
  
  freq <- list()
  for(w in high_pneu){
    count = 0
    if (w %in% corpus) count <- length(corpus[corpus == w])
    freq <- c(freq,count)
  }
  x <- as.data.frame(t(unlist(freq)))
  temp <- rbind_all(list(temp,x))
  if(p%%500 == 0){
    positive_terms <- rbind_all(list(positive_terms,temp))
    temp <- as.data.frame(matrix(nrow = 0,ncol = length(high_pneu)))
    print(p)
  }
}
positive_terms <- dplyr::rbind_all(list(positive_terms,temp))
colnames(positive_terms) <- high_pneu

for(c in codes){
  print(c)
}