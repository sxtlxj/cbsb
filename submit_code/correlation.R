library(psych)
result <- read.csv(paste0("data/","thres_schema_init","/all_dwsch_added.csv"))
result <- result %>% group_by(Subject) %>%
  summarise_at(vars(Cho_1:dwellmean_4), mean, na.rm = TRUE)
result <- result[,6:ncol(result)]
para_change_tag <- "thres_schema_init"
pms <- colnames(result)
pms <- pms[-which(pms == para_change_tag)]
rndb <- corr.test(result[ , colnames(result) != para_change_tag],  # Calculate correlations
                  result[ , colnames(result) == para_change_tag])
padj <- rndb$p.adj
r <- rndb$r
a <- cbind(rndb$r,rndb$p.adj)
rndb$r[which(rndb$p.adj<0.05)]
pms[which(rndb$p.adj<0.05)]
cordt <- data.frame("para_name" = para_change_tag, 
                    "measure" = paste(pms[which(rndb$p.adj!=1)], collapse = ","),
                    "cor" = paste(rndb$r[which(rndb$p.adj!=1)],collapse = ","),
                    "padj"= paste(rndb$p.adj[which(rndb$p.adj!=1)],collapse =","),
                    "median_abs_cor" = median(abs(rndb$r)))

name.list <- c("a_schema", "h_schema", "Beta_N", "Beta_Var", "a_generic", "h_generic", 
               "Beta_gN", "Beta_gVar", "decay_speed", "decay_speed_thres" ,
               "thres_schema_init","thres_item_inter","thres_item_final","timevar","theta_shift")

for (i in name.list){
  para_change_tag <- i
  result <- read.csv(paste0("data/iter5/data/",i,"/all_dwsch_added.csv"))
  result <- result %>% group_by(Subject) %>%
    summarise_at(vars(Cho_1:dwellmean_4), mean, na.rm = TRUE)
  result <- result[,6:ncol(result)]
  pms <- colnames(result)
  pms <- pms[-which(pms == para_change_tag)]
  rndb <- corr.test(result[ , colnames(result) != para_change_tag],  # Calculate correlations
                    result[ , colnames(result) == para_change_tag])
  a <- cbind(a,rndb$r)
  a <- cbind(a,rndb$p.adj)
  pos <- which(rndb$p.adj<0.05)
  rndb$r[pos]
  pms[pos]
  onerecord <- c(para_change_tag, paste(pms[pos], collapse = ','),
                 paste(rndb$r[pos],collapse = ","),
                 paste(rndb$p.adj[pos],collapse =","),
               median(abs(rndb$r)))
  cordt[nrow(cordt)+1,] <- onerecord
}
colnames(a) <- c(rep("thres_schema_init",2),rep(name.list,each=2))
colnames(a)
cordt <- cordt[-1,]

b <- cordt %>% arrange(desc(median_abs_cor)) %>% select(para_name,median_abs_cor)
