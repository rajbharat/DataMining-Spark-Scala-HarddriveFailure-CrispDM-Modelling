

df<-read.csv(file="C:\\Users\\Lenovo\\Downloads\\cleaned.csv",header = TRUE)

for (i in names(df[6:34])) {
     df[[i]][which(is.na(df[[i]]))]<-mean(df[[i]],na.rm = TRUE)
 }

for (i in names(df[6:34])) {
     df[[i]]<-(df[[i]]-min(df[[i]]))/(max(df[[i]])-min(df[[i]]))
}

write.csv(df,file="C:\\Users\\Lenovo\\Downloads\\norm.csv",row.names = TRUE)




