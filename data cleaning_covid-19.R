library(dplyr)
data1<- read.csv("final3data.csv")%>%
  mutate(State=as.character(State))
data1


data2<- read.csv("us_states_covid19_daily.csv")[1:56,]
data2<-data2[,2:4]
data2=data2%>%
  mutate(all=positive+negative)%>%
  mutate(confirmed_rate=100*positive/all)%>%
  rename(State=state)%>%
  mutate(State=as.character(State))
data2


schdata=data1%>%
  left_join(data2,by="State")%>%
  select(-positive,-negative,-all)%>%
  mutate(AvgCost=as.numeric(AvgCost),
       Earn=as.numeric(Earn))%>%
  mutate(CrimeRate=CrimeRate/10)%>%
  mutate(rank_level=ifelse(ForbesRank<30,"high","low"))%>%
  mutate(rank_level=ifelse(30<ForbesRank&ForbesRank<80,"median",rank_level))

save(schdata, file = "schdata.Rdata")

load(file="schdata.Rdata")
schdata


