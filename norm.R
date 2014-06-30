#args<-commandArgs(trailingOnly=T)
args <- c("D:/VBA/Tecan", 0)
v_max <- 200
v_min <- 10
v_dead <- 10
c_final <- args[2]

data <- na.omit(read.table(paste(args[1],"data.txt",sep="/"),head=T,fill=T))
c <- data$c
p <- data$p
v <- data$v

if (min(v)<v_min+v_dead) {
      "The volume of certain wells are too small."
}

#Find c_final
if (c_final == 0) {
    c_final <- min(c)
    v1 <- v - v_dead
    while(c_final>0){
        v2 <- mapply(function(x,y){
            return (x*(y-c_final)/c_final)
        },v1,c)
        if (min(v2) >= v_min) break
        print (v2)
        c_final <- c_final - 100
    }
}

#c_final <- min(c*(v-v_dead)/(v-v_dead+v_min))

#Calc v1 and v2
c_threshold <- c_final*(v_max+v_min)/v_min
v1 <- mapply(function(x,y){
      #For high concentration, v1 = v_min
      if (x>=c_threshold) {
            return (v_min)
      #For middle concentration, keep v2 = v_max, then v1 = v_max*c_final/(x-c_final)
      #For low concentration, v1 = v-v_dead
      } else {
            return (min(y-v_dead,v_max*c_final/(x-c_final)))
      }
},c,v)
v2 <- mapply(function(x,y){
      return (x*(y-c_final)/c_final)
},v1,c)

c_test <- v1*c/(v1+v2)
v_test <- v1+v2
v1_test <- v1<=v-v_dead && v1>=v_min
v2_test <- v2>=v_min
work <- data.frame("Position"=p,
                   "Source_con"=round(c,1),
                   "Source_vol"=v,
                   "Solution_vol"=round(v1,1),
                   "Solvent_vol"=round(v2,1),
                   "Final_con"=round(c_test,1),
                   "Final_vol"=round(v_test,1),
                   v1_test,
                   v2_test)
write.csv(work,file=paste(args[1],"Work.csv",sep="/"),row.names=F,quote=F)

Col1 <- rep("Source1",length(v1))
Col2 <- p
Col3 <- rep("Dest1",length(v1))
Col4 <- p
Col5 <- round(v1,1)
Worklist <- data.frame("Source Labware Lable" = Col1,
                       "Source Position" =  Col2,
                       "Destination Labware Lable" = Col3,
                       "Destination Position" = Col4,
                       "Volume" = Col5,
                       check.names = F)
Worklist <- Worklist[order(Worklist[2]),]

write.csv(Worklist,file=paste(args[1],"Worklist.csv",sep="/"),row.names=F,quote=F)

Col2 <- integer()
Col4 <- integer()
Col5 <- numeric()

for (i in c(1:length(v2))) {
      r <- (v2[i]-0.00001)%/%v_max
      #Col2 <- c(Col2,rep((p[i]-1)%%8+1,r+1))
      
      Col4 <- c(Col4,rep(p[i],r+1))
      if (r>=1) {
            Col5 <- c(Col5,rep(v_max,r-1),rep((v_max+(v2[i]-1)%%v_max+1)/2,2))
      } else {
            Col5 <- c(Col5,v2[i])
      }  
} 
Col1 <- rep("Source",length(Col4))
Col2 <- rep(c(2:7),length.out=length(Col4))
Col3 <- rep("Dest1",length(Col4))
Col5 <- round(Col5,1)
Worklist1 <- data.frame("Source Labware Lable" = Col1,
                         "Source Position" =  Col2,
                         "Destination Labware Lable" = Col3,
                         "Destination Position" = Col4,
                         "Volume" = Col5,
                         check.names = F)
Worklist1 <- Worklist1[order(Worklist1[4]),]
write.csv(Worklist1,file=paste(args[1],"Worklist1.csv",sep="/"),row.names=F,quote=F)
