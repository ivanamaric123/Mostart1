vrata<-c("A", "B", "C")


info<-c()

for (i in 1:1000){
nagrada<-sample(vrata)[1]
izbor<-sample(vrata)[1]
izbor_voditelja<-sample(vrata[which(vrata != nagrada & vrata != izbor)])[1]
promjena_izbora<-sample(vrata[which(vrata != izbor & vrata !=izbor_voditelja)])
if (izbor==nagrada)(info=c(info, "pobjeda bez promjene"))
if (promjena_izbora==nagrada)(info=c(info, "pobjeda s promjenom"))
}
cat("broj pobjeda bez promjene=", length(which(info=="pobjeda bez promjene")))
cat("broj pobjeda s promjenom=", length(which(info=="pobjeda s promjenom")))
