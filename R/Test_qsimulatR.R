library(qsimulatR)
nbits = 3
x <- qstate(nbits=nbits)
plot(x, qubitnames=paste0("qbit",1:nbits))

y <- H(1) * x



y
plot(y)
