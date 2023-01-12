joc <- function(s, castig) 
  {
  miza <- s
  while (miza > 0 & miza < castig) 
    {
    sanse <- sample(c(-1, 1), 1, miza = c(0.5, 0.5))
    miza <- miza + sanse
  }
  if (miza == 0)
    return(1)
  else 
    return(0)
}   

s <- 23
castig <- 1000
n <- 100
contor <- 0
total<- 0
while (contor < n){
  x <- joc(s,castig)
  total <- total + x
  contor <- contor + 1
}

Raspuns <- total/n