library(DCchoice)

data(NaturalPark, package = "Ecdat")
NaturalPark$R1 <- ifelse(substr(NaturalPark$answers, 1, 1) == "y", 1, 0)
NaturalPark$R2 <- ifelse(substr(NaturalPark$answers, 2, 2) == "y", 1, 0)
NaturalPark$bid2 <- ifelse(NaturalPark$R1 == 1, NaturalPark$bidh, NaturalPark$bidl)

fmdb <- R1 + R2 ~ sex + age + income | log(bid1) + log(bid2)
NPdb <- dbchoice(fmdb, data = NaturalPark, dist = "log-normal")
NPdb
NPdbs <- summary(NPdb)

object <- NPdb

NPdbs$meanWTP
# Table 2.1
exp(-Xb/b)*exp(Xb/(2*b^2))
# Carson Hanemann (2005)
exp(-Xb/b)*exp(1/(2*b^2))
# Hanemann and Kanninen (1995)
exp(-Xb/b)*exp(1/b^2)



