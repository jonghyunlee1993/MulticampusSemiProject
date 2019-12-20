setwd("~/GitRepo/Multicampus_semi/local_people/")
load("weekend.Rdata")

a = levels(res$남10)[1:2]

for (b in 5:length(res)){
  print(names(res)[b])
  print(sum(res[, b] %in% a) / length(res[, b]))
}


