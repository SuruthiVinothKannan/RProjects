ID<-c(1,1,3)
Trans<-c(20,21,30)
transaction<-data.frame(ID,Trans)
demo<-data.frame(ID=c(1,2,3),Address=c("a","b","c"))
library(dplyr)
new_left=left_join(demo,transaction,"ID")

new_right=right_join(demo,transaction,"ID")

new_full=full_join(demo,transaction,"ID")

new_inner=inner_join(demo,transaction,"ID")
