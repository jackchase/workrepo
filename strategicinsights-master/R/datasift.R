
library(RPostgreSQL)
install.packages("RPostgreSQL")

library(dplyr)


db <- src_postgres(host="ganesha", user="thoth")


ds <- tbl(db, "ds002")

ds %>% select(stream) %>% distinct()

ds <- filter(ds, stream=="8d92de3c5fb23b422c5e677897f1c4d1")
ds <- filter(ds, stream=="29f6a14a346497952882135531d1d15c")

nrow(ds)
ds <- collect(ds)




dss <- select(ds, interaction_author_username, interaction_content, twitter_created_at)


