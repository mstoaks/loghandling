
watchdog <- read.delim("~/Projects/LogHandler/working/logs/1528/watchdog/watchdog.log", header=FALSE, stringsAsFactors=FALSE, sep="|")

#get rid of the request idsn - create a new column without the request ID
watchdog$msg <- str_replace_all(watchdog$V9, "request_id.*$", "")
watchdog <- rename(watchdog, type = V3)


#lets get the types of errors
wdtypes <- watchdog %>% count(type)
wdtypes <- rename(wdtypes, count=n)
wdtypes <- arrange(wdtypes, desc(count))
write.table(wdtypes, "watchdog-error-types-20150916.txt", quote=FALSE, sep=" , ", row.names=FALSE)

#lets look at the biggies - first php
wdphpentries <- filter(watchdog, type=="php")
wdphpmsgs <- wdphpentries %>% count(msg)
wdphpmsgs <- rename(wdphpmsgs, count=n)
wdphpmsgs <- arrange(wdphpmsgs, desc(count))
write.table(wdphpmsgs, "watchdog-php-errors-20150916.txt", quote=FALSE, sep=" , ", row.names=FALSE)

#then page not found
wdPNFentries <- filter(watchdog, type=="page not found")
wdPNFmsgs <- wdPNFentries %>% count(msg)
names(wdPNFmsgs) = c("Page-Not-Found", "Count")
wdPNFmsgs <- arrange(wdPNFmsgs, desc(Count))
write.table(wdPNFmsgs, "watchdog-pagenotfound-errors-20150916.txt", quote=FALSE, sep=" , ", row.names=FALSE)

#then access denied
wdADentries <- filter(watchdog, type=="access denied")
wdADmsgs <- wdADentries %>% count(msg)
names(wdADmsgs) = c("Access-Denied-Target", "count")
wdADmsgs <- arrange(wdADmsgs, desc(count))
write.table(wdADmsgs, "watchdog-accessdenied-errors-20150916.txt", quote=FALSE, sep="\t", row.names=FALSE)
