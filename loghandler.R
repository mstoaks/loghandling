# Getting an error when trying to use scp from inside RStudio (not supported or enabled by libcurl). So we'll download to the server first
print("downloading prod server 1 log file")
system("scp -i ~/.ssh/id_rsa gsbpublic.prod@ded-1528.prod.hosting.acquia.com:/var/log/sites/gsbpublic.prod/logs/ded-1528/access.log 1528access.log")
print("downloading log server 2 log file")
system("scp -i ~/.ssh/id_rsa gsbpublic.prod@ded-1529.prod.hosting.acquia.com:/var/log/sites/gsbpublic.prod/logs/ded-1529/access.log 1529access.log")


system("ssh gsbpublic.prod@ded-1528.prod.hosting.acquia.com ls /var/log/sites/gsbpublic.prod/logs/ded-1528; ls ~")
system("ssh gsbpublic.prod@ded-1528.prod.hosting.acquia.com ls /var/log/sites/gsbpublic.prod/logs/ded-1528")
system("ssh gsbpublic.prod@ded-1528.prod.hosting.acquia.com cd /var/log/sites/gsbpublic.prod/logs/ded-1528")

system("scp -i ~/.ssh/id_rsa gsbpublic.prod@ded-1528.prod.hosting.acquia.com:/var/log/sites/gsbpublic.prod/logs/ded-1528/access-errors-august.log 1528access-errors-august.log")

#now read them into R

aug <- system("ssh gsbpublic.prod@ded-1529.prod.hosting.acquia.com zcat /var/log/sites/gsbpublic.prod/logs/ded-1529/access.log-201508*.gz | awk '{ if ($9 >= 400 && $9 <=600) print }' ")

readAccessLog <- function(logfile) {
    accesslog <- read.table(logfile, quote="\"", comment.char="")
    access <- mutate(accesslog, V4 = str_sub(V4, 2,12))
    access <- select(access, ip=V1, date=V4, request=V6, status=V7, requesttime=V8, referer=V9, useragent=V10)
    return(access)
}

allAccessLogs <- function() {
    biglog <- data.frame()
    for (log in system("ls ./logs/1528/access/access.log-201508*.gz")) {
        accesslog <- read.table(log, quote="\"", comment.char="")
        access <- mutate(accesslog, V4 = str_sub(V4, 2,12))
        access <- select(access, ip=V1, date=V4, request=V6, status=V7, requesttime=V8, referer=V9, useragent=V10)
        #bind_rows(biglog,access)
        print (log)
    }
}

l1528 <- read.table("~/Projects/LogHandler/working/1528.out", quote="\"", comment.char="", stringsAsFactors=FALSE)
l1529 <- read.table("~/Projects/LogHandler/working/1529.out", quote="\"", comment.char="", stringsAsFactors=FALSE)

#DOESN'T WORK - mutate(l1528, typeOfLog=unlist(str_split(V1, ".log-"))[1])
#DOESN'T WORK - mutate(l1528, typeOfLog=unlist(str_split(V1, ".log-"))[1])

#l1528 %>% mutate(typeOfLog=unlist(str_split(V1, ".log-"))[1]) %>% mutate(date=unlist(str_split(V1, ".log-"))[2])

separate(l1528, V1, c("type", "date"), sep=".log-")

#get rid of newrelic logs as they are useless
l1528 <- filter(l1528, !str_detect(V1, "newrelic"))
l1529 <- filter(l1529, !str_detect(V1, "newrelic"))

l1528 <- l1528 %>% separate(V1, c("type", "date"), sep=".log-") %>% mutate(date=as.Date(date, format="%Y%m%d")) %>% rename(events = V2)
l1529 <- l1529 %>% separate(V1, c("type", "date"), sep=".log-") %>% mutate(date=as.Date(date, format="%Y%m%d")) %>% rename(events = V2)

#add server column
l1528 <- mutate(l1528, server=1528)
l1529 <- mutate(l1529, server=1529)

#combine both server's data
lALL <- bind_rows(l1528, l1529)

#reorder the columns
lALL <- lALL[c("date", "server", "type", "events")]


library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

l1528watchdog <- filter(l1528, type=="drupal-watchdog")
l1529watchdog <- filter(l1529, type=="drupal-watchdog")

qplot(x=date, y=events, data = l1528, facets = .~type)

#fpm-error, fpm-access, error, drupal-watchdog, drupal-requests, access, php-errors
qplot(x=date, y=events, data=filter(l1528, type=="access"))
qplot(x=date, y=events, data=filter(l1528, type=="error"))
qplot(x=date, y=events, data=filter(l1528, type=="drupal-requests"))
qplot(x=date, y=events, data=filter(l1528, type=="drupal-watchdog"))
qplot(x=date, y=events, data=filter(l1528, type=="php-errors"))
qplot(x=date, y=events, data=filter(l1528, type=="fpm-access"))
qplot(x=date, y=events, data=filter(l1528, type=="fpm-error"))

qplot(x=date, y=events, data=filter(l1529, type=="access"))
qplot(x=date, y=events, data=filter(l1529, type=="error"))
qplot(x=date, y=events, data=filter(l1529, type=="drupal-requests"))
qplot(x=date, y=events, data=filter(l1529, type=="drupal-watchdog"))
qplot(x=date, y=events, data=filter(l1529, type=="php-errors"))
qplot(x=date, y=events, data=filter(l1529, type=="fpm-access"))
qplot(x=date, y=events, data=filter(l1529, type=="fpm-error"))

#using the master table with both servers data
qplot(x=date, y=events, data=filter(lALL, type=="access", server==1528)) + geom_line()
qplot(x=date, y=events, data=filter(lALL, type=="drupal"), facets=.~server) + geom_line()
#add a ylim
qplot(x=date, y=events, ylim=c(0,1000), data=filter(lALL, type=="php-errors"), facets=.~server) + geom_line()


#get the latest dates from lALL
max(lALL$date)



#to read in a new file
l1528publicprod <- read.table("~/Projects/LogHandler/working/logs/1528publicprod.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
l1528publicprod <- l1528publicprod %>% separate(V1, c("type", "date"), sep=".log-") %>% mutate(date=as.Date(date, format="%Y%m%d")) %>% rename(events = V2) %>% mutate(server=1528)
#append to 
lALL <- lALL.orig
lALL <- bind_rows(lALL, l1528publicprod)
l1529publicprod <- read.table("~/Projects/LogHandler/working/logs/1529publicprod.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
l1529publicprod <- l1529publicprod %>% separate(V1, c("type", "date"), sep=".log-") %>% mutate(date=as.Date(date, format="%Y%m%d")) %>% rename(events = V2) %>% mutate(server=1529)
lALL <- bind_rows(lALL, l1529publicprod)


#INSTEAD WE WILL INCLUDE THE SERVER NAME IN THE FILE SO DON"T NEED THE MUTATE
temp %>% separate(V2, c("type", "date"), sep=".log-") %>% mutate(date=as.Date(date, format="%Y%m%d")) %>% rename(events = V3) %>% mutate(server = str_sub(V1, 1, 8)) %>% select(server, date, type, events)




