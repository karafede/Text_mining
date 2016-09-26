
#install the RISmed package
# install.packages("RISmed")
library(RISmed)
library(maps)      
library(mapdata)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("C:/Data_mining")

# "GreenLight" AND "Laser" AND "Prostate"

#now let's look up this dude called Dave Tang
res1 <- EUtilsSummary('GreenLight', type='esearch', db='pubmed')
res2 <- EUtilsSummary('Laser', type='esearch', db='pubmed')
res3 <- EUtilsSummary('Prostate', type='esearch', db='pubmed')

summary(res1)
summary(res2)
summary(res3)

#limit by date
# res2 <- EUtilsSummary('dave tang', type='esearch', db='pubmed', mindate='2012', maxdate='2012')
# summary(res2)
# QueryId(res2)


##################################################################################
##################################################################################

# first how many total articles containing retrotransposon
# res3 <- EUtilsSummary('retrotransposon', type='esearch', db='pubmed', mindate='2012', maxdate='2012')
# summary(res3)

# auths <- Author(EUtilsGet(res3))
auths1 <- Author(EUtilsGet(res1))
auths2 <- Author(EUtilsGet(res2))
auths3 <- Author(EUtilsGet(res3))

showMethods("Author") 

Countries1 <- Country(EUtilsGet(res1))
Countries2 <- Country(EUtilsGet(res2))
Countries3 <- Country(EUtilsGet(res3))

Authors_Country1 <- data.frame()

for (i in 1:length(Countries1)) {
d <- cbind(auths1[[i]][,] , Countries1[i],i)
Authors_Country1 <- rbind(d, Authors_Country1)
}
colnames(Authors_Country1)[6] <- "co_index1"


Authors_Country2 <- data.frame()

for (i in 1:length(Countries2)) {
  d <- cbind(auths2[[i]][,] , Countries2[i], i)
  Authors_Country2 <- rbind(d, Authors_Country2)
}
colnames(Authors_Country2)[6] <- "co_index2"



Authors_Country3 <- data.frame()

for (i in 1:length(Countries3)) {
  d <- cbind(auths3[[i]][,] , Countries3[i], i)
  Authors_Country3 <- rbind(d, Authors_Country3)
}
colnames(Authors_Country3)[6] <- "co_index3"

# AAA <- cbind( auths[[1]][,] , Countries[1])

# GreenLight------------------------------

Last1 <-sapply(auths1, function(x)paste(x$LastName))
auths1 <- as.data.frame(sort(table(unlist(Last1)), dec=TRUE))
colnames(auths1)<-c("LastName", "counts1")

# auths <- cbind(Author = rownames(auths), auths)
# rownames(auths) <- NULL

Authors_Country1 <- Authors_Country1 %>%
  join(auths1, by = "LastName")
Authors_Country1$term1 <- "GreenLight"
colnames(Authors_Country1)[5] <- "Countries1"



# Laser------------------------------

Last2 <-sapply(auths2, function(x)paste(x$LastName))
auths2 <- as.data.frame(sort(table(unlist(Last2)), dec=TRUE))
colnames(auths2)<-c("LastName", "counts2")


Authors_Country2 <- Authors_Country2 %>%
  join(auths2, by = "LastName")
Authors_Country2$term2 <- "Laser"
colnames(Authors_Country2)[5] <- "Countries2"


# Prostate------------------------------

Last3 <-sapply(auths3, function(x)paste(x$LastName))
auths3 <- as.data.frame(sort(table(unlist(Last3)), dec=TRUE))
colnames(auths3)<-c("LastName", "counts3")


Authors_Country3 <- Authors_Country3 %>%
  join(auths3, by = "LastName")
Authors_Country3$term3 <- "Prostate"
colnames(Authors_Country3)[5] <- "Countries3"


#########################################################################


 # join_all <- left_join(Authors_Country1, Authors_Country2, by=c("LastName", "ForeName")) %>%
 #   left_join(., Authors_Country3, by= c("LastName", "ForeName")) 

join_all <- join_all(list(Authors_Country1, Authors_Country2, Authors_Country3), by = c("LastName", "ForeName"), type='left')


# write.csv(join_all, "PubMed_data.csv")
# remove Initials, order, Initials, order in xls

join_all <- read.csv("PubMed_data_clean.csv")

# remove duplicates
join_all <- join_all %>%
  distinct(LastName, ForeName,
           counts1 ,counts2, counts3,
           co_index1, co_index2, co_index3,
           Countries1, Countries2, Countries3) 

join_all$author <- paste(join_all$LastName, join_all$ForeName)


# categorise data by octiles

RANKING_authors <- join_all %>% 
  mutate(octile_counts1 = ntile(counts1, 8)) %>% 
  mutate(octile_counts2 = ntile(counts2, 8)) %>% 
  mutate(octile_counts3 = ntile(counts3, 8)) %>% 
  ungroup()


### Subset highest Octile 8 for each count ####

counts1_max <- RANKING_authors %>%
  subset(octile_counts1 == 8) %>%
    group_by(author) %>%
  summarise(conta1 = sum(counts1, na.rm = TRUE))

counts2_max <- RANKING_authors %>%
  subset(octile_counts2 == 8) %>%
  group_by(author) %>%
  summarise(conta2 = sum(counts2, na.rm = TRUE))

counts3_max <- RANKING_authors %>%
  subset(octile_counts3 == 8) %>%
  group_by(author) %>%
  summarise(conta3 = sum(counts3, na.rm = TRUE))


################################################################################################

jpeg("Greenlight_counts_tot.jpg",
     quality = 100, bg = "white", res = 200, width = 9, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

# counts1_max$author <- factor(counts1_max$author, levels = counts1_max$author[order(-counts1_max$counts1)])

counts1_max <- arrange(counts1_max, -conta1) 
counts1_max <- transform(counts1_max, author=reorder(author, -conta1) ) 
# counts1_max$author <- factor(counts1_max$author, levels=unique(as.character(counts1_max$author)) )

p <- ggplot(data = counts1_max,
            aes(author, conta1, fill = author)) +
  theme_bw() + 
  geom_bar(stat = "identity") + guides(fill=FALSE) +
 # scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=10,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("Total number of articles") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=15)) +
#        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
 # xlab("respondent source (cell)") +          
  theme(
        axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) +
#  geom_text(aes(label = paste(round(perc_weight), "%", sep = ""), y = pos), size = 8) +
  ggtitle("search term GreenLight (last octile of the research)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=15))
p


par(oldpar)
dev.off()




jpeg("Laser_counts_tot.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

counts2_max <- arrange(counts2_max, -conta2) 
counts2_max <- transform(counts2_max, author=reorder(author, -conta2) ) 

p <- ggplot(data = counts2_max,
            aes(author, conta2, fill = author)) +
  theme_bw() + 
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  # scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=10,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("Total number of articles") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=15)) +
  #        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  # xlab("respondent source (cell)") +          
  theme(
        axis.text.x  = element_text(angle=90, vjust=0.5, size=18)) +
  #  geom_text(aes(label = paste(round(perc_weight), "%", sep = ""), y = pos), size = 8) +
  ggtitle("search term Laser (last octile of the research)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=15))
p


par(oldpar)
dev.off()




jpeg("Prostate_counts_tot.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

counts3_max <- arrange(counts3_max, -conta3) 
counts3_max <- transform(counts3_max, author=reorder(author, -conta3) ) 

p <- ggplot(data = counts3_max,
            aes(author, conta3, fill = author)) +
  theme_bw() + 
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  # scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=10,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("Total number of articles") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=15)) +
  #        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  # xlab("respondent source (cell)") +          
  theme(
    axis.text.x  = element_text(angle=90, vjust=0.5, size=18)) +
  #  geom_text(aes(label = paste(round(perc_weight), "%", sep = ""), y = pos), size = 8) +
  ggtitle("search term Prostate (last octile of the research)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=15))
p


par(oldpar)
dev.off()



#################################################################################################################
# between co-authors in each group term

join_all_co_index1 <- join_all %>%
  group_by(co_index1,
           author) %>%
summarise(sum1 = sum(counts1, na.rm = TRUE)) %>%
  mutate(octile_counts1 = ntile(sum1, 8))
  

co_index1_max <- join_all_co_index1 %>%
  subset(octile_counts1 == 8) %>%
  group_by(author) %>%
  summarise(summa1 = sum(sum1, na.rm = TRUE))




jpeg("Greenlight_cou_authors_counts_tot.jpg",
     quality = 100, bg = "white", res = 200, width = 9, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

co_index1_max <- arrange(co_index1_max, -summa1) 
co_index1_max <- transform(co_index1_max, author=reorder(author, -summa1) ) 

p <- ggplot(data = co_index1_max,
            aes(author, summa1, fill = author)) +
  theme_bw() + 
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  # scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=10,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("number of co-authored articles") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=15)) +
  #        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  # xlab("respondent source (cell)") +          
  theme(
    axis.text.x  = element_text(angle=90, vjust=0.5, size=13)) +
  #  geom_text(aes(label = paste(round(perc_weight), "%", sep = ""), y = pos), size = 8) +
  ggtitle("co-authored articles - search term GreenLight (last octile of the research)") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=15))
p


par(oldpar)
dev.off()




########################################

join_all_co_index2 <- join_all %>%
  group_by(co_index2,
           author) %>%
  summarise(sum2 = sum(counts2, na.rm = TRUE)) %>%
  mutate(octile_counts2 = ntile(sum2, 8))


co_index2_max <- join_all_co_index2[!is.na(join_all_co_index2$co_index2),] %>%
group_by(author) %>%
  summarise(summa2 = sum(sum2, na.rm = TRUE))

co_index2_max <- arrange(co_index2_max, -summa2) 
co_index2_max <- transform(co_index2_max, author=reorder(author, -summa2) ) 

jpeg("Laser_co_authors_counts_tot.jpg",
     quality = 100, bg = "white", res = 200, width = 9, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


p <- ggplot(data = co_index2_max,
            aes(author, summa2, fill = author)) +
  theme_bw() + 
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  # scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=10,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("number of co-authored articles") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=15)) +
  #        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  # xlab("respondent source (cell)") +          
  theme(
    axis.text.x  = element_text(angle=90, vjust=0.5, size=13)) +
  #  geom_text(aes(label = paste(round(perc_weight), "%", sep = ""), y = pos), size = 8) +
  ggtitle("co-authored articles - search term Laser") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=15))
p


par(oldpar)
dev.off()



#######


join_all_co_index3 <- join_all %>%
  group_by(co_index3,
           author) %>%
  summarise(sum3 = sum(counts3, na.rm = TRUE)) %>%
  mutate(octile_counts3 = ntile(sum3, 8))


co_index3_max <- join_all_co_index3[!is.na(join_all_co_index3$co_index3),] %>%
  group_by(author) %>%
  summarise(summa3 = sum(sum3, na.rm = TRUE))


jpeg("Prostate_co_authors_counts_tot.jpg",
     quality = 100, bg = "white", res = 200, width = 9, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

co_index3_max <- arrange(co_index3_max, -summa3) 
co_index3_max <- transform(co_index3_max, author=reorder(author, -summa3) ) 

p <- ggplot(data = co_index3_max,
            aes(author, summa3, fill = author)) +
  theme_bw() + 
  geom_bar(stat = "identity") + guides(fill=FALSE) +
  # scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=10,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                                            # Remove x-axis label
  ylab("number of co-authored articles") +                                                        # Set y-axis label
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=15)) +
  #        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  # xlab("respondent source (cell)") +          
  theme(
    axis.text.x  = element_text(angle=90, vjust=0.5, size=13)) +
  #  geom_text(aes(label = paste(round(perc_weight), "%", sep = ""), y = pos), size = 8) +
  ggtitle("co-authored articles - search term Prostate") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",  size=15))
p


par(oldpar)
dev.off()


#########################################################################################################

# look at relation between authors and search term

join_all_co_auth <- join_all %>%
  filter(counts1,
         counts2,
         counts3) %>%
  group_by(author) %>%
  summarise(GreenLight = sum(counts1, na.rm = TRUE),
            Laser = sum(counts2, na.rm = TRUE),
            Prostate = sum(counts3, na.rm = TRUE))

join_all_co_auth <- cbind(join_all_co_auth[1], stack(join_all_co_auth[2:4]))
colnames(join_all_co_auth)[1] <- "Author"
colnames(join_all_co_auth)[2] <- "sum_papers"    #sum of papers with same authors & co-authors
join_all_co_auth$Author <- as.factor(join_all_co_auth$Author)


jpeg("relation_co_authors_terms.jpg",
     quality = 100, bg = "white", res = 200, width = 12, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

join_all_co_auth <- arrange(join_all_co_auth, -sum_papers) 
join_all_co_auth <- transform(join_all_co_auth, author=reorder(Author, -sum_papers) ) 

q <- ggplot(data = join_all_co_auth, 
            aes(Author, sum_papers, fill = Author)) +
  theme_bw() + 
  geom_bar(stat = "identity") + facet_grid(. ~ ind) + guides(fill=FALSE) +
  theme(strip.text.x = element_text(size = 22, colour = "black")) +
#  scale_fill_manual(values=c("#7f7fff", "#E69F00", "#a6a6a6", "#66b266")) +
  theme( strip.text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=12,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  
  ylab("number of co-authored articles") +            
  theme(axis.title.y = element_text(face="bold", colour="#990000", size=15),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  xlab("respondent source (cell)") +           
  theme(
        axis.text.x  = element_text(angle=90, vjust=0.5, size=15)) +
#  geom_text(aes(label = paste(round(contribution), "%", sep = ""), y = pos), size = 6) +
  ggtitle("Authors and co-authors who have papers containing all the three keywords") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 17))
q



par(oldpar)
dev.off()

















#######################################################################################
####### Mapping #####################################################################

ddf = read.table(text="
country value
USA 10
UK 30
Sweden 50
Japan 70
China 90
Germany 100
France 80
Italy 60
Nepal 40
Nigeria 20
", header=T)

library(rworldmap)

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="country")

mapCountryData(spdf, nameColumnToPlot="value", catMethod="fixedWidth")




#######################################################################################
#######################################################################################








map('worldHires')
map('world2Hires')

map.scale(160,0,relwidth = 0.15, metric = TRUE, ratio = TRUE)
map.scale(160,-40,relwidth = 0.15, metric = TRUE, ratio = TRUE)

map('worldHires','Italy')
map('worldHires', as.list(Countries))
map('worldHires', Countries)

#if you only want the number of articles
QueryCount(res3)
# [1] 8123

#tally each year beginning at 1970
#In order not to overload the E-utility servers, NCBI recommends that users post no more than three
#URL requests per second and limit large jobs to either weekends or between 9:00 PM and 5:00 AM
#Eastern time during weekdays. Failure to comply with this policy may result in an IP address being
#blocked from accessing NCBI.

tally <- array()
x <- 1
for (i in 1970:2013){
  Sys.sleep(1)
  r <- EUtilsSummary('retrotransposon', type='esearch', db='pubmed', mindate=i, maxdate=i)
  tally[x] <- QueryCount(r)
  x <- x + 1
}

names(tally) <- 1970:2013
max(tally)
# [1] 573

barplot(tally, las=2, ylim=c(0,600), main="Number of PubMed articles containing retrotransposon")

###########################################################################################
###########################################################################################

transposon <- array()
x <- 1
for (i in 1970:2013){
  Sys.sleep(1)
  r <- EUtilsSummary('transposon', type='esearch', db='pubmed', mindate=i, maxdate=i)
  transposon[x] <- QueryCount(r)
  x <- x + 1
}

names(transposon) <- 1970:2013
max(transposon)
# [1] 1634

barplot(transposon, las=2, ylim=c(0,2000), main="Number of PubMed articles containing transposon")

#######################################################################################
#######################################################################################

test <- EUtilsSummary('', type='esearch', db='pubmed', mindate=1970, maxdate=1970)
summary(test)
# Query:
#   1970[EDAT] : 1970[EDAT] 
# 
# Result count:  218690

#number of articles each year
total <- array()
x <- 1
for (i in 1970:2013){
  Sys.sleep(1)
  r <- EUtilsSummary('', type='esearch', db='pubmed', mindate=i, maxdate=i)
  total[x] <- QueryCount(r)
  x <- x + 1
}

names(total) <- 1970:2013
max(total)
#  17657

barplot(total, las=2, ylim=c(0,1000000), main="Number of PubMed articles each year")

#########################################################################################
#########################################################################################

tally_norm <- tally / total
transposon_norm <- transposon / total
trna_norm <- trna / total

par(mfrow=c(1,3))
barplot(tally_norm, las=2)
barplot(transposon_norm, las=2)
barplot(trna_norm, las=2)
#reset
par(mfrow=c(1,1))


