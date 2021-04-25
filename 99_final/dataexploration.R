library(ggplot2)
library(forcats)
library(dplyr)
options(scipen=10000)

#Data Import
kdd_header = read.csv("99_final/data/header.csv",header=FALSE)
kdd_header = as.character(unlist(kdd_header[1,]))

kdd = read.csv("99_final/data/kddcup.csv", 
               header=FALSE,
               stringsAsFactors=TRUE,
               col.names=kdd_header)

# Basic Cleaning

## Clean event column values
levels(kdd$eventType) = gsub("\\.", "", levels(kdd$eventType))

## Filter columns of same value, one column
apply(kdd, 2, function(x) length(unique(x)))
kdd$num_outbound_cmds = NULL

## Columns with nulls, no columns
colSums(is.na(kdd))


# Data Exploration of main non-Binary Numeric Data
kdd = kdd %>% mutate(eventCategory = case_when(eventType %in% c("back","land","neptune",
                                                                "pod","smurf","teardrop") ~ "dos",
                                               eventType %in% c("buffer_overflow","loadmodule",
                                                                "perl","rootkit") ~ "u2r",
                                               eventType %in% c("ftp_write","guess_passwd","imap",
                                                                "multihop","phf","spy","warezclient",
                                                                "warezmaster") ~ "r21",
                                               eventType %in% c("ipsweep","nmap","portsweep",
                                                                "satan") ~ "probe",
                                               TRUE ~ "normal"))

# Data Exploration of Categoric Data

## protocol_type
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(protocol_type)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("protocol type") +
  ylab("count of protocol types") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

### service
as.data.frame(sort(table(kdd$service)))

### flag
as.data.frame(sort(table(kdd$flag)))
ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(flag)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("flag type") +
  ylab("count of flags") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()


## eventType
as.data.frame(sort(table(kdd$eventType)))

ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(eventType)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("event type") +
  ylab("count of event types") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()

## eventCategory
as.data.frame(sort(table(kdd$eventCategory)))

ggplot(kdd, mapping=aes(x=fct_rev(fct_infreq(eventCategory)))) +
  geom_bar(stat="count", fill="steelblue") +
  xlab("event category") +
  ylab("count of event category") +
  scale_y_continuous(n.breaks=10) +
  theme_minimal() +
  coord_flip()


