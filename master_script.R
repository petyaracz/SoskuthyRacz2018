#############################################################################################
# data and code for the paper
#############################################################################################

try(setwd("/Users/pracz/Work/Bristol/SoskuthyRacz2018/"))
library(arm)
library(lme4)
library(reshape2)
library(ggplot2)
library(stringr)
library(knitr)
library(effects)
library(dplyr)
library(Cairo)

#############################################################################################
# datasets
#############################################################################################

# filtered corpus of echo pairs
cica.corp <- read.csv('echo_pair_corpus.csv')

# cols
# summary echo-pair. format: base + reduplicant consonant
# behaviour reduplicant consonant
# O1->C2 segmental skeleton for base
# Rule_1a_O2_voi is O2 voiced?
# Rule_1b_O2_voiceless is O2 voiceless?
# Rule_2_C2_m is C2 "m"?
# Rule_3_O1_p is O1 "p"?
# Rule_4_O1_null is O1 empty?
# Rule_5a_C1_m is C1 "m"?
# Rule_5b_O2_m is O2 "m"?
# Rule_5_C1_O2_m is C1,O2 "m"?
# filledO1 is O1 filled
# voicedO2 is O2 voiced
# voiced.second.consonant is the second C voiced?
# active.voicedO2 is O2 a voiced obstruent?

# filtered experiment data
cica.long = read.csv('exp_data.csv')
cica.long$responseid = cica.long$anonid

# cols
# summary target form
# startdate start of trial
# endddate end of trial
# progress trial #
# overall.dur overall duration
# sex No = F Ferfi = M
# place place of residence for subject
# outcome selected response in trial
# side side of button clicked
# behaviour behaviour selected
# one.sided.subject is subject one-sided
# anonid/responseid anonymised subject id
# rest: see corpus


gcm.preds <- cica.long %>% dplyr::select(summary,gcm.big) %>% unique

gcm.preds <- mutate(gcm.preds, trs=recode(summary,
                                            `ácó`="aːtsoː",
                                            `agyor`="ɒɟor",
                                            `azsu`="ɒʒu",
                                            `cege`="tsɛgɛ",
                                            `csanyi`="tʃɒɲi",
                                            `csükő`="tʃykøː",
                                            `cségi`="tʃeːgi",
                                            `éce`="eːtsɛ",
                                            `ecsa`="ɛtʃɒ",
                                            `édeg`="eːdɛg",
                                            `ége`="eːgɛ",
                                            `éki`="eːki",
                                            `esző`="ɛsøː",
                                            `etyő`="ɛcøː",
                                            `fázsi`="faːʒi",
                                            `föke`="føkɛ",
                                            `getye`="gɛcɛ",
                                            `gótyi`="goːci",
                                            `gyudó`="ɟudoː",
                                            `hácog`="haːtsog",
                                            `ilász`="ilaːs",
                                            `inó`="inoː",
                                            `ityi`="ici",
                                            `kasó`="kɒʃoː",
                                            `kegya`="kɛɟɒ",
                                            `kücsi`="kytʃi",
                                            `kunor`="kunor",
                                            `láki`="laːki",
                                            `lászi`="laːsi",
                                            `öcő`="øtsøː",
                                            `odó`="odoː",
                                            `óli`="oːli",
                                            `óti`="oːti",
                                            `rudog`="rudog",
                                            `rüli`="ryli",
                                            `runya`="ruɲɒ",
                                            `szacsog`="sɒtʃog",
                                            `szece`="sɛtsɛ",
                                            `ucser`="utʃɛr",
                                            `udok`="udok",
                                            `ugó`="ugoː",
                                            `ükér`="ykeːr",
                                            `unyog`="uɲog",
                                            `úszog`="uːsog",
                                            `uzsó`="uʒoː",
                                            `vikér`="vikeːr",
                                            `zédál`="zeːdaːl",
                                            `zinya`="ziɲɒ"))

# so that "m" is unmarked and "b" is marked in stats & graphs
cica.long$behaviour <- factor(cica.long$behaviour, levels=c("m","b"))

#############################################################################################
# corpus study
#############################################################################################

# model
cica.corp <- cica.corp %>% mutate_if(is.logical, as.factor)

cica.corp.mod1 <- glm(behaviour ~ filledO1 * voiced.second.consonant, data=cica.corp, family="binomial")

# plot
cica.corp.props1 <- dplyr::count(cica.corp, filledO1, voiced.second.consonant, behaviour) %>% mutate(prop=prop.table(n)) 

preds <- allEffects(cica.corp.mod1)[[1]]
preds <- cbind(preds[[6]], invlogit(preds$fit), invlogit(preds$lower), invlogit(preds$upper))
colnames(preds)[3:5] <- c("behaviour","lower","upper")
preds$filledO1 <- ifelse(preds$filledO1=="TRUE", "filled O1", "empty O1")
preds$voiced.second.consonant <- ifelse(preds$voiced.second.consonant=="TRUE", "voiced 2nd C", "voiceless 2nd C")

cica.corp.ns <- aggregate(n ~ filledO1 + voiced.second.consonant, cica.corp.props1, FUN=sum)
cica.corp.ns$n.text <- paste("n =", cica.corp.ns$n)
cica.corp.ns$behaviour <- "b"

# start with raw data, superpose preds
ggplot(cica.corp.props1, aes(x=voiced.second.consonant, fill=behaviour)) + 
    geom_bar(aes(y=prop), stat="identity", position="stack") + 
    geom_text(data=cica.corp.ns, aes(label=n.text), y=0.05) + 
    facet_wrap(~filledO1) + 
    ggtitle(paste0("raw proportions and model predictions (n=",nrow(cica.corp),")")) +
    geom_point(data=preds, aes(x=voiced.second.consonant, y=behaviour, fill=NULL), size=3) +
    geom_errorbar(data=preds, aes(x=voiced.second.consonant, ymin=lower, ymax=upper, fill=NULL), width=0.2) + 
    theme_bw() +
    scale_fill_manual(values=c("firebrick3", "deepskyblue4"), name="echo\nbehaviour", labels=c("[m]","[b]")) +
    ylab("proportion of [b] vs [m]") + xlab("") + 
    theme(axis.title=element_text(size=14, face="bold"),
          axis.text=element_text(size=12),
          strip.text=element_text(size=12),
          legend.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=12),
          plot.title=element_text(size=14,face="bold"))
ggsave("corpus_props.pdf", width=8, heigh=4)

#############################################################################################
# experiment
#############################################################################################

# model
cica.long$filledO1.num <- as.numeric(cica.long$filledO1)
cica.long$filledO1.fact <- factor(cica.long$filledO1)
cica.long$m.left.num <- as.numeric(cica.long$m.left)
cica.long$m.left.fact <- factor(cica.long$m.left)
cica.long$voicedO2.num <- as.numeric(cica.long$voicedO2)
cica.long$voicedO2.fact <- factor(cica.long$voicedO2)

# mod.fuller <- glmer(behaviour ~ 
#                      filledO1.fact + voicedO2.fact +
#                      m.left.fact + 
#                      (filledO1.num + voicedO2.num | responseid) + 
#                      (1 | summary),
#             data=cica.long, family="binomial", control=glmerControl(optimizer="bobyqa"))
# save(mod.fuller, file = 'modfuller.rda')
load('modfuller.rda')

# plot
by.set.props <- cica.long %>% dplyr::count(filledO1, voicedO2, behaviour) %>% mutate(prop = prop.table(n)) 
by.set.props$filledO1 <- ifelse(by.set.props$filledO1, "filled O1", "empty O1")
by.set.props$voiced.second.consonant <- ifelse(by.set.props$voicedO2, "voiced 2nd C", "voiceless 2nd C")

preds <- Effect(c("filledO1.fact","voicedO2.fact"), mod.fuller)
preds <- cbind(preds[[6]], invlogit(preds$fit), invlogit(preds$lower), invlogit(preds$upper))
colnames(preds)[3:5] <- c("behaviour","lower","upper")
preds$filledO1 <- ifelse(preds$filledO1.fact=="TRUE", "filled O1", "empty O1")
preds$voiced.second.consonant <- ifelse(preds$voicedO2.fact=="TRUE", "voiced 2nd C", "voiceless 2nd C")

# needs by.set.props, generated earlier
ggplot(by.set.props, aes(x=voiced.second.consonant, fill=behaviour)) + 
    geom_bar(aes(y=prop), stat="identity", position="stack") + 
    facet_wrap(~filledO1) + 
    geom_point(data=preds, aes(x=voiced.second.consonant, y=behaviour, fill=NA), size=3) +
    geom_errorbar(data=preds, aes(x=voiced.second.consonant, ymin=lower, ymax=upper, fill=NA), width=0.2) +
    theme_bw() +
    ggtitle(paste0("raw proportions and model predictions")) +
    scale_fill_manual(values=c("deepskyblue4","firebrick3"), name="echo\nbehaviour", labels=c("[b]","[m]")) +
    ylab("proportion of [b] vs [m]") + xlab("") + 
    theme(axis.title=element_text(size=14, face="bold"),
          axis.text=element_text(size=12),
          strip.text=element_text(size=12),
          legend.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=12),
          plot.title=element_text(size=14,face="bold"))
ggsave("exp_props.pdf", width=8, height=4)

#############################################################################################
# variation across items
#############################################################################################

# model

# mod.fuller.new <- glmer(behaviour ~ 
#                      filledO1 + voicedO2 +
#                      m.left + 
#                      (1 + filledO1 + voicedO2 | responseid) + 
#                      (1 | summary),
#             data=cica.long, family="binomial", control=glmerControl(optimizer="bobyqa"))
# save(mod.fuller.new, file = 'modfullernew.rda')
load('modfullernew.rda')
# simulation

iterations = 10000
preds <- predict(mod.fuller.new, cica.long, re.form=~(1 + filledO1 + voicedO2 | responseid), type="response")
sim.matrix <- matrix(rep(0,iterations*48), nrow=48)
rownames(sim.matrix) <- names(tapply(cica.long$behaviour=="b", cica.long$summary, FUN=mean))

for (i in 1:iterations) {
  outcomes <- rbinom(n=preds, size=1, prob=preds)
  sim.matrix[,i] <- tapply(outcomes, cica.long$summary, FUN=mean)
  if (i %% 100 == 0) {cat("\r               \r", i)}
}

sim.pred <- apply(sim.matrix, 1, mean)
sim.lower <- apply(sim.matrix, 1, quantile, 0.025)
sim.upper <- apply(sim.matrix, 1, quantile, 0.975)

sim.dat <- data.frame(summary=rownames(sim.matrix), fit=sim.pred, lower=sim.lower, upper=sim.upper)

# plot

by.word.props <- cica.long %>% 
  dplyr::count(summary, filledO1, voicedO2, behaviour) %>% 
  mutate(prop = prop.table(n)) %>%
  filter(behaviour=="b") %>%
  dplyr::select(-behaviour, -n)

by.word.props <- left_join(by.word.props, sim.dat, by="summary")
by.word.props.lev <- unique(by.word.props$summary)
by.word.props.lev <- by.word.props.lev[with(by.word.props, order(filledO1, voicedO2, prop))]
by.word.props$summary <- factor(by.word.props$summary, levels=by.word.props.lev)

by.word.props$filledO1 <- ifelse(by.word.props$filledO1, "filled O1", "empty O1")
by.word.props$voicedO2 <- ifelse(by.word.props$voicedO2, "+voi O2", "–voi O2")
by.word.props$combined.pred <- paste(by.word.props$filledO1, by.word.props$voicedO2, sep=", ")
by.word.props$combined.pred <- factor(by.word.props$combined.pred, levels=c(
                                        "empty O1, +voi O2",
                                        "empty O1, –voi O2",
                                        "filled O1, +voi O2",
                                        "filled O1, –voi O2")
)

by.word.props <- mutate(by.word.props, trs=recode(summary,
                                            `ácó`="aːtsoː",
                                            `agyor`="ɒɟor",
                                            `azsu`="ɒʒu",
                                            `cege`="tsɛgɛ",
                                            `csanyi`="tʃɒɲi",
                                            `csükő`="tʃykøː",
                                            `cségi`="tʃeːgi",
                                            `éce`="eːtsɛ",
                                            `ecsa`="ɛtʃɒ",
                                            `édeg`="eːdɛg",
                                            `ége`="eːgɛ",
                                            `éki`="eːki",
                                            `esző`="ɛsøː",
                                            `etyő`="ɛcøː",
                                            `fázsi`="faːʒi",
                                            `föke`="føkɛ",
                                            `getye`="gɛcɛ",
                                            `gótyi`="goːci",
                                            `gyudó`="ɟudoː",
                                            `hácog`="haːtsog",
                                            `ilász`="ilaːs",
                                            `inó`="inoː",
                                            `ityi`="ici",
                                            `kasó`="kɒʃoː",
                                            `kegya`="kɛɟɒ",
                                            `kücsi`="kytʃi",
                                            `kunor`="kunor",
                                            `láki`="laːki",
                                            `lászi`="laːsi",
                                            `öcő`="øtsøː",
                                            `odó`="odoː",
                                            `óli`="oːli",
                                            `óti`="oːti",
                                            `rudog`="rudog",
                                            `rüli`="ryli",
                                            `runya`="ruɲɒ",
                                            `szacsog`="sɒtʃog",
                                            `szece`="sɛtsɛ",
                                            `ucser`="utʃɛr",
                                            `udok`="udok",
                                            `ugó`="ugoː",
                                            `ükér`="ykeːr",
                                            `unyog`="uɲog",
                                            `úszog`="uːsog",
                                            `uzsó`="uʒoː",
                                            `vikér`="vikeːr",
                                            `zédál`="zeːdaːl",
                                            `zinya`="ziɲɒ"))


p <- ggplot(by.word.props, aes(x=trs, y=prop)) + 
  facet_grid(~combined.pred, scales = "free", space = "free") +
  geom_bar(stat="identity",fill="deepskyblue3") +
  geom_errorbar(aes(ymax = upper, ymin = lower), position = position_dodge(), width = 0.5) +
  geom_point(aes(y=fit)) +
  ylim(c(0,1)) +
  ylab("proportion of [b] behaviour") + xlab("") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1, size=10),
        axis.text.y=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=14,face="bold"),
        strip.text=element_text(size=12),
        plot.title=element_text(size=14,face="bold")) +
  ggtitle(paste0("by-item raw proportions and simulated predictions"))


#############################################################################################
# patterns versus instances
#############################################################################################

# plot

cica.sum <- cica.long %>% mutate(is.b = ifelse(behaviour == 'b', T, F)) %>% group_by(summary,filledO1,voicedO2) %>% summarise(b.prop = mean(is.b))
cica.sum <- cica.sum %>% merge(gcm.preds)

cica.sum$O1.supports <- ifelse(cica.sum$filledO1 == T, '[m]', '[b]')
cica.sum$C2.supports <- ifelse(cica.sum$voicedO2 == T, '[b]', '[m]')
# empty O1 -> b
# voi C -> b
knitr::opts_chunk$set(dev='cairo_pdf')

ggplot(cica.sum, aes(x = gcm.big, y = b.prop, label = trs, colour = O1.supports)) + 
  geom_text() +
    ggtitle(paste0("experiment: raw proportions x GCM and Pattern 1 predictions")) + 
    ylab("proportion of [b] vs [m]") + xlab("GCM weight of [b] vs [m]") + 
    theme(axis.title=element_text(size=14, face="bold"),
          axis.text=element_text(size=12),
          strip.text=element_text(size=12),
          legend.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=12),
          plot.title=element_text(size=14,face="bold")) +
  scale_colour_manual(values=c("deepskyblue4","firebrick3"))
ggsave("gcm_pattern1.pdf", width=8, height=6, device = cairo_pdf) # ...

ggplot(cica.sum, aes(x = gcm.big, y = b.prop, label = trs, colour = C2.supports)) + 
  geom_text() +
    ggtitle(paste0("experiment: raw proportions x GCM and Pattern 2 predictions")) + 
    ylab("proportion of [b] vs [m]") + xlab("GCM weight of [b] vs [m]") + 
    theme(axis.title=element_text(size=14, face="bold"),
          axis.text=element_text(size=12),
          strip.text=element_text(size=12),
          legend.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=12),
          plot.title=element_text(size=14,face="bold")) +
  scale_colour_manual(values=c("deepskyblue4","firebrick3"))
ggsave("gcm_pattern2.pdf", width=8, height=6, device = cairo_pdf) # ...
