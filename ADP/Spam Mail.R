ls()
rm(list = ls())

data <- tbl_df(read.table("D:/DataScience/first-repository/raw_data/Spam/spambase.data", 
                          strip.white = TRUE, sep = ",", header=FALSE))

names(data) <- c('word_freq_make', 'word_freq_address', 'word_freq_all', 'word_freq_3d',
                 'word_freq_our', 'word_freq_over', 'word_freq_remove', 'word_freq_internet',
                 'word_freq_order', 'word_freq_mail', 'word_freq_receive', 'word_freq_will', 
                 'word_freq_people', 'word_freq_report', 'word_freq_addresses',
                 'word_freq_free', 'word_freq_business', 'word_freq_email', 'word_freq_you',
                 'word_freq_credit', 'word_freq_your', 'word_freq_font', 'word_freq_000',
                 'word_freq_money', 'word_freq_hp', 'word_freq_hpl', 'word_freq_george',
                 'word_freq_650', 'word_freq_lab', 'word_freq_labs', 'word_freq_telnet',
                 'word_freq_857', 'word_freq_data', 'word_freq_415', 'word_freq_85',
                 'word_freq_technology', 'word_freq_1999', 'word_freq_parts', 
                 'word_freq_pm', 'word_freq_direct', 'word_freq_cs', 'word_freq_meeting',
                 'word_freq_original', 'word_freq_project', 'word_freq_re',
                 'word_freq_edu', 'word_freq_table', 'word_freq_conference',
                 'char_freq_;', 'char_freq_(', 'char_freq_[', 'char_freq_!', 
                 'char_freq_$', 'char_freq_#', 'capital_run_length_average',
                 'capital_run_length_longest', 'capital_run_length_total', 'class'
                 )
names(data)[58] <- 'class'
data$class <- factor(data$class)

glimpse(data)
summary(data)
library(psych)
describe(data)

panel.cor <-function(x,y, digits=2, prefix="", cex.cor,...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x,y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
  
set.seed(1610)
pairs(data %>% dplyr::select(1:10, 58) %>%
        sample_n(min(1000, nrow(data))),
      lower.panel=function(x,y){ points(x,y); abline(0,1,col='red')},
      upper.panel=panel.cor)

pairs(data %>% dplyr::select(48:57, 58) %>%
        sample_n(min(1000, nrow(data))),
      lower.panel=function(x,y){ points(x,y); abline(0,1,col='red')},
      upper.panel=panel.cor)

tmp <- as.data.frame(cor(data[,-58], as.numeric(data$class)))
tmp <- tmp %>% rename(cor=V1)
tmp$var <- rownames(tmp)
tmp %>%
  ggplot(aes(reorder(var, cor), cor)) +
  geom_point() +
  coord_flip()

library(ggplot2)
library(dplyr)
library(gridExtra)
glimpse(data)
p1 <- data %>% ggplot(aes(class)) + geom_bar()
p2 <- data %>% ggplot(aes(class, `char_freq_$`)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5) +
  scale_y_sqrt()
p3 <- data %>% ggplot(aes(`char_freq_$`, group=class, fill=class)) +
  geom_density(alpha=.5) +
  scale_x_sqrt() + scale_y_sqrt()
p4 <- data %>% ggplot(aes(class, capital_run_length_longest)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5) +
  scale_y_log10()
grid.arrange(p1, p2, p3, p4, ncol=2)
