try1<- select(creditData, 金融機構名稱, 月份, 流通卡數, 有效卡數, 當月發卡數, 當月停卡數, 逾期三個月以上帳款占應收帳款餘額含催收款之比率) %>% filter(金融機構名稱 == "臺灣銀行")
ggplot(creditData,aes(x=as.numeric(月份),y=流通卡數))+geom_line(aes(group=金融機構名稱))
try1=filter(creditData,金融機構名稱 != "總計")

try1=filter(creditData,金融機構名稱 != "總計") %>% filter(月份=='10410')

barplot(try1$流通卡數,names=try1$金融機構名稱,horiz=TRUE,las=1,cex.names=0.5)
ggplot(try1,aes(x=金融機構名稱,y=流通卡數))+geom_bar(stat='identity')+coord_flip()

try1=filter(creditData,金融機構名稱 != "總計") %>% filter(月份=='10410') %>%
  arrange(流通卡數)


index=which(try1$金融機構名稱=="永豐商業銀行")


try1=filter(creditData,金融機構名稱 != "總計") %>% filter(月份=='10410') %>%
  arrange(desc(流通卡數))

try1=filter(creditData,金融機構名稱 != "總計") %>% filter(月份=='10410') %>%
  arrange(desc(流通卡數)) %>% 
  mutate(金融機構名稱=factor(try1$金融機構名稱,levels=try1$金融機構名稱))

try1=filter(creditData,金融機構名稱 != "總計") %>% filter(月份=='10410') %>%
  arrange(desc(流通卡數)) %>% 
  mutate(金融機構名稱=factor(try1$金融機構名稱,levels=try1$金融機構名稱)) %>% 
  filter(between(row_number(),6,8))

ggplot(try1,aes(x=金融機構名稱,y=流通卡數))+geom_bar(stat='identity')+coord_flip()

try2=filter(creditData,金融機構名稱 != "總計") %>% 
  filter(金融機構名稱 %in% try1$金融機構名稱) %>%
  mutate(月份=as.numeric(月份)) 

try2=filter(creditData,金融機構名稱 != "總計") %>% 
  filter(金融機構名稱 %in% try1$金融機構名稱) %>%
  mutate(月份=as.numeric(月份)) %>% 
  filter(金融機構名稱 == "永豐商業銀行")

ggplot(try2,aes(x=月份,y=流通卡數)) +
  geom_line()

ggplot(try2,aes(x=月份,y=流通卡數,group=金融機構名稱,color=金融機構名稱)) +
  geom_line()
