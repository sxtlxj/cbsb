library(tidyverse)
# read in data
dw39 <- read.csv("data/ppt11/dwell_table.csv")
dwq <- read.csv("data/quote_dwell.csv") # 130941
dwp <- read.csv("data/painting_dwell.csv") # 144474
# exclude outliers
dw39 <- dw39 %>% filter(dwelltime<1000) #218864
dwp <- dwp %>% filter(dwelltime<1000) #140593
dwq <- dwq %>% filter(dwelltime<1000) #115690

# make sure the total amount of data is the same
dw39 <- dw39[1:115690,] %>% mutate(choice = replace(choice, choice == 0, 1))
dw39$type = "model"
dwq$type = "quote"
dwp$type = "painting"
dwp <- dwp[1:115690,]%>% dplyr::select(-phase) %>%
  rename(Subject = ID,Round = round) %>% mutate(dwelltime=dwelltime)
dwq <- dwq %>% dplyr::select(-phase) %>% 
  rename(Subject = ID,Round = round) %>% mutate(dwelltime=dwelltime)
dwtimeadjust <- bind_rows(dw39,dwp,dwq) 
dwtimeadjust$choice <- factor(dwtimeadjust$choice)
# overview with boxplot
dwtimeadjust %>%
  filter(dwelltime<2000) %>%
  ggplot() +
  geom_boxplot(aes(x=choice,y=dwelltime, color = type, fill = type),alpha = 0.5)

## mean dwell time
dwmeantime <- dwtimeadjust %>% group_by(type,choice) %>% 
  #filter(type %in% c("painting","quote")) %>%
  summarise(mean_time=mean(dwelltime))

## histograms 
#dwpm <- dwp %>% group_by(Subject, choice)%>%  summarise(dwelltime=mean(dwelltime))
dwp %>% filter(choice == 1) %>%
  ggplot() +
  geom_histogram(aes(dwelltime),fill="lightblue",colour="darkblue",binwidth = 20) + 
  facet_grid(~choice) + ggtitle("dwell time of paintings")+
  scale_x_continuous(limits = c(0,800)) + 
  xlab("dwell time (ms)")

dwp %>% filter(choice != 1) %>%
  ggplot() +
  geom_histogram(aes(dwelltime),fill="lightblue",colour="darkblue",binwidth = 20) + 
  facet_grid(~choice) + ggtitle("dwell time of paintings")+
  scale_x_continuous(limits = c(0,800)) + 
  xlab("dwell time (ms)")

dwq %>% filter(choice == 1) %>%
  ggplot() +
  geom_histogram(aes(dwelltime),fill="lightblue",colour="darkblue",binwidth = 20) + 
  facet_grid(~choice) + ggtitle("dwell time of quotes")+
  scale_x_continuous(limits = c(0,800)) + 
  xlab("dwell time (ms)")

dwq %>% filter(choice != 1) %>%
  ggplot() +
  geom_histogram(aes(dwelltime),fill="lightblue",colour="darkblue",binwidth = 20) + 
  facet_grid(~choice) + ggtitle("dwell time of quotes")+
  scale_x_continuous(limits = c(0,800)) + 
  xlab("dwell time (ms)")
