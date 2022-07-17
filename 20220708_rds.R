pacman::p_load(tidyverse,
               stats,
               ggsoccer,
               utils,
               tidytext,
               rvest,
               corrr,
               jsonlite,
               fuzzyjoin,
               stringr,
               viridis,
               tictoc)

setwd("~/OneDrive - The University of Nottingham/BB_SideProject/BelutListrik")
#setwd("C:/Users/lexapsu/OneDrive - The University of Nottingham/BB_SideProject/BelutListrik")

db07 <- readRDS("db07.rds")
def08 <- readRDS("def08.rds")
def09 <- readRDS("def09.rds")
def10 <- readRDS("def10.rds")

def08b <- as.data.frame(def08)
def09b <- as.data.frame(def09)
def10b <- as.data.frame(def10)

colnames(def08b) <- c("player1", "player2", "player3", "player4", "player5")
saveRDS(def08b, "def08b.rds")

colnames(def09b) <- c("player1", "player2", "player3", "player4", "player5")
saveRDS(def09b, "def09b.rds")

colnames(def10b) <- c("player1", "player2", "player3", "player4", "player5")
saveRDS(def10b, "def10b.rds")

def08b <- readRDS("def08b.rds")
def09b <- readRDS("def09b.rds")
def10b <- readRDS("def10b.rds")

def11 <- def08b %>%
  bind_rows(def09b) %>%
  unique()

%>%
  bind_rows(def10b) %>%
  unique()

#write.csv(db07, "db07.csv", row.names = FALSE)


def01 <- db07 %>% filter(position_name %in% "Defender") %>%
  mutate(
    number = row_number()
  ) %>%
  relocate(number)

def02 <- def01 %>%
  filter(number <= 61)

def03 <- def01 %>%
  filter(number > 61 & number <=122)

def04 <- def01 %>%
  filter(number > 122)

def05 <- def02 %>%
  bind_rows(def03) %>%
  select(player_team)

def06 <- def02 %>%
  bind_rows(def04) %>%
  select(player_team)

def07 <- def03 %>%
  bind_rows(def04) %>%
  select(player_team)

tic("def08")
def08 <-  t(combn(def05$player_team, 5))
saveRDS(def08, "def08.rds")
toc()

#head(def08)

tic("def09")
def09 <-  t(combn(def06$player_team, 5))
saveRDS(def09, "def09.rds")
toc()

tic("def10")
def10 <-  t(combn(def07$player_team, 5))
saveRDS(def10, "def10.rds")
toc()

head(def08)
head(def09)
head(def10)

names(def08) <- c(
  "player1", "player2", "player3", "player4", "player5"
)

def11 <- def08 %>%
  bind_rows(def09) %>%
  bind_rows(def10)

tic("mid02")
mid01 <- db07 %>% filter(position_name %in% "Midfielder") %>%
  mutate(
    number = row_number()
  ) %>%
  relocate(number)
# mid02 <-  t(combn(mid01$player_team, 5))
# saveRDS(mid02, "mid02.rds")
toc()

tic("fwd02")
fwd01 <- db07 %>% filter(position_name %in% "Forward")
fwd02 <-  t(combn(fwd01$player_team, 3))
saveRDS(fwd02, "fwd02.rds")
toc()
