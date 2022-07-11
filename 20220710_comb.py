#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 10 09:16:26 2022

@author: arifpras
"""

from itertools import permutations
import pandas as pd
#import pyreadr
import os

workdir = "/Users/arifpras/OneDrive - The University of Nottingham/BB_SideProject/BelutListrik"
os.chdir(workdir)

#db07 = pyreadr.read_r("db07.rds")
db07 = pd.read_csv("db07.csv", index_col=0)
db08 = pd.DataFrame.from_records(
    data=db07, columns=['player_team', 'position_name', 'now_cost', 'avg_points', 'avg_minutes',
           'avg_ict', 'avg_influence', 'avg_xG', 'avg_xA', 'avg_xGChain',
           'diff_opponent', 'full_name', 'web_name', 'team_name', 'team_strength',
           'position', 'short_name'])

print(db08.columns)

#print(db07.to_string())
print(db08.head())
#db08.shape()

db08["position_name"] == "Goalkeeper"

# goalkeeper

gk = db08[db08["position_name"] == "Goalkeeper"]
print(gk.head())

gk_perm = permutations(gk["player_team"], 2)
gk_comb = pd.DataFrame (gk_perm, columns = ["gk1", "gk2"])
print (gk_comb.head())

 
# Print the obtained permutations
for i in list(gk_perm):
    print (i)

# defender

defender = db08[db08["position_name"] == "Defender"]
print(defender.head())

defender_perm = permutations(defender["player_team"], 5)
defender_comb = pd.DataFrame (defender_perm, columns = ["def1", "def2", "def3", "def4", "def5"])
print (defender_comb.head())

# midfielder

midfielder = db08[db08["position_name"] == "Midfielder"]
print(midfielder.head())

midfielder_perm = permutations(midfielder["player_team"], 5)
midfielder_comb = pd.DataFrame (midfielder_perm, columns = ["mid1", "mid2", "mid3", "mid4", "mid5"])
print (midfielder_comb.head())

# midfielder

forward = db08[db08["position_name"] == "Forward"]
print(forward.head())

forward_perm = permutations(forward["player_team"], 3)
forward_comb = pd.DataFrame (forward_perm, columns = ["fwd1", "fwd2", "fwd3"])
print (forward_comb.head())
 
