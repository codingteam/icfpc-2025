#!/usr/bin/env python

route = "225145500422005005503015004102250045445143430212032020"
rooms = "0,2,1,1,0,0,1,1,2,2,0,2,1,2,2,2,2,2,2,2,2,1,2,1,1,2,2,0,0,0,2,1,1,2,2,0,1,2,0,1,0,0,0,0,0,0,2,1,1,2,1,1,2,1,2"
rooms = rooms.replace(",", "")

for i in range(0, len(route)):
    door = route[i]
    enter = rooms[i]
    exit = rooms[i+1]
    print(enter, door, exit)
