#!/usr/bin/env bash

conn=`sed 's/^/{_"from":_{_"room":_/; s/ /,_"door":_/; s/ /_},_"to":_{_"room":_/; s/ /,_"door":_/; s/$/_}_},_/' table.csv | tr "_" " " | tr -d "\n" | sed "s/, $/ /"`
map='"map": { "rooms": [0, 1, 2], "startingRoom": 0, "connections": [ '"$conn"'] }'
body='{"id": "'"$ICFPC2025ID"'", '"$map"' }'
echo $body

curl  --header "Content-Type: application/json" --request POST  --data "${body}"  https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com/guess
