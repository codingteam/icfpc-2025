#!/usr/bin/env bash

if [ -z "${ICFPC2025ID}" ]; then
  echo "ICFPC2025ID is not set. Provide it with your team's id."
  exit 1
fi

curl  --header "Content-Type: application/json" --request POST  --data '{ "id": "'"${ICFPC2025ID}"'", "problemName": "probatio" }'  https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com/select
echo ""
curl  --header "Content-Type: application/json" --request POST  --data '{ "id": "'"${ICFPC2025ID}"'", "plans": ["225145500422005005503015004102250045445143430212032020"] }'  https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com/explore
echo ""
