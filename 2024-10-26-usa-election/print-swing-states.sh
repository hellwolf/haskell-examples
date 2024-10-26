#!/usr/bin/env bash

# List all likely state election events:
# $ cat all-polyevents.json | jq -r '.[] | select(.slug | test("presidential-election-winner$")) | .slug'

GAMMA_API=https://gamma-api.polymarket.com

declare -A votes
declare -A swing_states
votes[ga]=16; swing_states[ga]=georgia-presidential-election-winner
votes[nc]=16; swing_states[nc]=north-carolina-presidential-election-winner
votes[pa]=19; swing_states[pa]=pennsylvania-presidential-election-winner
votes[mi]=15; swing_states[mi]=michigan-presidential-election-winner
votes[wi]=10; swing_states[wi]=wisconsin-presidential-election-winner
votes[az]=11; swing_states[az]=arizona-presidential-election-winner
votes[nv]=6;  swing_states[nv]=nevada-presidential-election-winner

for i in "${!swing_states[@]}"; do
    event_slug="${swing_states[$i]}"
    v="${votes[$i]}"
    market_id=$(cat all-polyevents.json | jq -r ".[]
        | select(.slug == \"$event_slug\")
        | .markets[]
        | select(.slug | test(\"^will-a-republican-win\"))
        | .id
    ")
    curl -s "${GAMMA_API}/markets?id=${market_id}" | jq -r ".[]
        | \"  , State \\\"$i\\\" $v \" + (.lastTradePrice | tostring)
    "
done

