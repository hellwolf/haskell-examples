#!/usr/bin/env bash

GAMMA_API=https://gamma-api.polymarket.com/events
LIMIT=100

iter_events() {
    offset=$1
    echo offset=$offset >&2
    curl -s "${GAMMA_API}?limit=${LIMIT}&offset=${offset}" > tmp.json
    if [[ "$(jq length tmp.json)" == $LIMIT ]]; then
        jq '.[]' tmp.json
        iter_events $((offset + ${LIMIT}))
    fi
}

iter_events 0 | jq -s '.' > all-polyevents.json
rm -f tmp.json

