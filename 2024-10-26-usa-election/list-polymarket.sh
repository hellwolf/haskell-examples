#!/usr/bin/env bash

CLOB_URL=https://clob.polymarket.com/markets

iter_markets() {
    next_cursor=$1
    echo next_cursor=$next_cursor >&2
    curl -s "${CLOB_URL}?next_cursor=${next_cursor}" > tmp.json
    next_cursor=$(jq -r .next_cursor tmp.json)
    if [ "$next_cursor" != null ]; then
        jq '.data | .[]' tmp.json
        iter_markets $next_cursor
    fi
}

iter_markets | jq -s '.' > all-polymarkets.json
rm -f tmp.json
