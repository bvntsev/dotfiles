#!/bin/bash

if [[ -n "$http_proxy" ]]; then
    echo "Proxy is ON"
else
    echo "Proxy is OFF"
fi
