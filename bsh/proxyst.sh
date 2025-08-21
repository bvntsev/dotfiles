#!/bin/bash

status=""
if [[ -n "$http_proxy" ]]; then
    status="Proxy is ON"
else
    status="Proxy is OFF"
fi

echo "$status"
echo "$status" | tee /tmp/.proxy_status > /dev/null
chmod 600 /tmp/.proxy_status
