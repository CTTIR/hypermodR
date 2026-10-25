#!/usr/bin/env python3
"""Independent protocol fixture: mirrors arrays without importing package code."""
import json
import sys
import time

for line in sys.stdin:
    request = json.loads(line)
    if request.get("metadata", {}).get("sleep"):
        time.sleep(10)
    response = {key: request[key] for key in ("protocol", "id", "model", "plan_digest")}
    response["status"] = "ok"
    if request["operation"] == "handshake":
        response["operations"] = ["fit", "predict", "extract", "cancel"]
    else:
        response["array"] = request["array"]
        response["input_digest"] = request["input_digest"]
    print(json.dumps(response), flush=True)
