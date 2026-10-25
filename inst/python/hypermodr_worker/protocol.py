"""Bounded, pickle-free transport for isolated package workers.

Array order is explicitly Fortran order to preserve R tensor reconstruction.
Adapters must implement dispatch; this module never dynamically imports user code.
"""
import json
import math
import sys

PROTOCOL = "hypermodr-worker/1"
OPERATIONS = ("handshake", "fit", "predict", "extract", "cancel")
MAX_BYTES = 16 * 1024 * 1024


def validate_array(value):
    if value.get("dtype") != "float64" or value.get("order") != "F":
        raise ValueError("unsupported dtype or order")
    shape = value.get("shape", [])
    if not shape or any(type(n) is not int or n <= 0 for n in shape):
        raise ValueError("invalid shape")
    count = math.prod(shape)
    values = value.get("values", [])
    if count > MAX_BYTES // 8 or count != len(values):
        raise ValueError("invalid array size")
    if any(type(x) not in (int, float) or not math.isfinite(x) for x in values):
        raise ValueError("nonfinite numeric array")
    channels = value.get("channels", [])
    if len(channels) != shape[-1] or len(set(channels)) != len(channels):
        raise ValueError("invalid channels")
    return value


def serve(dispatch, source=None, sink=None):
    source = source or sys.stdin.buffer
    sink = sink or sys.stdout
    while True:
        line = source.readline(MAX_BYTES + 1)
        if not line:
            return
        if len(line) > MAX_BYTES or not line.endswith(b"\n"):
            raise ValueError("oversized or incomplete request")
        request = json.loads(line)
        base = {key: request.get(key) for key in
                ("protocol", "id", "model", "plan_digest")}
        try:
            if request.get("protocol") != PROTOCOL:
                raise ValueError("protocol mismatch")
            operation = request.get("operation")
            if operation not in OPERATIONS:
                raise ValueError("unsupported operation")
            if "array" in request:
                validate_array(request["array"])
            if operation == "handshake":
                result = {"operations": OPERATIONS}
            elif operation == "cancel":
                return
            else:
                result = dispatch(request)
                if "array" in result:
                    validate_array(result["array"])
            base.update(result)
            base["status"] = "ok"
        except Exception:
            # Never leak filesystem locations, credentials or input data.
            base["status"] = "error"
            base["error"] = "worker_operation_failed"
        encoded = json.dumps(base, allow_nan=False, separators=(",", ":"))
        if len(encoded.encode()) > MAX_BYTES:
            raise ValueError("oversized response")
        sink.write(encoded + "\n")
        sink.flush()
