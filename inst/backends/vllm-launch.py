"""Launch vLLM with an explicit already-pinned local checkpoint and loopback."""
import os
from pathlib import Path
import sys

# Model download/configuration is declared in the parent profile, never inferred.
if len(sys.argv) != 4:
    raise ValueError("expected local checkpoint directory, served identity and port")
checkpoint, identity, port = sys.argv[1:]
path = Path(checkpoint).resolve(strict=True)
if not path.is_dir() or not port.isdigit() or not 1024 <= int(port) <= 65535:
    raise ValueError("invalid local checkpoint or unprivileged port")
os.execv(sys.executable, [sys.executable, "-m", "vllm.entrypoints.openai.api_server",
                         "--model", str(path), "--served-model-name", identity,
                         "--host", "127.0.0.1", "--port", port,
                         "--tensor-parallel-size", "1"])
