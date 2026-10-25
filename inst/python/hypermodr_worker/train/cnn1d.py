"""Package-owned cnn1d reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("cnn1d", bands, outputs, width)
