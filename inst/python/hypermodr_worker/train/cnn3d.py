"""Package-owned cnn3d reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("cnn3d", bands, outputs, width)
