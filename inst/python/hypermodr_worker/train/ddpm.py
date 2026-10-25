"""Package-owned ddpm reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("ddpm", bands, outputs, width)
