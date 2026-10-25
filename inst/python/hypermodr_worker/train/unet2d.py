"""Package-owned unet2d reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("unet2d", bands, outputs, width)
