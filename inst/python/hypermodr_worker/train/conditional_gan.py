"""Package-owned conditional_gan reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("conditional_gan", bands, outputs, width)
