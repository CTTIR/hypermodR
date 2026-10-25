"""Package-owned spectral_vit reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("spectral_vit", bands, outputs, width)
