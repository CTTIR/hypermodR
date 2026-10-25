"""Package-owned contrastive_encoder reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("contrastive_encoder", bands, outputs, width)
