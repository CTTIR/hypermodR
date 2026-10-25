"""Package-owned masked_autoencoder reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("masked_autoencoder", bands, outputs, width)
