"""Package-owned domain_adversarial reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("domain_adversarial", bands, outputs, width)
