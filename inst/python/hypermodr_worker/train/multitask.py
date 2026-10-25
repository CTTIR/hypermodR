"""Package-owned multitask reference architecture."""
from .core import Reference

def build(bands, outputs=2, width=8):
    return Reference("multitask", bands, outputs, width)
