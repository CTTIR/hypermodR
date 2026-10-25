"""Exact-admission external hyperfree API route."""
from .native import NativeAdapter

class Adapter(NativeAdapter):
    def __init__(self,admission=None):
        super().__init__("hyperfree",admission)
