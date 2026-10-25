"""Exact-admission external htc API route."""
from .native import NativeAdapter

class Adapter(NativeAdapter):
    def __init__(self,admission=None):
        super().__init__("htc",admission)
