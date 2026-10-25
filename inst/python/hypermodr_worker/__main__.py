"""Protocol entry point; model adapters must be explicitly selected and qualified."""
from .protocol import serve


def unavailable(request):
    raise ValueError("No qualified adapter selected")


serve(unavailable)
