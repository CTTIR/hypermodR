"""Seven visible, explicitly gated upstream candidates."""
from .base import CandidateAdapter, BlockedAdapter, CONTRACTS, cache_key, masked_pool

def create_adapter(model_id,admission=None):
    if model_id in ("htc","hyperfree","hypersigma"):
        from .native import NativeAdapter
        return NativeAdapter(model_id,admission)
    return CandidateAdapter(model_id,admission)
