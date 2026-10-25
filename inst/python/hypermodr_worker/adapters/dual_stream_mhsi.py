"""Admission-gated dual_stream_mhsi candidate; upstream execution remains unqualified."""
from .base import CandidateAdapter

class Adapter(CandidateAdapter):
    def __init__(self, admission=None):
        super().__init__("dual_stream_mhsi", admission)
