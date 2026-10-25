"""Admission-gated spectralearth candidate; upstream execution remains unqualified."""
from .base import CandidateAdapter

class Adapter(CandidateAdapter):
    def __init__(self, admission=None):
        super().__init__("spectralearth", admission)
