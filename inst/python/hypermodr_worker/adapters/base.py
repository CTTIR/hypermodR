"""Admission and geometry contracts for external named HSI adapters.

No external implementation or checkpoint is bundled or implicitly executed.
"""
import hashlib
import json
import numpy as np

CONTRACTS = {
 'htc': ('native_sensor','full_frame','source_organ_head_or_fresh_target_head'),
 'hypervision': ('native_sensor','full_frame','distilled_b_wavelength_conditioned'),
 'hyperfree': ('native_sensor','full_frame','base_measured_channels'),
 'hypersigma': ('native_sensor','full_frame','spatial_and_spectral_fusion'),
 'dual_stream_mhsi': ('native_sensor','full_frame','documented_60_band_segmentation'),
 'spectralearth': ('documented_spectral_adaptation','full_frame','spec_rn50_moco'),
 'spectralgpt': ('lossy_synthetic_multispectral_auxiliary','full_frame','masked_autoencoder')}

class BlockedAdapter(RuntimeError):
    def __init__(self, model_id, reasons):
        self.status='blocked'; self.model_id=model_id; self.reasons=reasons
        super().__init__('; '.join(reasons))

class CandidateAdapter:
    def __init__(self,model_id,admission=None):
        if model_id not in CONTRACTS: raise ValueError('unknown candidate')
        self.model_id=model_id; self.admission=admission or {}
    def identity(self):
        track,scope,variant=CONTRACTS[self.model_id]
        return dict(model_id=self.model_id,input_track=track,input_scope=scope,variant=variant,pretrained=False,status='blocked',qualified_platforms=[])
    def availability(self):
        required=('source_sha256','weights_sha256','source_terms_sha256','weight_terms_sha256','preprocessing_sha256','platform_receipt_sha256')
        reasons=['missing '+k for k in required if not isinstance(self.admission.get(k),str) or len(self.admission[k])!=64 or any(c not in '0123456789abcdef' for c in self.admission[k])]
        reasons.append('exact upstream execution and independent parity not qualified in this package')
        return dict(status='blocked',reasons=reasons,model_id=self.model_id)
    def load_pretrained(self):
        raise BlockedAdapter(self.model_id,self.availability()['reasons'])
    def extract_features(self,*args,**kwargs):
        self.load_pretrained()
    def validate_input(self,cube,wavelengths,mask):
        x=np.asarray(cube); w=np.asarray(wavelengths); m=np.asarray(mask)
        if x.ndim!=3 or m.shape!=x.shape[:2] or w.shape!=(x.shape[-1],): raise ValueError('HWC cube, measured wavelengths and full-frame mask required')
        if not np.isfinite(x).all() or not np.isfinite(w).all() or np.any(np.diff(w)<=0): raise ValueError('finite cube and strictly increasing measured nm wavelengths required')
        if m.dtype!=np.bool_ or not m.any(): raise ValueError('nonempty boolean eligible-pixel mask required')
        if self.model_id=='dual_stream_mhsi' and len(w)!=60: raise ValueError('documented 60 measured bands required')
        return dict(shape=list(x.shape),eligible_pixels=int(m.sum()),input_track=CONTRACTS[self.model_id][0])

def cache_key(cube,wavelengths,model_identity,preprocessing):
    """Bind all frame bytes and exact encoder/preprocessing state; masks pool separately."""
    h=hashlib.sha256()
    for a in (np.asarray(cube),np.asarray(wavelengths)):
        h.update(str((a.shape,a.dtype.str)).encode()); h.update(a.tobytes(order='C'))
    h.update(json.dumps([model_identity,preprocessing],sort_keys=True,allow_nan=False).encode())
    return h.hexdigest()

def masked_pool(features,mask):
    """Exact area overlap pools every eligible edge pixel on arbitrary grids."""
    f=np.asarray(features); m=np.asarray(mask)
    if f.ndim!=3 or m.ndim!=2 or m.dtype!=np.bool_ or not m.any(): raise ValueError('feature grid and eligible boolean mask required')
    def overlap(n,k):
        left=np.arange(n)[:,None]; edges=np.linspace(0,n,k+1)
        return np.maximum(0,np.minimum(left+1,edges[1:])-np.maximum(left,edges[:-1]))
    weights=overlap(m.shape[0],f.shape[0]).T@m.astype(float)@overlap(m.shape[1],f.shape[1])
    return np.sum(f*weights[:,:,None],axis=(0,1))/weights.sum()
