"""Causal temporal references with explicit timestamp and streaming state."""
import time
import numpy as np
import torch
from torch import nn

FAMILIES=('framewise','timestamp_mean','temporal_cnn','lstm','gru','transformer','state_space')

class TemporalReference(nn.Module):
    def __init__(self,family,bands,outputs=2,width=8):
        super().__init__()
        if family not in FAMILIES: raise ValueError('unknown temporal family')
        self.config=dict(family=family,bands=bands,outputs=outputs,width=width)
        self.family=family; self.project=nn.Linear(bands,width); self.head=nn.Linear(width,outputs)
        if family in ('gru','lstm'): self.recurrent=getattr(nn,family.upper())(width,width,batch_first=True)
        if family=='temporal_cnn': self.conv=nn.Conv1d(width,width,3)
        if family=='transformer': self.attention=nn.TransformerEncoder(nn.TransformerEncoderLayer(width,2,width*2,dropout=0,batch_first=True),1)
        if family=='state_space': self.decay=nn.Parameter(torch.zeros(width))
    def forward(self,x,times):
        z=self.project(x)
        if self.family in ('gru','lstm'): z=self.recurrent(z)[0]
        elif self.family=='temporal_cnn': z=self.conv(torch.nn.functional.pad(z.transpose(1,2),(2,0))).transpose(1,2)
        elif self.family=='transformer':
            z=z+torch.sin(times[None,:,None])*0.01
            mask=torch.triu(torch.ones(len(times),len(times),dtype=torch.bool),1)
            z=self.attention(z,mask=mask)
        elif self.family in ('timestamp_mean','state_space'):
            states=[]; state=torch.zeros_like(z[:,0]); weight=0.
            for i in range(z.shape[1]):
                dt=1. if i==0 else times[i]-times[i-1]
                if self.family=='state_space':
                    decay=torch.exp(-torch.nn.functional.softplus(self.decay)*dt)
                    state=state*decay+z[:,i]*(1-decay)
                else:
                    state=(state*weight+z[:,i]*dt)/(weight+dt); weight=weight+dt
                states.append(state)
            z=torch.stack(states,1)
        return self.head(z)

class CausalStream:
    """Bounded exact-prefix replay. Overflow rejects rather than silently dropping frames.

    Sequence changes reset state. Input frames are precomputed feature vectors;
    variable source geometry is preserved by the caller's frame feature contract.
    """
    def __init__(self,model,max_frames=256):
        if type(max_frames) is not int or max_frames<1: raise ValueError('positive frame bound required')
        self.model=model.eval(); self.max_frames=max_frames; self.reset()
    def reset(self):
        self.sequence=None; self.frames=[]; self.times=[]; self.latencies=[]; self.cancelled=False
    def cancel(self): self.cancelled=True
    def push(self,sequence,frames,timestamps):
        if self.cancelled: raise RuntimeError('stream cancelled')
        x=np.asarray(frames,dtype=np.float32); t=np.asarray(timestamps,dtype=float)
        if x.ndim!=2 or len(x)!=len(t) or not np.isfinite(x).all() or not np.isfinite(t).all() or np.any(np.diff(t)<=0): raise ValueError('finite ordered frame features and timestamps required; missing frames retain time gaps')
        if sequence!=self.sequence: self.reset(); self.sequence=sequence
        if self.times and len(t) and t[0]<=self.times[-1]: raise ValueError('duplicate or reversed timestamp')
        if len(self.frames)+len(x)>self.max_frames: raise ValueError('sequence buffer full; explicit reset required')
        result=[]
        for frame,stamp in zip(x,t):
            started=time.perf_counter(); self.frames.append(frame.copy()); self.times.append(float(stamp))
            with torch.no_grad():
                out=self.model(torch.tensor(np.stack(self.frames))[None],torch.tensor(self.times,dtype=torch.float32))[0,-1].numpy()
            result.append(out.copy()); self.latencies.append(time.perf_counter()-started)
        return np.asarray(result)
    def diagnostics(self):
        return dict(frames=len(self.frames),drops=0,buffer_bytes=sum(x.nbytes for x in self.frames),latency_p50=float(np.quantile(self.latencies,.5)) if self.latencies else None,latency_p95=float(np.quantile(self.latencies,.95)) if self.latencies else None,real_time_qualified=False)


def save_temporal(model,path):
    """Serialize only configuration and numeric arrays."""
    import json
    from pathlib import Path
    path=Path(path); path.mkdir(parents=True,exist_ok=False)
    (path/'model.json').write_text(json.dumps(model.config))
    np.savez(path/'state.npz',**{k:v.detach().numpy() for k,v in model.state_dict().items()})

def load_temporal(path):
    import json
    from pathlib import Path
    path=Path(path); model=TemporalReference(**json.loads((path/'model.json').read_text()))
    with np.load(path/'state.npz',allow_pickle=False) as state:
        model.load_state_dict({k:torch.from_numpy(state[k].copy()) for k in state.files},strict=True)
    return model.eval()
