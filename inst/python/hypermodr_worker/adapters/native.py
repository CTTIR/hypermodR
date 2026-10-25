"""Real external HTC, HyperFree and dual-branch HyperSIGMA API routes.

Source execution requires a complete, hash-bound local admission. Imports never
occur during an ordinary availability query. Dependencies belong in an isolated
worker; no upstream source, weights or source license is bundled here.
"""
import argparse
import hashlib
import importlib
import json
from pathlib import Path
import sys
import numpy as np
from .base import CandidateAdapter, BlockedAdapter, masked_pool, cache_key

def digest(path):
    h=hashlib.sha256()
    with Path(path).open('rb') as f:
        for b in iter(lambda:f.read(4194304),b''): h.update(b)
    return h.hexdigest()

class NativeAdapter(CandidateAdapter):
    def __init__(self,model_id,admission=None):
        super().__init__(model_id,json.loads(json.dumps(admission or {}))); self.models={}; self._cache={}; self.accounting={}
    def _verify(self):
        a=self.admission
        if a.get('execute_source_acknowledged') is not True or a.get('code_terms_admitted') is not True or a.get('weight_terms_admitted') is not True:
            raise BlockedAdapter(self.model_id,['exact source execution and separate code/weight terms must be admitted'])
        root=Path(a['source_dir']).resolve()
        files=a.get('source_files',{})
        actual={str(p.relative_to(root)) for p in root.rglob('*.py')}
        if not files or not actual.issubset(files): raise ValueError('complete imported Python source manifest required')
        for relative,expected in files.items():
            original=root/relative; p=original.resolve()
            if not p.is_relative_to(root) or original.is_symlink() or any(q.is_symlink() for q in original.parents if q!=root and q.is_relative_to(root)) or digest(p)!=expected: raise ValueError('source identity mismatch')
        for item in a['checkpoints'].values():
            if digest(item['path'])!=item['sha256']: raise ValueError('checkpoint identity mismatch')
        for field in ('code_terms_sha256','weight_terms_sha256','preprocessing_sha256'):
            if not isinstance(a.get(field),str) or len(a[field])!=64: raise ValueError('hash-bound admission record required')
        self.source_fingerprint=hashlib.sha256(json.dumps(files,sort_keys=True).encode()).hexdigest()
        return root
    def _import(self,name,root):
        old=sys.dont_write_bytecode; sys.dont_write_bytecode=True; sys.path.insert(0,str(root))
        try:
            module=importlib.import_module(name)
            if not Path(module.__file__).resolve().is_relative_to(root): raise ValueError('upstream module origin mismatch')
            return module
        finally: sys.path.pop(0); sys.dont_write_bytecode=old
    def availability(self):
        if not self.models: return dict(status='blocked',model_id=self.model_id,reasons=['explicit source/checkpoint/rights admission and load required'],live_qualification=False)
        return dict(status='installed_unverified',model_id=self.model_id,live_qualification=False)
    def identity(self):
        result=super().identity()
        result.update(status='installed_unverified' if self.models else 'blocked',pretrained=bool(self.models),source_fingerprint=getattr(self,'source_fingerprint',None),head_semantics='restored source head; no fitted target head',qualification='independent package parity pending')
        return result
    def _restore(self,name,model,state,allowed_missing=(),allowed_extra=()):
        import torch
        current=model.state_dict()
        missing=sorted(set(current)-set(state)); extra=sorted(set(state)-set(current))
        if any(not k.startswith(allowed_missing) for k in missing) or any(not k.startswith(allowed_extra) for k in extra): raise ValueError('unadmitted checkpoint tensor difference')
        selected={k:state[k] for k in current if k in state}
        if any(current[k].shape!=v.shape for k,v in selected.items()): raise ValueError('checkpoint shape mismatch')
        model.load_state_dict(selected,strict=not missing)
        if any(not torch.equal(model.state_dict()[k],v) for k,v in selected.items()): raise ValueError('restore parity failure')
        model.eval().requires_grad_(False)
        self.models[name]=model
        self.accounting[name]=dict(loaded=len(selected),missing=missing,unexpected=extra,exact_tensor_restore=True,trainable_parameters=0)
    def load_pretrained(self):
        import torch
        root=self._verify(); torch.set_num_threads(1)
        a=self.admission; checkpoints=a['checkpoints']
        if self.model_id=='htc':
            cls=self._import('htc.models.image.ModelImage',root).ModelImage
            config_cls=self._import('htc.utils.Config',root).Config
            config_item=a['config']
            if digest(config_item['path'])!=config_item['sha256']: raise ValueError('configuration hash mismatch')
            config=config_cls(config_item['path'])
            if config['input/n_channels']!=100 or config['input/preprocessing']!='L1': raise ValueError('unsupported HTC source preprocessing')
            config['model/architecture_kwargs/encoder_weights']=None
            model=cls(config)
            payload=torch.load(checkpoints['main']['path'],map_location='cpu',weights_only=True)
            state=payload['state_dict']
            if sorted(k for k in state if not k.startswith('model.'))!=['ce_loss_weighted.weight']: raise ValueError('unexpected Lightning state')
            self._restore('main',model,{k[6:]:v for k,v in state.items() if k.startswith('model.')})
        elif self.model_id=='hyperfree':
            module=self._import('HyperFree',root)
            model=module.HyperFree_model_registry['vit_b'](checkpoint=None,encoder_global_attn_indexes=-1,merge_indexs=None)
            state=torch.load(checkpoints['main']['path'],map_location='cpu',weights_only=True)
            self._restore('main',model,state,('image_encoder.contras_modules.',),('image_encoder.semantic_blocks.','image_encoder.semantic_neck.','image_encoder.pos_embed'))
            self.predictor=module.HyperFree_Predictor(model)
        elif self.model_id=='hypersigma':
            modules={k:self._import('models_mae_'+v,root/'Pretrain') for k,v in [('spatial','Spat'),('spectral','Spec')]}
            args=argparse.Namespace(image_size=64,use_ckpt='none',num_tokens=100)
            for branch in ('spatial','spectral'):
                constructor=getattr(modules[branch],'spat_mae_b' if branch=='spatial' else 'spec_mae_b')
                model=constructor(args,inchannels=100)
                with torch.serialization.safe_globals([argparse.Namespace]): payload=torch.load(checkpoints[branch]['path'],map_location='cpu',weights_only=True)
                # Extra checkpoint tensors are never silently discarded.
                extra=tuple(a.get('allowed_unused_checkpoint_prefixes',{}).get(branch,[]))
                self._restore(branch,model,payload['model'],allowed_extra=extra)
        else: raise ValueError('no native execution route')
        self._verify(); self._cache.clear()
        return dict(status='installed_unverified',branches=self.accounting,pretrained=True,independent_parity=False)
    def extract_features(self,cube,wavelengths,mask):
        import torch
        import torch.nn.functional as F
        self.validate_input(cube,wavelengths,mask)
        if not self.models: raise BlockedAdapter(self.model_id,['load admitted exact source and weights first'])
        self._verify()
        key=cache_key(cube,wavelengths,self.identity(),self.admission['preprocessing_sha256'])
        if key not in self._cache:
            x=torch.tensor(np.asarray(cube).copy(),dtype=torch.float32).permute(2,0,1)[None]
            with torch.inference_mode():
                if self.model_id=='htc':
                    if len(wavelengths)!=100 or not np.allclose(wavelengths,np.arange(500,1000,5),atol=.1): raise ValueError('HTC native TIVITA wavelengths required')
                    x=x/x.abs().sum(1,keepdim=True).clamp_min(1e-12)
                    h,w=x.shape[-2:]; x=F.pad(x,(0,(-w)%32,0,(-h)%32),mode='replicate')
                    maps=[]
                    hook=self.models['main'].architecture.decoder.register_forward_hook(lambda m,i,o: maps.append(o))
                    try: self.models['main'](x)
                    finally: hook.remove()
                    fmap=maps[-1][0].permute(1,2,0).numpy(); extent=x.shape[-2:]
                elif self.model_id=='hyperfree':
                    resized=self.predictor.transform.apply_image_torch(x)
                    if resized.max()<=0: raise ValueError('undefined source maximum normalization')
                    self.predictor.set_torch_image(resized,tuple(cube.shape[:2]),True,list(map(float,wavelengths)),GSD=None)
                    stages=self.predictor.multi_scale_features
                    fmap=torch.cat((stages[1][0],stages[4][0]),0).permute(1,2,0).numpy()
                    extent=(max(cube.shape[:2]),)*2
                else:
                    if len(wavelengths)!=100: raise ValueError('100 measured consecutive bands required')
                    if np.max(np.diff(wavelengths))>1.5*np.min(np.diff(wavelengths)): raise ValueError('spectral gap unsupported')
                    x=F.interpolate(x,size=(64,64),mode='bilinear',align_corners=False,antialias=True)
                    spatial=self.models['spatial']; tokens,(h,w)=spatial.patch_embed(x)
                    for block in spatial.blocks: tokens=block(tokens,h,w)
                    spatial_map=spatial.norm(tokens).reshape(1,h,w,-1)[0].numpy()
                    spectral=self.models['spectral']; tokens=spectral.spec_embed(spectral.patch_embed(x)).transpose(1,2); tokens=spectral.spat_map(tokens)
                    for block in spectral.blocks: tokens=block(tokens,64,64)
                    global_feature=spectral.norm(tokens).mean(1)[0].numpy()
                    fmap=np.concatenate((spatial_map,np.broadcast_to(global_feature,spatial_map.shape[:2]+global_feature.shape)),axis=-1)
                    extent=cube.shape[:2]
            self._cache={key:(fmap.copy(),extent)}
        fmap,extent=self._cache[key]
        padded=np.zeros(extent,bool); padded[:mask.shape[0],:mask.shape[1]]=mask
        self._verify()
        return masked_pool(fmap,padded).astype('float32')
