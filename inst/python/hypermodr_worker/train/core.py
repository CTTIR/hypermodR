"""Small package-owned CPU reference architectures; no pretrained claims.

Input is N,C,H,W for spatial families and N,B for spectral families.
Training access is explicit: every supplied sample must have the role train.
"""
import json
from pathlib import Path
import numpy as np
import torch
from torch import nn
from torch.nn import functional as F

FAMILIES = ('cnn1d','unet2d','cnn3d','spectral_vit','masked_autoencoder',
            'contrastive_encoder','vae','conditional_gan','ddpm','multitask',
            'domain_adversarial','distillation')

class Reverse(torch.autograd.Function):
    @staticmethod
    def forward(ctx, x): return x.view_as(x)
    @staticmethod
    def backward(ctx, g): return -g

class Reference(nn.Module):
    def __init__(self, family, bands, outputs=2, width=8):
        super().__init__()
        if family not in FAMILIES: raise ValueError('unknown reference family')
        self.config = dict(family=family,bands=bands,outputs=outputs,width=width)
        self.family = family
        self.encoder = nn.Sequential(nn.Linear(bands,width),nn.GELU())
        self.head = nn.Linear(width,outputs)
        self.decoder = nn.Linear(width,bands)
        if family == 'cnn1d': self.conv = nn.Conv1d(1,width,3,padding=1)
        if family == 'unet2d':
            self.down = nn.Conv2d(bands,width,3,padding=1)
            self.up = nn.Conv2d(width*2,outputs,1)
        if family == 'cnn3d': self.conv = nn.Conv3d(1,width,3,padding=1)
        if family in ('spectral_vit','masked_autoencoder'):
            self.token = nn.Linear(1,width)
            self.position = nn.Parameter(torch.zeros(1,bands,width))
            self.attention = nn.TransformerEncoder(nn.TransformerEncoderLayer(width,2,width*2,dropout=0,batch_first=True),1)
        if family == 'vae': self.logvar = nn.Linear(width,width)
        if family == 'conditional_gan':
            self.generator = nn.Sequential(nn.Linear(bands+outputs,width),nn.ReLU(),nn.Linear(width,bands))
            self.discriminator = nn.Sequential(nn.Linear(bands+outputs,width),nn.ReLU(),nn.Linear(width,1))
        if family == 'ddpm': self.denoiser = nn.Sequential(nn.Linear(bands+1,width),nn.SiLU(),nn.Linear(width,bands))
        if family == 'multitask': self.auxiliary = nn.Linear(width,1)
        if family == 'domain_adversarial': self.domain = nn.Linear(width,2)
        if family == 'distillation':
            self.teacher = nn.Sequential(nn.Linear(bands,width*2),nn.Tanh(),nn.Linear(width*2,outputs))
            self.teacher.requires_grad_(False)
        # Unused generic heads remain frozen and are accounted separately.
        self.encoder.requires_grad_(family in ('contrastive_encoder','vae','multitask','domain_adversarial','distillation'))
        self.head.requires_grad_(family in ('cnn1d','cnn3d','spectral_vit','multitask','domain_adversarial','distillation'))
        self.decoder.requires_grad_(family in ('masked_autoencoder','vae'))

    def encode(self,x):
        if self.family == 'cnn1d': return self.conv(x[:,None,:]).relu().mean(-1)
        if self.family == 'cnn3d': return self.conv(x[:,None,:,:,:]).relu().mean((2,3,4))
        if self.family in ('spectral_vit','masked_autoencoder'):
            return self.attention(self.token(x[:,:,None])+self.position).mean(1)
        return self.encoder(x)

    def forward(self,x):
        f = self.family
        if f == 'unet2d':
            skip = self.down(x).relu()
            pooled = F.avg_pool2d(skip,2,ceil_mode=True)
            return self.up(torch.cat((skip,F.interpolate(pooled,size=skip.shape[-2:],mode='nearest')),1))
        if f == 'conditional_gan':
            condition = torch.zeros(x.shape[0],self.config['outputs'],device=x.device)
            return self.generator(torch.cat((x,condition),1))
        if f == 'ddpm': return self.denoiser(torch.cat((x,torch.zeros(len(x),1,device=x.device)),1))
        z = self.encode(x)
        if f in ('masked_autoencoder','vae'): return self.decoder(z)
        if f == 'contrastive_encoder': return F.normalize(z,dim=1)
        return self.head(z)

    def loss(self,x,y=None,domain=None):
        f = self.family
        if f == 'masked_autoencoder':
            mask = torch.rand_like(x)<0.5
            mask[:,0] = True
            return F.mse_loss(self(x.masked_fill(mask,0))[mask],x[mask])
        if f == 'contrastive_encoder':
            a = self(x); b = self(x+0.01*torch.randn_like(x))
            logits = a@b.T/0.1
            return F.cross_entropy(logits,torch.arange(len(x)))
        if f == 'vae':
            mu = self.encode(x); lv = self.logvar(mu).clamp(-8,8)
            z = mu+torch.randn_like(mu)*torch.exp(lv/2)
            return F.mse_loss(self.decoder(z),x)+0.01*(-0.5*(1+lv-mu.square()-lv.exp()).mean())
        if f == 'ddpm':
            t = torch.rand(len(x),1)*0.9+0.05; noise = torch.randn_like(x)
            noisy = (1-t).sqrt()*x+t.sqrt()*noise
            return F.mse_loss(self.denoiser(torch.cat((noisy,t),1)),noise)
        if f == 'conditional_gan':
            if y is None: raise ValueError('conditional labels required')
            c = F.one_hot(y,self.config['outputs']).float()
            fake = self.generator(torch.cat((torch.randn_like(x),c),1))
            real_score = self.discriminator(torch.cat((x,c),1))
            fake_score = self.discriminator(torch.cat((fake.detach(),c),1))
            d = F.binary_cross_entropy_with_logits(real_score,torch.ones_like(real_score))+F.binary_cross_entropy_with_logits(fake_score,torch.zeros_like(fake_score))
            g = F.binary_cross_entropy_with_logits(self.discriminator(torch.cat((fake,c),1)),torch.ones_like(real_score))
            return d+g+F.mse_loss(fake,x)
        if y is None: raise ValueError('supervised labels required')
        result = F.cross_entropy(self(x),y)
        if f == 'multitask': result = result+F.mse_loss(self.auxiliary(self.encode(x)).squeeze(-1),x.mean(1))
        if f == 'domain_adversarial':
            if domain is None: raise ValueError('explicit domain labels required')
            result = result+F.cross_entropy(self.domain(Reverse.apply(self.encode(x))),domain)
        if f == 'distillation':
            result = result+F.kl_div(F.log_softmax(self(x)/2,1),F.softmax(self.teacher(x).detach()/2,1),reduction='batchmean')*4
        return result

def train_reference(family,x,y=None,epochs=2,roles=None,groups=None,domain=None,seed=1,outputs=2):
    torch.set_num_threads(1)
    x = torch.as_tensor(np.asarray(x),dtype=torch.float32).clone()
    if not torch.isfinite(x).all() or len(x)<2: raise ValueError('finite training batch of at least two required')
    if roles is None or len(roles)!=len(x) or any(r!='train' for r in roles): raise ValueError('explicit train-only sample roles required')
    if groups is None or len(groups)!=len(x) or any(g is None for g in groups): raise ValueError('biological group identity required')
    if type(epochs) is not int or not 1<=epochs<=100: raise ValueError('bounded epochs required')
    torch.manual_seed(seed)
    bands = x.shape[1]
    model = Reference(family,bands,outputs)
    labels = None if y is None else torch.as_tensor(y,dtype=torch.long)
    domains = None if domain is None else torch.as_tensor(domain,dtype=torch.long)
    optimizer = torch.optim.Adam((p for p in model.parameters() if p.requires_grad),lr=0.005)
    history=[]
    if family=='conditional_gan':
        generator_optimizer=torch.optim.Adam(model.generator.parameters(),lr=.005)
        discriminator_optimizer=torch.optim.Adam(model.discriminator.parameters(),lr=.005)
    for _ in range(epochs):
        if family=='conditional_gan':
            if labels is None: raise ValueError('conditional labels required')
            c=F.one_hot(labels,outputs).float()
            noise=torch.randn_like(x)
            fake=model.generator(torch.cat((noise,c),1))
            discriminator_optimizer.zero_grad()
            real_logits=model.discriminator(torch.cat((x,c),1))
            fake_logits=model.discriminator(torch.cat((fake.detach(),c),1))
            dloss=F.binary_cross_entropy_with_logits(real_logits,torch.ones_like(real_logits))+F.binary_cross_entropy_with_logits(fake_logits,torch.zeros_like(fake_logits))
            dloss.backward(); discriminator_optimizer.step()
            model.discriminator.requires_grad_(False)
            generator_optimizer.zero_grad()
            logits=model.discriminator(torch.cat((fake,c),1))
            gloss=F.binary_cross_entropy_with_logits(logits,torch.ones_like(logits))+F.mse_loss(fake,x)
            gloss.backward(); generator_optimizer.step()
            model.discriminator.requires_grad_(True)
            history.append(float((dloss+gloss).detach())); continue
        optimizer.zero_grad(); loss=model.loss(x,labels,domains)
        if not torch.isfinite(loss): raise ValueError('nonfinite loss')
        loss.backward()
        if not any(p.grad is not None and p.grad.abs().sum()>0 for p in model.parameters() if p.requires_grad): raise ValueError('missing gradient')
        optimizer.step(); history.append(float(loss.detach()))
    model.eval()
    model.receipt=dict(identity='package_reference:'+family,pretrained=False,teacher_origin='fixed_seed_scratch_reference' if family=='distillation' else None,roles=list(roles),groups=list(groups),loss=history,seed=seed,trainable_tensors=[k for k,p in model.named_parameters() if p.requires_grad],frozen_tensors=[k for k,p in model.named_parameters() if not p.requires_grad])
    return model

def predict_reference(model,x):
    model.eval()
    with torch.no_grad(): return model(torch.as_tensor(np.asarray(x),dtype=torch.float32)).numpy()

def save_reference(model,path):
    """Use JSON and numeric NPZ arrays, never executable pickle checkpoints."""
    path=Path(path); path.mkdir(parents=True,exist_ok=False)
    (path/'model.json').write_text(json.dumps(dict(config=model.config,receipt=model.receipt)))
    np.savez(path/'state.npz',**{k:v.detach().cpu().numpy() for k,v in model.state_dict().items()})

def load_reference(path):
    path=Path(path); metadata=json.loads((path/'model.json').read_text())
    model=Reference(**metadata['config'])
    with np.load(path/'state.npz',allow_pickle=False) as state:
        model.load_state_dict({k:torch.from_numpy(state[k].copy()) for k in state.files},strict=True)
    model.receipt=metadata['receipt']; model.eval(); return model

def generate_reference(model,count=4,condition=None,steps=8,seed=1):
    """Bounded CPU sampling for the three generative reference identities.

    DDPM uses a linear beta schedule and its trained continuous noise variance
    predictor; VAE samples the unit-normal prior; GAN labels are explicit.
    """
    if type(count) is not int or not 1<=count<=256 or type(steps) is not int or not 2<=steps<=100: raise ValueError('bounded sample count/steps required')
    rng=torch.Generator().manual_seed(seed)
    model.eval(); bands=model.config['bands']
    with torch.no_grad():
        if model.family=='vae':
            return model.decoder(torch.randn(count,model.config['width'],generator=rng)).numpy()
        if model.family=='conditional_gan':
            labels=torch.as_tensor(condition,dtype=torch.long)
            if labels.shape!=(count,): raise ValueError('one explicit class condition per generated sample required')
            c=F.one_hot(labels,model.config['outputs']).float()
            return model.generator(torch.cat((torch.randn(count,bands,generator=rng),c),1)).numpy()
        if model.family=='ddpm':
            beta=torch.linspace(.001,.1,steps); alpha=1-beta; cumulative=torch.cumprod(alpha,0)
            x=torch.randn(count,bands,generator=rng)
            for i in reversed(range(steps)):
                t=torch.full((count,1),float(1-cumulative[i]))
                noise=model.denoiser(torch.cat((x,t),1))
                x=(x-beta[i]/torch.sqrt(1-cumulative[i])*noise)/torch.sqrt(alpha[i])
                if i: x=x+torch.sqrt(beta[i])*torch.randn(x.shape,generator=rng)
            return x.numpy()
    raise ValueError('family has no generative sampling contract')
