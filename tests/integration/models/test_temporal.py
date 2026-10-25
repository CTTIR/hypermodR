import unittest
import tempfile
from pathlib import Path
import numpy as np
import torch
from hypermodr_worker.temporal import *

class TemporalTests(unittest.TestCase):
    def test_each_causal_stream(self):
        torch.set_num_threads(1)
        x=np.random.default_rng(2).normal(size=(5,6)).astype('float32'); t=np.array([0.,.1,.3,.8,1.])
        for family in FAMILIES:
            with self.subTest(family=family):
                torch.manual_seed(7); model=TemporalReference(family,6)
                # Actual optimizer step for every trainable temporal architecture.
                optimizer=torch.optim.Adam(model.parameters(),lr=.001)
                output=model(torch.tensor(x)[None],torch.tensor(t,dtype=torch.float32))
                output.square().mean().backward(); optimizer.step()
                a=CausalStream(model); full=a.push('a',x,t)
                with tempfile.TemporaryDirectory() as folder:
                    path=Path(folder)/'artifact'; save_temporal(model,path)
                    np.testing.assert_array_equal(full,CausalStream(load_temporal(path)).push('a',x,t))
                b=CausalStream(model); chunks=np.concatenate((b.push('a',x[:2],t[:2]),b.push('a',x[2:],t[2:])))
                np.testing.assert_array_equal(full,chunks)
                altered=x.copy(); altered[3:]+=100
                c=CausalStream(model); future=c.push('a',altered,t)
                np.testing.assert_array_equal(full[:3],future[:3])
                np.testing.assert_array_equal(full,b.push('new',x,t))
                self.assertFalse(a.diagnostics()['real_time_qualified'])
                with self.assertRaises(ValueError): a.push('a',x[:1],[1.])
                a.cancel()
                with self.assertRaises(RuntimeError): a.push('a',x[:1],[2.])
