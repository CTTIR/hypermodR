"""Synthetic CPU train, gradient partition and exact safe-restore conformance."""
import tempfile
import unittest
from pathlib import Path
import numpy as np
import torch
from hypermodr_worker.train import *

class TrainableTests(unittest.TestCase):
    def test_each_family(self):
        torch.set_num_threads(1)
        for family in FAMILIES:
            with self.subTest(family=family):
                rng=np.random.default_rng(7)
                shape=(4,6,5,7) if family in ('unet2d','cnn3d') else (4,6)
                x=rng.normal(size=shape).astype('float32')
                y=np.array([0,1,0,1])
                if family=='unet2d': y=np.broadcast_to(y[:,None,None],(4,5,7)).copy()
                model=train_reference(family,x,y,roles=['train']*4,groups=['a','b','c','d'],domain=[0,1,0,1])
                self.assertTrue(all(np.isfinite(model.receipt['loss'])))
                self.assertTrue(any(p.grad is not None and p.grad.abs().sum()>0 for p in model.parameters() if p.requires_grad))
                if family=='distillation': self.assertTrue(all(p.grad is None and not p.requires_grad for p in model.teacher.parameters()))
                output=predict_reference(model,x)
                self.assertEqual(output.shape[0],4)
                self.assertTrue(np.isfinite(output).all())
                if family in ('vae','conditional_gan','ddpm'):
                    samples=generate_reference(model,count=4,condition=[0,1,0,1])
                    self.assertEqual(samples.shape,(4,6))
                    self.assertTrue(np.isfinite(samples).all())
                with tempfile.TemporaryDirectory() as folder:
                    path=Path(folder)/'artifact'; save_reference(model,path)
                    restored=load_reference(path)
                    np.testing.assert_array_equal(output,predict_reference(restored,x))
    def test_roles(self):
        with self.assertRaises(ValueError): train_reference('cnn1d',np.ones((2,6)),[0,1],roles=['train','test'],groups=['a','b'])
