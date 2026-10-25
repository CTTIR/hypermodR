import unittest
import numpy as np
from hypermodr_worker.adapters import *

class NamedTests(unittest.TestCase):
    def test_all_visible_and_blocked(self):
        for name in CONTRACTS:
            adapter=create_adapter(name)
            self.assertEqual(adapter.availability()['status'],'blocked')
            with self.assertRaises(BlockedAdapter): adapter.load_pretrained()
            self.assertFalse(adapter.identity()['pretrained'])
    def test_edges_cache_and_mutation(self):
        cube=np.arange(5*7*6,dtype=float).reshape(5,7,6); original=cube.copy()
        wavelengths=np.arange(6)+500.; mask=np.zeros((5,7),bool); mask[-1,-1]=True
        adapter=create_adapter('htc')
        self.assertEqual(adapter.validate_input(cube,wavelengths,mask)['eligible_pixels'],1)
        np.testing.assert_array_equal(cube,original)
        features=np.ones((2,3,4)); np.testing.assert_array_equal(masked_pool(features,mask),np.ones(4))
        self.assertNotEqual(cache_key(cube,wavelengths,{},{}),cache_key(cube+1,wavelengths,{},{}))
        self.assertNotEqual(cache_key(cube,wavelengths,{},{}),cache_key(cube,wavelengths,{'head':'fresh'},{}))
