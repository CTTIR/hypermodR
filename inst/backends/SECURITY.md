# Runtime version admission — 2 November 2026

The automated CPU reference profile now pins Torch 2.13.0, NumPy 2.2.6 and
setuptools 84.1.0. The latter replaces vulnerable venv bootstrap copies during
installation; the full resolved environment still needs a current advisory check.
Resolution still supports existing Python 3.11/3.12 on Linux x86_64; exact
Linux runtime/model qualification is separate from version selection.

Generated `reference_cpu-*` profiles with `torch-2.6.0-numpy-2.2.6` and generated
`vllm_linux_cuda-*` profiles with `vllm-0.11.0` are retired. New plans, installation,
reuse, availability inspection and startup reject these profiles before acting.
Existing environments and artifacts are preserved; resolve a new CPU profile
and review its exact artifacts and terms before installing it in a new directory.

Automatic vLLM resolution is unavailable until a replacement version and its
platform/model behavior are reviewed. The explicit connector remains available
for independently maintained services. Custom exact manifests are a separate
review path: their owner must check current advisories, artifact provenance,
licenses and actual model/platform behavior. Recognition of the two retired
profiles is not a general-purpose vulnerability scanner or a security guarantee
for custom runtimes.

Primary publisher evidence:

- [PyTorch GHSA-63cw-57p8-fm3p](https://github.com/pytorch/pytorch/security/advisories/GHSA-63cw-57p8-fm3p):
  Torch <=2.9.1 is affected by checkpoint memory corruption that can permit code
  execution even with weights_only=True; fixed in >=2.10.0. Package-owned
  reference models persist JSON/NPZ, but native adapters load admitted upstream
  checkpoints with torch.load, making the retired version inappropriate there.
- [vLLM GHSA-4r2x-xpjr-7cvv](https://github.com/vllm-project/vllm/security/advisories/GHSA-4r2x-xpjr-7cvv):
  vLLM >=0.8.3,<0.14.1 has critical video-processing vulnerabilities; serving a
  video model is the stated precondition. Loopback binding limits reachability
  but does not repair the runtime.
- [vLLM GHSA-q8gq-377p-jq3r](https://github.com/vllm-project/vllm/security/advisories/GHSA-q8gq-377p-jq3r):
  versions <0.22.0 also have an activation-loading issue under optimized Python
  with malicious cross-encoder configuration. Those preconditions are not
  advertised by the former generated route; no exploit is claimed here.

No malicious checkpoint or exploit was executed. Version changes do not
establish scientific validity, GPU support or the absence of other advisories.
