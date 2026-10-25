# Model and backend specification

Status: **proposed; awaiting implementation approval**. The interfaces and profiles below are design contracts, not implemented functions or available installers. Every package adapter is currently unqualified. The companion [proposed registry](model-registry.json) is documentation data, not a runtime registry.

## Backend architecture

`hypermodR` will expose one R modelling interface while selecting a registered backend compatible with the requested architecture, task, input modality and system. Interpretable R models remain usable without Python, GPUs or network access. The native HSI models use separate Python worker environments. Supported language and vision-language models may use vLLM through a local managed server or an explicitly configured remote endpoint.

These are different execution routes. A model being a transformer, being hosted on Hugging Face, or having “GPT” in its name does not establish vLLM compatibility. In particular, SpectralGPT is a spectral masked autoencoder, not an autoregressive language model. vLLM supports specified generative/pooling architectures, compatible Transformers implementations and custom integrations with explicit implementation requirements. A new HSI plugin would require its own equivalence and performance qualification. [vLLM model contracts](https://docs.vllm.ai/en/latest/models/supported_models/)

Native workers will be isolated by exact environment profile because upstream Python, PyTorch, compiled-extension and image-library requirements differ. The design uses a versioned worker protocol and declared array interchange, not arbitrary Python objects or pickles passed into R. Worker commands use argument vectors, not interpolated shell commands. A shared in-process Python environment is not required.

Training and fine-tuning use qualified native training workers. vLLM can supply inference, rollouts and weight-transfer facilities within training systems; it does not replace the package's gradient-training implementation. New heads, learned spectral adapters, fine-tuning, quantization and converted checkpoints create distinct model identities. [vLLM training integration](https://docs.vllm.ai/en/latest/training/rlhf/)

## Required HSI candidate registry

All seven candidate families remain visible, including those with unavailable weights or unresolved terms. A previous research prototype does not establish package, platform or clinical qualification. Each row below is a proposed package option.

| Model option | Planned worker and initial variant | Input and evaluation contract | Outstanding qualification |
|---|---|---|---|
| [HTC](https://github.com/IMSY-DKFZ/htc) | Native PyTorch/HTC; image HSI organ baseline | Native TIVITA pathway; retain full-frame context. Distinguish a restored source organ head from a newly fitted target head. | Package loading, numerical parity, source/target overlap and exact platform profile. Confirm accompanying-material terms and artifact exceptions. |
| [HyperVision](https://github.com/lronkitty/HyperVision) | Native PyTorch; distilled-B first | Wavelength-conditioned full-frame features. Preserve measured wavelengths and spatial metadata; do not copy tutorial GSD. Prompt-assisted tasks are separate. | Package parity and exact upstream code terms. The [HF card](https://huggingface.co/IronKitty/HyperVision) declares Apache-2.0; the audited GitHub source route has unresolved licensing details. |
| [HyperFree](https://github.com/Jingtao-Li-CVer/HyperFree) | Native PyTorch; base first | Measured-channel features; frozen, tuned and prompt-assisted modes remain separate. Record prompt origin and annotation budget. | Package/platform parity; inspect inherited notices. Repository MIT and [weight-card Apache-2.0](https://huggingface.co/JingtaoLi/HyperFree) are separate declarations. |
| [HyperSIGMA](https://github.com/WHU-Sigma/HyperSIGMA) | Native PyTorch; spatial plus spectral base branches | Complete ROI support and explicit fusion. A spatial-only encoder is a named ablation, not the full model. | Package parity and file-level terms. Repository and [weight card](https://huggingface.co/WHU-Sigma/HyperSIGMA) declare Apache-2.0; inherited MAE notices require review. |
| [Dual-Stream-MHSI](https://github.com/DeepMed-Lab-ECNU/Dual-Stream-MHSI) | Native PyTorch candidate; documented 60-band model | Source medical segmentation task and target task stay distinct. Source band response, spatial geometry and model arguments require evidence. | Documented pretrained artifact is unavailable in the retained audit; weight terms and configuration remain unresolved. Scratch training would be a separate comparator. |
| [SpectralEarth](https://github.com/AABNassim/spectral_earth) | External native PyTorch worker; Spec-RN50/MoCo first | Explicit spectral adaptation and measured spectral support. No fabricated SWIR bands, spatial scale or full-spectrum equivalence. | Exact code/weight permission and package parity. No declared license was found in the retained source/weight audit; this option cannot silently inherit MIT. |
| [SpectralGPT](https://github.com/danfenghong/IEEE_TPAMI_SpectralGPT) | External native PyTorch worker; documented MAE checkpoint | Lossy synthetic multispectral auxiliary track. Declare projection, dropped unsupported groups and altered spatial resolution. A trained full-spectrum projection is a different method. | Package parity and exact upstream terms. Repository states GPL-3.0 and includes inherited notices; [weight record](https://zenodo.org/records/13139925) is separate. |

Options with unresolved code/weight use terms or unavailable artifacts will return an explicit reason and next prerequisite, not a zero score or a substitute architecture. Installation admission records code, weights, pretraining data and evaluation-data provenance separately.

## Optional language and vision-language models

The registry also proposes Qwen and Mistral language families, Qwen2.5-VL, Qwen3-VL, InternVL and Pixtral/Mistral vision families as optional profiles. They have not inherited HSI evaluation from the native candidates above. A release must select exact architectures, checkpoint revisions, terms, context limits and preprocessing profiles before enabling installation.

The current vLLM table documents image/video-capable architectures and notes that some Qwen-VL preprocessing differs from official Hugging Face examples. This must be tested rather than assumed equivalent. A vLLM image/video request is not a lossless transport for arbitrary spectral cubes. [Supported modalities and implementation notes](https://docs.vllm.ai/en/latest/models/supported_models/)

VLM inputs derived from HSI require a rendering manifest: source identity, selected wavelengths or response functions, colour mapping, scaling, clipping, spatial resizing, frame selection, timestamps and discarded information. RGB, false-colour images, parameter maps and spectral plots are auxiliary inputs. They cannot silently replace full-spectrum inputs in a model comparison. Prompt-conditioned outputs remain separate from unassisted predictions and from expert reference labels.

Remote calls require an explicitly configured endpoint and disclosure of which data will leave the machine. Returned model identity, input transformations, generation configuration, request/response status and supported probability semantics must be recorded. Generated explanations are not reference standards or proof of calibrated clinical risk.

## Platform profiles

The platform table defines routes to qualify. It is not a claim that every model works on every route.

| System | Proposed supported route | Profile admission |
|---|---|---|
| Linux with NVIDIA GPU | R core; native PyTorch/CUDA workers; vLLM/CUDA profiles | Exact GPU architecture, driver/runtime, Python, Torch, kernel and model compatibility. |
| Linux with AMD GPU | R core; qualified PyTorch/ROCm workers and vLLM/ROCm profiles | Exact supported device and wheel/container profile. Custom CUDA operators cannot be assumed portable. |
| Linux CPU, x86 or ARM | R core; qualified CPU workers and supported vLLM CPU profiles | ISA, dtype, memory and feature checks; x86 AVX2 and AVX512 profiles have different capabilities. |
| Windows | R core; qualified native CPU workers; WSL2 or remote vLLM | WSL distribution/device/path/port qualification. No native Windows vLLM promise. |
| macOS Apple Silicon | R core; qualified Torch CPU/MPS; optional vLLM-Metal or CPU build; remote connector | Metal plugin, MLX checkpoint and quantization are separately qualified. |
| macOS Intel | R core; available qualified workers; remote vLLM connector | Do not infer local vLLM compatibility from Apple Silicon documentation. |
| Other systems | Portable R core and qualified remote connector | Unsupported local workers remain explicitly unavailable. |

Official vLLM documentation currently covers Linux GPU routes, Windows through WSL, and Apple Silicon through a community-maintained Metal plugin. The CPU documentation also describes an Apple Silicon build. The installer must use a dated, tested profile rather than assuming one wheel works everywhere. [GPU requirements](https://docs.vllm.ai/en/latest/getting_started/installation/gpu/), [CPU requirements](https://docs.vllm.ai/en/latest/getting_started/installation/cpu/), [vLLM-Metal](https://github.com/vllm-project/vllm-metal)

GPU availability alone does not qualify a profile. Resource admission includes available memory, existing services, thread/process limits and the user's scheduling policy. A fallback changes the recorded backend/device and is never hidden.

## Proposed R interfaces

```r
hm_models(task = NULL, backend = NULL, include_unavailable = TRUE)
hm_model_spec(model_id, task, parameters = list())
hm_backend_plan(model_id, device = "auto", ...)
hm_install_backend(plan, accept_terms = FALSE, offline = FALSE)
hm_check_backend(model_id, tests = c("availability", "parity"))
hm_start_backend(backend, model, resources, host = "127.0.0.1")
hm_stop_backend(handle)
hm_backend_status(handle)
hm_fit(workflow, data, split, control)
hm_predict(object, newdata, ...)
```

The model specification records task, requested mode, input track and parameters. `hm_backend_plan()` returns a complete installation manifest with the selected engine, exact profile, resolved artifact locations/hashes, terms, download sizes, environment actions, cache destinations and resource requirements. `device = "auto"` chooses among registered compatible profiles; it does not invent unsupported conversions. The R workflow and result interfaces are specified in the package architecture plan.

Calling `hm_install_backend()` explicitly authorizes downloads and user-space actions already declared in the exact supplied plan. No download or installation occurs on package installation, `.onLoad()`, vignette execution, a Shiny page opening or an ordinary model query. `offline = TRUE` forbids network access and succeeds only from fully verified cache entries.

`accept_terms = FALSE` leaves newly required external terms unaccepted. Explicit acceptance is bound to the exact plan digest and artifact/license identifiers, not an unrestricted permanent flag. The receipt records those identifiers and the accepted text hashes. A missing license is not resolved by an acceptance checkbox. Required custom source execution needs separately recorded, hash-bound acknowledgement; no implicit `trust_remote_code` or unrestricted checkpoint unpickling is permitted.

`hm_check_backend()` separates availability from numerical parity and reports tests that were skipped or could not run. Starting a server is separate from installing its environment. Process handles identify only package-owned processes; stopping a handle cannot stop a pre-existing service. Local serving defaults to loopback. Remote serving requires explicit endpoint configuration, credentials handling and a declared transmission policy.

## Installation and runtime contracts

1. The package ships a proposed capability schema and, after qualification, signed release registries. Installed plans pin registry version/signature, source revision, full artifact SHA-256, environment lock, adapter version and preprocessing contract. A release cannot silently replace a model at the same identity.
2. Per-platform environments and model artifacts live in a versioned user cache. The installer verifies all downloads and archive destinations before activation. It rejects traversal, unexpected symlinks, incomplete artifacts and inconsistent hashes.
3. Installation stages into an exclusive temporary destination and activates atomically. Concurrent and interrupted runs preserve receipts, cannot overwrite an admitted installation, and can be resumed only under the same verified plan. Rollback removes only files owned by that attempt.
4. The installer does not elevate privileges, install drivers, change unrelated environments, take over ports or stop another service. Unsupported system prerequisites produce actionable status. Credentials never enter manifests or logs.
5. Model code, weights and datasets retain their own terms. The package's MIT license does not relicense them. No restricted upstream source or weights are bundled into the package. Separate external execution is an architectural boundary, not a substitute for reviewing applicable terms.
6. Native workers verify source/model identity before and after work. Input layout, dtype, wavelength units/order, invalid-pixel policy, calibration and geometry are validated. Feature cache keys bind full content, transformations and model state; adapter mutation cannot alter caller storage.
7. Video workers preserve recording identity, frame order, timestamps, spectral calibration and geometry. Chunking and batching cannot change frame coverage. Streaming inference declares buffering, dropped-frame policy and temporal state; temporal models remain separate from independent-frame inference.
8. Training, tuning, normalization, feature selection and calibration obey the supplied grouped split. Pixels, ROIs, augmented samples and video frames from one biological unit/recording cannot cross the declared split boundary. Prompt support, target labels and external evaluation roles remain explicit.

## Qualification states and acceptance evidence

The planned state machine distinguishes `planned`, `license_unresolved`, `checkpoint_unavailable`, `platform_unsupported`, `dependency_missing`, `installed_unverified`, `technical_pass`, `task_evaluated` and `scientifically_qualified`. Installation success cannot promote an option directly to task evaluation or scientific qualification. Each report binds evidence to model, task, input track, platform and version.

Required tests include:

- Registry completeness for all seven native families; separate optional LLM/VLM candidates; unavailable states retained; code/weight/data terms independently represented.
- Simulated OS, ISA, GPU and ABI failures before network access; WSL and Metal treated as distinct profiles; no default package-load network or process activity.
- Hash/signature mismatch, archive traversal, partial downloads, disk exhaustion, concurrent installs, offline cache misses, atomic activation and owned-file rollback.
- Model/worker protocol mismatch, malformed arrays, unsorted or unsupported wavelengths, complete ROI geometry, source drift, cache invalidation, caller-storage mutation, timeout and cancellation.
- Full versus chunked/video inference parity, timestamp/order preservation, missing/duplicate frames, recording/subject leakage rejection and declared temporal state reset.
- Mock vLLM authentication failures, wrong served-model identity, malformed/partial streams, request limits, credential redaction and explicit HSI rendering provenance.
- Independent reference-to-adapter output parity; checkpoint tensor coverage; pretrained versus new/random head accounting; prespecified CPU/GPU/quantization tolerances; prompt-origin separation.

Live model, hardware and server lanes are opt-in integration tests. Their absence is reported as unavailable or skipped, never passed. The portable package and documentation checks use small synthetic fixtures and mocked external interfaces. No model installation or qualification is performed by this planning document.
