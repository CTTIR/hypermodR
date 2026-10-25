# vLLM profile admission

Qwen, Mistral, Qwen2.5-VL, Qwen3-VL, InternVL and Pixtral are visible candidates,
not enabled local installers. No pinned upstream profile has completed package
smoke/parity or terms admission. Explicit custom vLLM installations use the same
artifact-only, offline-wheel environment planner and owned process lifecycle as
native workers. A server launcher must be an exact reviewed hashed artifact in
that manifest; no shell interpolation or existing-service takeover occurs.

The connector sends non-streaming chat requests to an explicitly configured
OpenAI-compatible `/chat/completions` endpoint, verifies served model identity
and completion status, and enforces remote consent. Native arbitrary HSI models
are rejected. Rendered three-band HSI images are lossy auxiliary data. Image/video
wire payloads require a separately admitted processor profile; this release does
not claim that a text-only transport establishes vision parity.

Linux CUDA setup can now be resolved without authoring a profile:
`hm_resolve_backend("vllm_linux_cuda", model_id, python)` freezes vLLM 0.11.0
and its complete compatible binary dependency graph; `hm_resolve_checkpoint()`
freezes an immutable public checkpoint. After exact wheel/checkpoint terms and
driver/GPU ABI review, `hm_admit_backend()` assembles the declared install plan.
`hm_install_backend()` creates its private environment and `hm_start_backend()`
runs the packaged loopback vLLM launcher. This availability of automation does not
claim a successful live vLLM smoke/parity result.
