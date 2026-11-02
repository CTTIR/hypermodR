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

Automatic `hm_resolve_backend("vllm_linux_cuda", ...)` is currently blocked.
Its former vLLM 0.11.0 profile is affected by published upstream security
advisories; no replacement profile is presented as qualified. Old generated
profiles cannot be installed or started, and their files remain untouched.
See [runtime security admission](../../SECURITY.md).

A manually reviewed current custom manifest may still use the packaged loopback
launcher after exact wheel/checkpoint terms, current security, driver/GPU ABI
and actual model behavior have been checked. An immutable checkpoint manifest
can be prepared separately with `hm_resolve_checkpoint()`. Connector operation
is distinct from installing or qualifying the server.
