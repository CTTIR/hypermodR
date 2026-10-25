# Explicit runtime manifests

No upstream native or vLLM model profile is presently advertised as qualified.
`hm_models()` retains candidate families and their admission blockers. A profile
is admitted only with exact source/runtime identities, byte counts, SHA256 hashes,
reviewed full terms text and its SHA256, OS/architecture/device requirements,
command and argument vector. `hm_backend_plan()` accepts explicit custom profiles;
this does not confer upstream or scientific qualification.

`hm_install_backend()` copies individual declared files only. It never extracts
archives, executes setup scripts, installs drivers or resolves online dependencies.
For isolated Python setup, declare `python = list(executable = ..., version = ...,
lock = "requirements.txt")`, include the lock and every wheel as hashed artifacts,
and place wheels in the artifact root. Setup creates a private venv with copies
and invokes pip with --no-index --no-deps --require-hashes. Runtime commands are
verified again before launch. Custom executable code requires both reviewed
manifest acknowledgement and explicit installer acknowledgement.

Installation directories are immutable and named by full plan digest. Exclusive
locks contain owner PID/start evidence. Failed staging is removed; old runtime
directories are preserved. A stale lock is never silently stolen. Interrupted
staging may be inspected and removed manually after checking the owner has ended.
All downloads/receipts/environments live in the declared external cache.

The development registry is unsigned and cannot be represented as a verified
release. `hm_verify_registry()` verifies a detached OpenSSL SHA256 RSA signature
against explicitly provisioned allowlisted public-key hashes, revocation,
expiry and minimum release sequence. A maintainer must provision the release
key and sign the exact registry bytes outside the repository before release.

## Automated official-artifact resolution

`hm_resolve_backend()` automates Linux x86_64 CPU reference and vLLM CUDA wheel
resolution with exact uv 0.11.26, an explicitly selected existing Python 3.11/3.12,
and official PyPI/PyTorch binary origins. It resolves metadata only, pins every
transitive wheel's URL/SHA256/bytes, records published terms, and bundles packaged
launchers/workers. It does not require scientific or live runtime qualification to
plan an installation. Unknown license/notice terms and unknown driver ABI are
separate hard admission gates. Review the exact per-artifact distribution terms
then use `hm_admit_backend()` to create an install plan; a missing license cannot
be admitted by toggling a checkbox. `hm_resolve_checkpoint()` resolves immutable
public Hugging Face commits, exact safe-tensor hashes, config/tokenizer metadata
and exact license files without downloading weight tensors. Remote custom model
code is rejected. Existing interpreters and uv are only queried, never modified.

The metadata resolver never auto-downloads Python, builds source packages, or
uses caller-specific package indexes/configuration. The selected interpreter
itself remains an explicit existing prerequisite. Resolution records and pip
locks are written only into the requested external exclusive directory.
