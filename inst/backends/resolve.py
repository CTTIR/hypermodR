"""Metadata-only exact wheel resolver; never installs or builds distributions."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import tomllib
import urllib.parse
import urllib.request


def resolve():
    request = json.loads(Path(sys.argv[1]).read_text())
    directory = Path(request["directory"])
    directory.mkdir(parents=True, exist_ok=True)
    source = directory / "requirements.in"
    source.write_text("\n".join(request["requirements"]) + "\n")
    lock = directory / "pylock.toml"
    env = {k: v for k, v in os.environ.items() if not k.startswith(("UV_", "PIP_"))}
    env.update(UV_PYTHON_DOWNLOADS="never", UV_CACHE_DIR=str(directory / "uv-cache"),
               OMP_NUM_THREADS="1", OPENBLAS_NUM_THREADS="1", MKL_NUM_THREADS="1")
    args = [request["uv"], "--no-config", "--no-progress", "pip", "compile",
            "--python", request["python"], "--no-python-downloads",
            "--python-platform", request["platform"], "--only-binary", ":all:",
            "--default-index", "https://pypi.org/simple",
            "--format", "pylock.toml", "--no-header", "--no-annotate",
            "--output-file", str(lock)]
    if request.get("torch_backend"):
        args += ["--torch-backend", request["torch_backend"]]
    args += [str(source)]
    subprocess.run(args, env=env, check=True, stdout=subprocess.DEVNULL)
    resolved = tomllib.loads(lock.read_text())
    artifacts, packages, terms, lines = [], [], [], []
    for package in resolved["packages"]:
        wheels = package.get("wheels", [])
        if not wheels:
            raise ValueError("binary wheel unavailable for package")
        # uv's target-specific pylock contains only target-compatible wheels.
        # Freeze one deterministically, preferring the exact interpreter ABI.
        tag = "cp{}{}".format(sys.version_info.major, sys.version_info.minor)
        def preference(w):
            filename = w["url"].rsplit("/", 1)[-1]
            return (0 if "-" + tag + "-" in filename else 1,
                    1 if filename.endswith("-any.whl") else 0, filename)
        wheel = sorted(wheels, key=preference)[0]
        url = wheel["url"]
        if not url.startswith(("https://files.pythonhosted.org/", "https://download.pytorch.org/", "https://download-r2.pytorch.org/")):
            raise ValueError("nonallowlisted wheel origin")
        name = urllib.parse.unquote(url.rsplit("/", 1)[-1])
        if "/" in name or "\\" in name or not name.endswith(".whl"):
            raise ValueError("invalid wheel filename")
        size = wheel.get("size")
        if size is None:
            head = urllib.request.Request(url, method="HEAD")
            with urllib.request.urlopen(head, timeout=30) as response:
                size = int(response.headers["Content-Length"])
        artifacts.append(dict(path=name, url=url, sha256=wheel["hashes"]["sha256"], bytes=size))
        version = package["version"]
        metadata_url = "https://pypi.org/pypi/{}/{}/json".format(package["name"], version.split("+")[0])
        with urllib.request.urlopen(metadata_url, timeout=30) as response:
            metadata = json.load(response)["info"]
        declaration = metadata.get("license_expression") or metadata.get("license")
        unresolved = not declaration or declaration in ("UNKNOWN", "Other")
        if unresolved:
            declaration = "UNRESOLVED: inspect exact wheel license/notice files before admission."
        text = "{}=={}\nPublished license declaration:\n{}\nMetadata source: {}\nArtifact SHA256: {}\nDistribution notices require artifact-level review.".format(
            package["name"], version, declaration, metadata_url, wheel["hashes"]["sha256"])
        terms.append(dict(package=package["name"], version=version, text=text,
                          source=metadata_url, reviewed=False, unresolved=unresolved, artifact_sha256=wheel["hashes"]["sha256"]))
        lines.append("{}=={} --hash=sha256:{}".format(package["name"], version, wheel["hashes"]["sha256"]))
        packages.append(dict(name=package["name"], version=version))
    print(json.dumps(dict(artifacts=artifacts, terms=terms, requirements="\n".join(lines) + "\n",
                          packages=packages, pylock=lock.read_text())))


if __name__ == "__main__":
    resolve()
