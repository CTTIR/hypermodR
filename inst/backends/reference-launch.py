"""Launch only the packaged, explicitly named CPU reference architectures."""
import numpy as np
import torch
from hypermodr_worker.protocol import serve
from hypermodr_worker.train import train_reference, predict_reference, save_reference, load_reference
from pathlib import Path
import hashlib
import json

model = None
identity = None
training_channels = None
training_shape = None


def dispatch(request):
    global model, identity, training_channels, training_shape
    if not request["model"].startswith("reference:"):
        from hypermodr_worker.adapters import create_adapter
        if request["operation"] != "extract":
            raise ValueError("native route supports extraction only")
        meta = request.get("metadata", {})
        a = request["array"]
        cube = np.asarray(a["values"], dtype=np.float64).reshape(a["shape"], order="F")
        mask = np.asarray(meta["mask"]["values"],dtype=bool).reshape(meta["mask"]["shape"],order="F")
        adapter = create_adapter(request["model"],meta["admission"])
        adapter.load_pretrained()
        output = np.asarray(adapter.extract_features(cube,meta["wavelengths"],mask),dtype=np.float64)
        if output.ndim == 1: output = output[None,:]
        return {"array": {"dtype":"float64","order":"F","shape":list(output.shape),
                "channels":list(range(output.shape[-1])),"values":output.ravel(order="F").tolist()},
                "identity":adapter.identity(),"input_digest":request["input_digest"]}
    family = request["model"].removeprefix("reference:")
    metadata = request.get("metadata", {})
    typed = request["array"]
    x = np.asarray(typed["values"], dtype=np.float64).reshape(typed["shape"], order="F")
    if request["operation"] == "fit":
        allowed = {"epochs", "roles", "groups", "domain", "seed", "outputs", "y", "artifact_path"}
        artifact_path = metadata.pop("artifact_path", None)
        if artifact_path and Path(artifact_path).exists():
            raise ValueError("new explicit model artifact directory required")
        if set(metadata) - allowed:
            raise ValueError("unknown training metadata")
        model = train_reference(family, x, **metadata)
        identity = request["model"]
        training_channels = typed["channels"]
        training_shape = typed["shape"][1:]
        if artifact_path:
            save_reference(model, artifact_path)
            (Path(artifact_path)/"input.json").write_text(json.dumps(dict(channels=training_channels, shape=training_shape, identity=identity)))
        return {"receipt": model.receipt, "input_digest": request["input_digest"]}
    artifact_path = metadata.get("artifact_path")
    expected = metadata.get("artifact_sha256", {})
    if artifact_path:
        if set(expected) != {"model.json", "state.npz", "input.json"}:
            raise ValueError("verified artifact required")
        for name, sha in expected.items():
            if hashlib.sha256((Path(artifact_path)/name).read_bytes()).hexdigest() != sha:
                raise ValueError("model artifact integrity changed")
        model = load_reference(artifact_path)
        inputs = json.loads((Path(artifact_path)/"input.json").read_text())
        identity = inputs["identity"]
        training_channels = inputs["channels"]
        training_shape = inputs["shape"]
    if (model is None or identity != request["model"] or
            typed["channels"] != training_channels or typed["shape"][1:] != training_shape):
        raise ValueError("model is not fitted in this owned worker")
    if request["operation"] == "extract":
        if family in ("unet2d", "conditional_gan", "ddpm"):
            raise ValueError("this reference family has no declared pooled encoder extraction")
        with torch.no_grad():
            model.eval()
            output = model.encode(torch.as_tensor(x, dtype=torch.float32)).cpu().numpy()
    else:
        output = predict_reference(model, x)
    output = np.asarray(output, dtype=np.float64)
    return {"array": {"dtype": "float64", "order": "F", "shape": list(output.shape),
                      "channels": list(range(output.shape[-1])),
                      "values": output.ravel(order="F").tolist()},
            "input_digest": request["input_digest"], "receipt": model.receipt}


serve(dispatch)
