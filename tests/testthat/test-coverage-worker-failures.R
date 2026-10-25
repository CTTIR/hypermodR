test_that(
  paste0(
    "owned worker transport rejects malformed output and ",
    "stops offending process"
  ),
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    skip_if(!nzchar(Sys.which("python3")))
    parent <- tempfile("worker-failures-")
    dir.create(parent)
    on.exit(
      unlink(parent, recursive = TRUE),
      add = TRUE
    )
    start <- function(mode) {
      source <- file.path(parent, paste0(mode, ".py"))
      writeLines(
        c(
          "#!/usr/bin/env python3", "import sys,json",
          paste0("mode = '", mode, "'"),
          "for line in sys.stdin:", "    req=json.loads(line)",
          paste0(
            "    ans={k:req[k] for k in ",
            "('protocol','id','model','plan_digest')}"
          ),
          "    ans['status']='ok'",
          "    ans['operations']=['fit','predict','extract','cancel']",
          "    if mode=='partial':", paste0(
            "        ",
            "sys.stdout.write('{partial');sys.stdout.flush();sys.exit(0)"
          ),
          "    if mode=='oversize':",
          "        print('x'*1000,flush=True);continue",
          "    if mode=='extra':", paste0(
            "        ",
            "sys.stdout.write('{}\\n{}\\n');sys.stdout.flush();continue"
          ),
          "    if mode=='identity': ans['model']='wrong-model'",
          "    if mode=='handshake': ans['operations']=['predict']",
          paste0(
            "    if mode=='array': ",
            paste0(
              "ans['array']={'dtype':'float64','order':'F','shape'",
              ":[2,2],'values':[1,2,3],'channels':['a','b']}"
            )
          ),
          "    print(json.dumps(ans),flush=True)"
        ),
        source
      )
      profile <- backend_test_profile(source)
      backend <- hm_install_backend(
        hm_backend_plan(
          "test-identity", profile = profile,
          cache = file.path(parent, paste0(mode, "-cache"))
        ),
        TRUE, TRUE, TRUE
      )
      hm_start_backend(backend)
    }
    for (mode in c(
      "partial", "oversize", "extra", "identity",
      "handshake", "array"
    )) {
      handle <- start(mode)
      expect_error(
        hm_worker_request(
          handle, max_bytes = if (mode ==
                                    "oversize")
            512 else 4096
        ),
        switch(
          mode, partial = "partial response",
          oversize = "exceeds byte", extra = "extra worker",
          identity = "identity", handshake = "capability",
          array = "Malformed worker"
        )
      )
      expect_false(hm_backend_status(handle)$alive)
      hm_stop_backend(handle)
    }
    handle <- start("valid")
    on.exit(
      hm_stop_backend(handle),
      add = TRUE
    )
    expect_error(
      hm_worker_request(handle, operation = "unsupported"),
      "operation or bounds"
    )
    expect_error(
      hm_worker_request(
        handle, metadata = list(
          text = paste(
            rep("x", 1000),
            collapse = ""
          )
        ),
        max_bytes = 512
      ),
      "request exceeds"
    )
    expect_true(hm_backend_status(handle)$alive)
    status <- hm_worker_request(handle, "cancel")
    expect_false(status$alive)
  }
)
