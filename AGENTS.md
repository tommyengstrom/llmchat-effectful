ai-rake is a library for supplier agnostic llms calls with automatic tool call handling.

## Workflow

- `process-compose` is the expected local build/test/ghcid interface.
- Assume the process manager is already running in the background and is owned
  outside of the agent process. Only use the configured
  `process-compose process ...` commands against that manager.
- Never start, stop, or replace the process manager yourself, including by
  running `nix develop -c process-compose up`. If its server is unavailable,
  report that instead of launching a new instance.
- Do not run `cabal build` / `cabal test` directly; use the configured
  `process-compose` processes instead.
- The output from ghcid is continuously written to `ghcid.log`; read it to get
  compiler feedback.

The Nix dev shell sets `PC_PORT_NUM=6599`, so process-compose should use port
6599 for its server.

Available processes:

- `backend-1-cabal-update` - update Cabal package indexes
- `backend-2-cabal-build-all-deps` - build dependencies only, after update
- `backend-3-cabal-build-all` - full build, after dependencies
- `backend-4-cabal-test-all` - full test run, after full build
- `backend-5-ghcid` - ghcid via `run-ghcid.sh`, after full build

Useful commands:

```bash
process-compose process logs backend-5-ghcid
process-compose process logs backend-4-cabal-test-all
process-compose process restart backend-5-ghcid
process-compose process restart backend-4-cabal-test-all
process-compose process list
```
