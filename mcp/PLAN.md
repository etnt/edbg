# edbg MCP Server — Implementation Plan

## Overview

Build an MCP server that exposes edbg's Erlang tracing capabilities to AI
assistants. Since plain edbg has no YANG model or RESTCONF API (unlike the
NSO variant), we need two components:

1. **Erlang REST API** — A lightweight HTTP server (OTP `httpd`/`inets`)
   running inside the BEAM, exposing edbg operations as JSON endpoints.
2. **Python MCP Server** — A FastMCP-based server (like the NSO variant)
   that translates MCP tool calls into HTTP requests against the Erlang
   REST API.

```
┌─────────────┐       JSON/HTTP        ┌──────────────────┐
│  MCP Client │◄──── MCP protocol ────►│  Python MCP      │
│  (AI Agent) │                        │  Server          │
└─────────────┘                        └────────┬─────────┘
                                                │ HTTP
                                                ▼
                                       ┌──────────────────┐
                                       │  Erlang REST API  │
                                       │  (OTP httpd)      │
                                       │  ┌──────────────┐ │
                                       │  │ edbg / edbg_ │ │
                                       │  │ file_tracer  │ │
                                       │  └──────────────┘ │
                                       └──────────────────┘
```

---

## Phase 1: Erlang REST API Server

### Module: `edbg_rest_api.erl`

A gen_server that starts an `inets` httpd instance and dispatches JSON
requests to edbg functions.

### Endpoints

| Method | Path | Description |
|--------|------|-------------|
| POST | `/api/trace/start` | Start tracing (modules, options) |
| POST | `/api/trace/stop` | Stop active tracing |
| GET | `/api/trace/status` | Check if tracing is active |
| GET | `/api/trace/result` | Get trace results (with offset/limit) |
| GET | `/api/trace/summary` | Get statistical summary of trace |
| GET | `/api/trace/config` | Get current trace configuration |
| POST | `/api/trace/config` | Update trace configuration |

### Request/Response Format

All responses are `application/json`. Example:

**POST `/api/trace/start`**
```json
{
  "modules": ["mymod", "othermod"],
  "trace_time": 10,
  "max_msgs": 1000,
  "monotonic_ts": true,
  "memory": false
}
```

**GET `/api/trace/result?offset=0&limit=50&format=text`**
```json
{
  "total": 237,
  "offset": 0,
  "limit": 50,
  "entries": [
    {
      "seq": 0,
      "type": "call",
      "pid": "<0.123.0>",
      "mfa": "mymod:myfun/2",
      "args": "[arg1, arg2]",
      "ts": 12345678900
    },
    ...
  ]
}
```

**GET `/api/trace/summary`**
```json
{
  "total_entries": 237,
  "modules": {"mymod": 150, "othermod": 87},
  "top_functions": [{"mfa": "mymod:myfun/2", "count": 95}, ...],
  "unique_pids": ["<0.123.0>", "<0.456.0>"],
  "has_timestamps": true,
  "has_memory": false,
  "duration_ns": 9800000000
}
```

### Implementation Details

- Use `inets` application with `mod_esi` for dynamic request handling
- Default port: `4242` (configurable via application env or start args)
- Parse trace result file (`edbg.trace_result`) with
  `file:read_file/1` + `binary_to_term/1`
- Format Erlang terms to JSON-safe strings using `io_lib:format/2`
- Keep the server stateless where possible — trace state lives in
  `edbg_file_tracer`

### Key Design Decisions

1. **Use `mod_esi`** — simplest OTP httpd approach for dynamic content;
   callbacks receive the request and return response data directly.
2. **JSON encoding** — implement minimal JSON encoding in Erlang (or use
   a small helper) since we don't want heavy dependencies. The data
   structures are simple (strings, integers, lists of maps).
3. **Trace result parsing** — read the binary file on each request (it's
   typically small, <1MB). Cache if performance becomes an issue.
4. **Error handling** — return HTTP 4xx/5xx with JSON error bodies.

---

## Phase 2: Python MCP Server

### Module: `mcp/edbg_mcp_server.py`

Modeled after `nso_beam_mcp_server.py` but targeting the Erlang REST API.

### MCP Tools

| Tool | Purpose |
|------|---------|
| `start_trace` | Configure modules and start tracing |
| `stop_trace` | Stop an active trace session |
| `get_trace_status` | Check if tracing is running |
| `get_trace_result` | Retrieve formatted trace entries (paginated) |
| `get_trace_summary` | Statistical overview of collected trace data |
| `get_trace_config` | Show current trace configuration |
| `set_trace_config` | Update trace configuration without starting |

### Configuration

| Env Variable | Default | Description |
|---|---|---|
| `EDBG_URL` | `http://localhost:4242` | Erlang REST API base URL |
| `MCP_HOST` | `127.0.0.1` | MCP server bind address |
| `MCP_PORT` | `9090` | MCP server port |
| `MCP_TRANSPORT` | `stdio` | Transport: `stdio` or `sse` |

Also supports command-line arguments (like the NSO variant).

### Instructions Prompt

```
You are an Erlang/BEAM tracing assistant connected to edbg.
Use start_trace to begin tracing Erlang modules. Specify module names
as a list. Tracing runs for trace_time seconds then stops automatically.
After tracing completes, use get_trace_summary for an overview, then
get_trace_result to inspect individual call/return entries.
Use get_trace_status to check if a trace session is still active.
Erlang process IDs look like <0.123.0>.
```

---

## Phase 3: Build & Integration

### Files to Create

```
src/edbg_rest_api.erl      — Erlang REST API server (gen_server + httpd)
mcp/edbg_mcp_server.py     — Python MCP server (already exists, empty)
```

### Files to Modify

```
src/edbg.erl               — Add start_api/0, start_api/1 wrapper functions
src/edbg.app.src            — Add edbg_rest_api to modules, inets to applications
```

### Dependencies

- **Erlang side**: `inets`, `crypto` (both OTP built-in, no external deps)
- **Python side**: `mcp[cli]>=1.0.0` (already in requirements.txt)

### Startup Sequence

1. Start the Erlang node with edbg loaded
2. Call `edbg:start_api()` (or `edbg:start_api(Port)`) — this starts
   the REST API server on `127.0.0.1:4242` (or custom port)
3. Start the Python MCP server: `python mcp/edbg_mcp_server.py`
   (supports both `--transport stdio` and `--transport sse`)
4. AI client connects to the MCP server via stdio or SSE

---

## Phase 4: Testing & Validation

1. **Unit test the REST API** — Start the Erlang server, use `curl` to
   verify endpoints return correct JSON.
2. **Integration test** — Run the MCP server, connect with an MCP client,
   invoke tools and verify trace data flows end-to-end.
3. **Edge cases**:
   - No trace result file exists yet
   - Tracing already active when start is called
   - Very large trace results (pagination)
   - Invalid module names

---

## Implementation Order

1. `src/edbg_rest_api.erl` — Erlang REST API with `/api/trace/*` endpoints
2. `mcp/edbg_mcp_server.py` — Python MCP server with tool definitions
3. Wire up: test the full chain (MCP → Python → HTTP → Erlang → edbg)
4. Documentation in README or wiki

---

## Design Decisions (Resolved)

- **No interactive debugger** — only file-based tracing is exposed.
- **No authentication** — the REST API always binds to `127.0.0.1`.
- **No polling** — the client checks trace status on demand via
  `get_trace_status`; the server does not poll or push notifications.
