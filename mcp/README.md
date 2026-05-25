# edbg MCP Server

An MCP (Model Context Protocol) server that exposes edbg's Erlang function
tracing capabilities as AI-queryable tools.

## Prerequisites

- Python 3.10+
- An Erlang node with edbg loaded

Set up a Python virtual environment:

```bash
cd mcp
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Setup

### 1. Start the edbg REST API

In your Erlang shell (with edbg loaded):

```erlang
edbg:start_api().
%% Or with a custom port:
edbg:start_api(4242).
```

### 2. Configure VS Code

Create or edit `.vscode/mcp.json` in your workspace:

```json
{
  "servers": {
    "edbg": {
      "type": "stdio",
      "command": "mcp/.venv/bin/python",
      "args": ["mcp/edbg_mcp_server.py", "--transport", "stdio"],
      "env": {
        "EDBG_URL": "http://localhost:4242"
      }
    }
  }
}
```

For SSE transport (e.g. shared access), use:

```json
{
  "servers": {
    "edbg": {
      "url": "http://127.0.0.1:9090/sse"
    }
  }
}
```

And start the server manually:

```bash
mcp/venv/bin/python mcp/edbg_mcp_server.py --transport sse --port 9090
```

### 3. Use from Copilot Chat

Once configured, the edbg tools are available in VS Code Copilot Chat.
Ask it to trace modules, check status, or analyze results:

- "Trace the `lists` and `maps` modules for 5 seconds"
- "Show me the trace summary"
- "Get the last 20 trace entries"

## Available Tools

| Tool | Description |
|------|-------------|
| `start_trace` | Start tracing specified Erlang modules |
| `stop_trace` | Stop an active trace session early |
| `get_trace_status` | Check if tracing is currently running |
| `get_trace_result` | Retrieve trace entries (paginated) |
| `get_trace_summary` | Statistical overview of collected trace data |
| `get_trace_config` | Show current trace configuration |
| `set_trace_config` | Update configuration without starting a trace |

## Command-Line Options

```
python edbg_mcp_server.py [options]

Options:
  --edbg-url URL     edbg REST API base URL (default: http://localhost:4242)
  --host HOST        MCP server bind address (default: 127.0.0.1)
  --port PORT        MCP server port (default: 9090)
  --transport TYPE   stdio or sse (default: stdio)
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `EDBG_URL` | `http://localhost:4242` | edbg REST API base URL |
| `MCP_HOST` | `127.0.0.1` | MCP server bind address |
| `MCP_PORT` | `9090` | MCP server port |
| `MCP_TRANSPORT` | `stdio` | Transport type: `stdio` or `sse` |
