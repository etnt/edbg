"""edbg MCP Server - Erlang tracing via edbg REST API.

An MCP server that exposes edbg's Erlang function tracing capabilities
as AI-queryable tools. Connects to edbg's built-in REST API to start/stop
tracing and retrieve results.

Usage:
    python edbg_mcp_server.py [options]

Options (override environment variables):
    --edbg-url URL       edbg REST API base URL
    --host HOST          MCP server bind address
    --port PORT          MCP server port
    --transport TYPE     MCP transport: sse or stdio

Environment variables:
    EDBG_URL       edbg REST API base URL (default: http://localhost:4242)
    MCP_HOST       MCP server bind address (default: 127.0.0.1)
    MCP_PORT       MCP server port (default: 9090)
    MCP_TRANSPORT  MCP transport type (default: stdio)
"""

import argparse
import json
import os
import urllib.error
import urllib.request
from mcp.server.fastmcp import FastMCP

EDBG_URL = os.environ.get("EDBG_URL", "http://localhost:4242")

MCP_HOST = os.environ.get("MCP_HOST", "127.0.0.1")
MCP_PORT = int(os.environ.get("MCP_PORT", "9090"))
MCP_TRANSPORT = os.environ.get("MCP_TRANSPORT", "stdio")

mcp = FastMCP(
    "edbg Tracing Server",
    instructions=(
        "You are an Erlang/BEAM tracing assistant connected to edbg. "
        "Use start_trace to begin tracing Erlang modules. Specify module "
        "names as a list. Tracing runs for trace_time seconds then stops "
        "automatically. After tracing completes, use get_trace_summary for "
        "an overview of collected data, then get_trace_result to inspect "
        "individual call/return entries with pagination. "
        "Use get_trace_status to check if a trace session is still active. "
        "Use stop_trace to stop tracing early. "
        "Use get_trace_config to see current configuration, or "
        "set_trace_config to update settings without starting a trace. "
        "Erlang process IDs look like <0.123.0>. "
        "Module names are atoms like 'lists', 'gen_server', 'myapp'."
    ),
)


def _make_request(url, method="GET", data=None):
    """Perform an HTTP request to the edbg REST API."""
    req = urllib.request.Request(url, method=method)
    req.add_header("Accept", "application/json")

    if data is not None:
        req.add_header("Content-Type", "application/json")
        req.data = json.dumps(data).encode()
    elif method == "POST":
        req.add_header("Content-Type", "application/json")
        req.data = b""

    try:
        with urllib.request.urlopen(req) as resp:
            body = resp.read().decode()
            if not body:
                return {}
            return json.loads(body)
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode() if exc.fp else ""
        return {"error": f"HTTP {exc.code}: {exc.reason}", "detail": detail}
    except urllib.error.URLError as exc:
        return {"error": f"Connection failed: {exc.reason}",
                "hint": f"Is edbg REST API running at {EDBG_URL}? "
                        "Start it with edbg:start_api() in the Erlang shell."}


def _api_get(path, params=None):
    """GET an edbg REST API path with optional query parameters."""
    url = f"{EDBG_URL}/api{path}"
    if params:
        query = "&".join(f"{k}={v}" for k, v in params.items())
        url = f"{url}?{query}"
    return _make_request(url)


def _api_post(path, data=None):
    """POST data to an edbg REST API path."""
    url = f"{EDBG_URL}/api{path}"
    return _make_request(url, method="POST", data=data)


def _fmt(data):
    """Format data as readable JSON."""
    return json.dumps(data, indent=2)


# ---------------------------------------------------------------------------
# Tool 1: Start Tracing
# ---------------------------------------------------------------------------

@mcp.tool()
def start_trace(modules: list[str],
                trace_time: int = 10,
                max_msgs: int = 1000,
                monotonic_ts: bool = True,
                memory: bool = False,
                send_receive: bool = False) -> str:
    """Start Erlang function tracing on specified modules.

    Traces all function calls in the given modules. Tracing runs for
    trace_time seconds then stops automatically, writing results to a
    file that can be queried with get_trace_result.

    CAUTION: Tracing has runtime overhead. Use specific modules rather
    than broad patterns. Keep trace_time short on busy systems.

    Args:
        modules: Erlang module names to trace, e.g. ['gen_server', 'myapp']
        trace_time: Seconds to trace (default: 10, max recommended: 60)
        max_msgs: Max trace messages to collect (default: 1000)
        monotonic_ts: Include nanosecond timestamps (default: True)
        memory: Track per-process memory usage (default: False)
        send_receive: Also trace send/receive messages (default: False)
    """
    data = _api_post("/trace/start", {
        "modules": modules,
        "trace_time": trace_time,
        "max_msgs": max_msgs,
        "monotonic_ts": monotonic_ts,
        "memory": memory,
        "send_receive": send_receive,
    })
    if "error" in data:
        return _fmt(data)
    return _fmt(data)


# ---------------------------------------------------------------------------
# Tool 2: Stop Tracing
# ---------------------------------------------------------------------------

@mcp.tool()
def stop_trace() -> str:
    """Stop an active tracing session early.

    Normally tracing stops automatically after trace_time seconds.
    Use this to stop early and flush results to file. After stopping,
    use get_trace_summary or get_trace_result to inspect the data.
    """
    data = _api_post("/trace/stop")
    if "error" in data:
        return _fmt(data)
    return _fmt(data)


# ---------------------------------------------------------------------------
# Tool 3: Get Trace Status
# ---------------------------------------------------------------------------

@mcp.tool()
def get_trace_status() -> str:
    """Check whether tracing is currently active.

    Returns whether the tracer process is running. If running=true,
    tracing is still collecting data. Wait for it to finish or call
    stop_trace to end early.
    """
    data = _api_get("/trace/status")
    if "error" in data:
        return _fmt(data)
    return _fmt(data)


# ---------------------------------------------------------------------------
# Tool 4: Get Trace Results (paginated)
# ---------------------------------------------------------------------------

@mcp.tool()
def get_trace_result(offset: int = 0, limit: int = 50,
                     format: str = "text") -> str:
    """Retrieve trace entries from the last tracing session.

    Each entry shows a function call or return with module, function,
    arguments/return value, calling process PID, and optional timestamp.

    Entry types:
    - 'call': function was called (shows args)
    - 'return': function returned (shows return value)
    - 'send': message sent between processes
    - 'receive': message received by a process

    Use format='brief' for compact output (MFA only, no args/values).
    Use format='text' for full detail including arguments and return values.

    Args:
        offset: Zero-based index of first entry to return (default: 0)
        limit: Maximum entries to return (default: 50)
        format: 'text' for full detail or 'brief' for compact (default: 'text')
    """
    params = {
        "offset": str(offset),
        "limit": str(limit),
        "format": format,
    }
    data = _api_get("/trace/result", params)
    if "error" in data:
        return _fmt(data)
    return _fmt(data)


# ---------------------------------------------------------------------------
# Tool 5: Get Trace Summary
# ---------------------------------------------------------------------------

@mcp.tool()
def get_trace_summary() -> str:
    """Get a statistical summary of the collected trace data.

    Returns:
    - total_entries: number of trace messages collected
    - modules: map of module name to call count
    - top_functions: most frequently called functions
    - unique_pids: list of process IDs seen in the trace
    - has_timestamps: whether monotonic timestamps were captured

    Use this to understand the trace before diving into individual
    entries with get_trace_result.
    """
    data = _api_get("/trace/summary")
    if "error" in data:
        return _fmt(data)
    return _fmt(data)


# ---------------------------------------------------------------------------
# Tool 6: Get Trace Config
# ---------------------------------------------------------------------------

@mcp.tool()
def get_trace_config() -> str:
    """Get the current edbg trace configuration.

    Shows: log file path, max messages, trace time, configured modules,
    and enabled options (timestamps, memory tracking, send/receive).
    """
    data = _api_get("/trace/config")
    if "error" in data:
        return _fmt(data)
    return _fmt(data)


# ---------------------------------------------------------------------------
# Tool 7: Set Trace Config
# ---------------------------------------------------------------------------

@mcp.tool()
def set_trace_config(max_msgs: int = None,
                     trace_time: int = None,
                     log_file: str = None,
                     monotonic_ts: bool = None,
                     memory: bool = None,
                     send_receive: bool = None,
                     modules: list[str] = None) -> str:
    """Update trace configuration without starting a trace.

    Modify settings that will be used on the next start_trace call.
    Only provided parameters are changed; others remain as-is.

    Args:
        max_msgs: Max trace messages to collect
        trace_time: Max seconds to trace
        log_file: Output file path for trace results
        monotonic_ts: Enable nanosecond timestamps
        memory: Enable per-process memory tracking
        send_receive: Enable send/receive message tracing
        modules: List of module names to trace
    """
    config = {}
    if max_msgs is not None:
        config["max_msgs"] = max_msgs
    if trace_time is not None:
        config["trace_time"] = trace_time
    if log_file is not None:
        config["log_file"] = log_file
    if monotonic_ts is not None:
        config["monotonic_ts"] = monotonic_ts
    if memory is not None:
        config["memory"] = memory
    if send_receive is not None:
        config["send_receive"] = send_receive
    if modules is not None:
        config["modules"] = modules

    data = _api_post("/trace/config", config)
    if "error" in data:
        return _fmt(data)
    return _fmt(data)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def _parse_args():
    parser = argparse.ArgumentParser(
        description="edbg Tracing MCP Server",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "Environment variables (used as fallback when switches are "
            "not provided):\n"
            "  EDBG_URL       edbg REST API base URL "
            "(default: http://localhost:4242)\n"
            "  MCP_HOST       MCP bind address (default: 127.0.0.1)\n"
            "  MCP_PORT       MCP port (default: 9090)\n"
            "  MCP_TRANSPORT  MCP transport type (default: stdio)"
        ),
    )
    parser.add_argument(
        "--edbg-url", default=None,
        help=f"edbg REST API base URL (default: {EDBG_URL})")
    parser.add_argument(
        "--host", default=None,
        help=f"MCP server bind address (default: {MCP_HOST})")
    parser.add_argument(
        "--port", type=int, default=None,
        help=f"MCP server port (default: {MCP_PORT})")
    parser.add_argument(
        "--transport", choices=["sse", "stdio"], default=None,
        help=f"MCP transport type (default: {MCP_TRANSPORT})")
    return parser.parse_args()


if __name__ == "__main__":
    args = _parse_args()

    if args.edbg_url is not None:
        EDBG_URL = args.edbg_url

    mcp.settings.host = args.host if args.host is not None else MCP_HOST
    mcp.settings.port = args.port if args.port is not None else MCP_PORT
    transport = args.transport if args.transport is not None else MCP_TRANSPORT

    mcp.run(transport=transport)
