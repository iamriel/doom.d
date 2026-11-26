// Read the signed-in account's plan windows without starting a Codex turn.
// node agent-hq-codex-usage.mjs CODEX_COMMAND [ARGS...]
import { spawn } from "node:child_process";
import { createInterface } from "node:readline";

const [command, ...args] = process.argv.slice(2);
const server = spawn(command, [...args, "app-server"], {
  stdio: ["pipe", "pipe", "ignore"],
});
const lines = createInterface({ input: server.stdout });
const send = (message) => server.stdin.write(JSON.stringify(message) + "\n");
const timeout = setTimeout(() => server.kill(), 30_000);

function usage(result) {
  const limits = result.rateLimitsByLimitId?.codex ?? result.rateLimits;
  if (!limits) throw new Error("Missing Codex rate limits");
  const windows = [limits.primary, limits.secondary];
  function windowOf(minutes) {
    const entry = windows.find((w) => w?.windowDurationMins === minutes);
    if (!Number.isFinite(entry?.usedPercent)) return null;
    return {
      utilization: entry.usedPercent,
      resets_at: Number.isFinite(entry.resetsAt)
        ? new Date(entry.resetsAt * 1000).toISOString()
        : null,
    };
  }
  return {
    subscription_type: limits.planType ?? null,
    five_hour: windowOf(300),
    seven_day: windowOf(10080),
  };
}

try {
  // A failed spawn or a broken pipe closes stdout before the reply.
  server.on("error", () => lines.close());
  server.stdin.on("error", () => lines.close());
  send({
    id: 1,
    method: "initialize",
    params: { clientInfo: { name: "agent-hq", version: "1" } },
  });
  let received = false;
  for await (const line of lines) {
    const reply = JSON.parse(line);
    if (reply.id !== 1 && reply.id !== 2) continue;
    if (reply.error) throw new Error(reply.error.message);
    if (reply.id === 1) {
      send({ method: "initialized" });
      send({ id: 2, method: "account/rateLimits/read" });
    } else {
      process.stdout.write(JSON.stringify(usage(reply.result)) + "\n");
      received = true;
      break;
    }
  }
  if (!received) throw new Error("Codex closed before reporting usage");
} catch (error) {
  console.error(`agent-hq-codex-usage: ${error.message}`);
  process.exitCode = 1;
} finally {
  clearTimeout(timeout);
  lines.close();
  server.stdin.end();
  server.kill();
}
