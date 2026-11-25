// Print a Claude account's plan usage (5-hour and weekly) as one JSON line.
//
//   node agent-hq-usage.mjs SDK_MJS
//
// SDK_MJS is the Agent SDK's sdk.mjs; CLAUDE_CONFIG_DIR picks the account.
// Claude Code answers through the SDK's usage call, the data behind /usage,
// so nothing here reads credentials and no model is called.
import { pathToFileURL } from "node:url";

const [sdkPath] = process.argv.slice(2);
if (!sdkPath) {
  console.error("usage: node agent-hq-usage.mjs SDK_MJS");
  process.exit(2);
}

const timeout = setTimeout(() => {
  console.error("agent-hq-usage: timed out");
  process.exit(3);
}, 30_000);

const { query } = await import(pathToFileURL(sdkPath).href);
// A prompt stream that never yields: the session starts, no turn runs.
const idle = (async function* () {
  await new Promise(() => {});
})();
const session = query({
  prompt: idle,
  options: { persistSession: false, settingSources: [] },
});

const windowOf = (w) =>
  w ? { utilization: w.utilization ?? null, resets_at: w.resets_at ?? null } : null;

let status = 0;
try {
  const usage = await session.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET();
  const limits = usage?.rate_limits ?? {};
  process.stdout.write(
    JSON.stringify({
      subscription_type: usage?.subscription_type ?? null,
      five_hour: windowOf(limits.five_hour),
      seven_day: windowOf(limits.seven_day),
    }) + "\n",
  );
} catch (error) {
  console.error(`agent-hq-usage: ${error?.message ?? error}`);
  status = 1;
} finally {
  clearTimeout(timeout);
  session.close();
}
process.exit(status);
