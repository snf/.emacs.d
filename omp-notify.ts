/**
 * Bridge Oh My Pi completed turns into the existing Emacs codex-attn protocol.
 *
 * This extension is globally discoverable by OMP, but deliberately activates
 * only for terminals launched by Emacs/Ghostel.  Other OMP sessions keep their
 * normal native notifications and do not leave unowned attention state behind.
 */

import { execFile } from "node:child_process";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);
const NOTIFY_SCRIPT = "/home/core/.emacs.d/codex-notify.py";

type Message = {
	role?: string;
	content?: unknown;
	stopReason?: string;
	timestamp?: number;
};

type AgentEndEvent = {
	messages: Message[];
	willContinue?: boolean;
};

type ExtensionContext = {
	cwd: string;
	sessionManager: {
		getSessionId(): string;
		getLeafId(): string | null;
	};
};

type ExtensionApi = {
	on(
		event: "agent_end",
		handler: (event: AgentEndEvent, context: ExtensionContext) => Promise<void>,
	): void;
};

function messageText(content: unknown): string | undefined {
	if (typeof content === "string") return content;
	if (!Array.isArray(content)) return undefined;

	const text = content
		.map(block => {
			if (!block || typeof block !== "object") return "";
			const value = (block as { text?: unknown }).text;
			return typeof value === "string" ? value : "";
		})
		.filter(Boolean)
		.join("\n");

	return text || undefined;
}

export default function ompNotify(pi: ExtensionApi): void {
	pi.on("agent_end", async (event, ctx) => {
		if (event.willContinue) return;
		if (!process.env.CODEX_ATTN_EMACS_INSTANCE_ID || !process.env.CODEX_ATTN_TERMINAL_ID) return;

		const assistant = event.messages.findLast(message => message.role === "assistant");
		if (!assistant || assistant.stopReason === "aborted" || assistant.stopReason === "error") return;

		const sessionId = ctx.sessionManager.getSessionId();
		const payload = {
			type: "agent-turn-complete",
			provider: "omp",
			thread_id: sessionId,
			turn_id: ctx.sessionManager.getLeafId() ?? assistant.timestamp ?? `${sessionId}:${Date.now()}`,
			cwd: ctx.cwd,
			last_assistant_message: messageText(assistant.content),
		};

		try {
			await execFileAsync("python3", [NOTIFY_SCRIPT, "--provider", "omp", JSON.stringify(payload)]);
		} catch (error) {
			console.error("[codex-attn] OMP notification bridge failed:", error);
		}
	});
}
