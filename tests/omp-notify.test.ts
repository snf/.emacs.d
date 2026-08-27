import { afterEach, describe, expect, test } from "bun:test";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import ompNotify from "../omp-notify";

const savedEnvironment = {
	stateDir: process.env.CODEX_ATTN_STATE_DIR,
	emacsId: process.env.CODEX_ATTN_EMACS_INSTANCE_ID,
	terminalId: process.env.CODEX_ATTN_TERMINAL_ID,
};

let temporaryDirectory: string | undefined;

afterEach(async () => {
	if (temporaryDirectory) await rm(temporaryDirectory, { recursive: true, force: true });
	temporaryDirectory = undefined;

	for (const [key, value] of Object.entries({
		CODEX_ATTN_STATE_DIR: savedEnvironment.stateDir,
		CODEX_ATTN_EMACS_INSTANCE_ID: savedEnvironment.emacsId,
		CODEX_ATTN_TERMINAL_ID: savedEnvironment.terminalId,
	})) {
		if (value === undefined) delete process.env[key];
		else process.env[key] = value;
	}
});

function installHandler() {
	let handler: ((event: any, context: any) => Promise<void>) | undefined;
	ompNotify({
		on(event, callback) {
			expect(event).toBe("agent_end");
			handler = callback;
		},
	});
	return () => {
		if (!handler) throw new Error("agent_end handler was not registered");
		return handler;
	};
}

describe("OMP notification bridge", () => {
	test("writes OMP state with the inherited Emacs terminal identity", async () => {
		temporaryDirectory = await mkdtemp(join(tmpdir(), "omp-notify-test-"));
		process.env.CODEX_ATTN_STATE_DIR = temporaryDirectory;
		process.env.CODEX_ATTN_EMACS_INSTANCE_ID = "emacs-test";
		process.env.CODEX_ATTN_TERMINAL_ID = "terminal-test";

		const getHandler = installHandler();
		await getHandler()(
			{
				messages: [
					{ role: "user", content: "Do the thing" },
					{ role: "assistant", content: [{ type: "text", text: "Done" }], timestamp: 42 },
				],
			},
			{
				cwd: "/tmp/project",
				sessionManager: {
					getSessionId: () => "omp-session",
					getLeafId: () => "leaf-entry",
				},
			},
		);

		const state = JSON.parse(await readFile(join(temporaryDirectory, "omp-session.json"), "utf8"));
		expect(state.provider).toBe("omp");
		expect(state.turn_id).toBe("leaf-entry");
		expect(state.cwd).toBe("/tmp/project");
		expect(state.last_assistant_message).toBe("Done");
		expect(state.emacs_instance_id).toBe("emacs-test");
		expect(state.terminal_id).toBe("terminal-test");
	});

	test("does nothing outside an Emacs-owned terminal", async () => {
		temporaryDirectory = await mkdtemp(join(tmpdir(), "omp-notify-test-"));
		process.env.CODEX_ATTN_STATE_DIR = temporaryDirectory;
		delete process.env.CODEX_ATTN_EMACS_INSTANCE_ID;
		delete process.env.CODEX_ATTN_TERMINAL_ID;

		const getHandler = installHandler();
		await getHandler()(
			{ messages: [{ role: "assistant", content: "Done" }] },
			{
				cwd: "/tmp/project",
				sessionManager: {
					getSessionId: () => "unowned-session",
					getLeafId: () => "leaf-entry",
				},
			},
		);

		expect(await Array.fromAsync(new Bun.Glob("*.json").scan(temporaryDirectory))).toEqual([]);
	});

	test("does not notify for automatic continuations or failed turns", async () => {
		temporaryDirectory = await mkdtemp(join(tmpdir(), "omp-notify-test-"));
		process.env.CODEX_ATTN_STATE_DIR = temporaryDirectory;
		process.env.CODEX_ATTN_EMACS_INSTANCE_ID = "emacs-test";
		process.env.CODEX_ATTN_TERMINAL_ID = "terminal-test";

		const getHandler = installHandler();
		const context = {
			cwd: "/tmp/project",
			sessionManager: {
				getSessionId: () => "quiet-session",
				getLeafId: () => "leaf-entry",
			},
		};
		await getHandler()(
			{ willContinue: true, messages: [{ role: "assistant", content: "Continuing" }] },
			context,
		);
		await getHandler()(
			{ messages: [{ role: "assistant", content: "Failed", stopReason: "error" }] },
			context,
		);

		expect(await Array.fromAsync(new Bun.Glob("*.json").scan(temporaryDirectory))).toEqual([]);
	});
});
