#!/usr/bin/env python3
"""JSON-lines entry point for Chatty; diagnostics belong on stderr."""
import sys

if __name__ == '__main__':
    if '--tts-worker' in sys.argv:
        from codex_chatty.tts_worker import main
        main()
    else:
        from codex_chatty.controller import main
        main()
