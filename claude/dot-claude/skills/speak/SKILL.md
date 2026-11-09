---
name: speak
description: Read your previous response aloud with the local Kokoro TTS server.
disable-model-invocation: true
allowed-tools: Bash(fish -c say-kokoro:*)
---

# Steps

1. Take your previous response to the user, the one immediately before this command.

2. Pipe the response verbatim to `say-kokoro` in the background with a quoted heredoc.
   `say-kokoro` strips markdown itself, so do not rewrite the text:
   ```
   fish -c say-kokoro <<'EOF'
   <previous response>
   EOF
   ```
   Run it with `run_in_background: true` so playback does not block.

3. Reply with one short line, e.g. "Speaking." Do not repeat the text.
   If the command fails, report the error output.
