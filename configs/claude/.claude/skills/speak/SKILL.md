---
name: speak
description: Read your previous response aloud with the local Kokoro TTS server.
disable-model-invocation: true
allowed-tools: Bash(fish -c say-kokoro:*)
---

# Steps

1. Take your previous response to the user, the one immediately before this command.

2. Rewrite it as plain spoken text:
   - Remove fenced code blocks entirely.
   - Remove markdown symbols: headings, bold, italics, backticks, bullets, table syntax.
   - Replace links with their link text.
   - Reduce file paths to just the basename.
   - Keep the wording otherwise unchanged.

3. Pipe the text to `say-kokoro` in the background with a quoted heredoc:
   ```
   fish -c say-kokoro <<'EOF'
   <plain text>
   EOF
   ```
   Run it with `run_in_background: true` so playback does not block.

4. Reply with one short line, e.g. "Speaking." Do not repeat the text.
   If the command fails, report the error output.
