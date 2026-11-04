function say-kokoro --description "Speak text via local Kokoro"
    set -l model kokoro
    set -l voice bf_isabella
    set -l format mp3
    set -l port 8880
    set -l text "$argv"

    if test -z "$text"
        read -z text
    end

    set -l output (mktemp -t kokoro)

    jq -n --arg model $model --arg voice $voice --arg format $format --arg input "$text" \
        '{model:$model, voice:$voice, response_format:$format, input:$input}' \
        | curl -sf http://localhost:$port/v1/audio/speech -H "Content-Type: application/json" -d @- -o $output
    and afplay $output

    rm -f $output
end
