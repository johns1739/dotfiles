function __kokoro_play --description 'Synthesize and play text via local Kokoro'
    set -l model kokoro
    set -l voice bf_isabella # af_heart is graded A
    set -l format mp3
    set -l port 8880
    set -l output (mktemp -t kokoro)

    jq -n --arg model $model --arg voice $voice --arg format $format --arg input "$argv" \
        '{model:$model, voice:$voice, response_format:$format, input:$input}' \
        | curl -sf http://localhost:$port/v1/audio/speech -H "Content-Type: application/json" -d @- -o $output
    and afplay $output
    set -l result $status

    rm -f $output
    return $result
end

function __kokoro_attempt_lock --argument-names lock
    mkdir $lock 2>/dev/null; and echo $fish_pid >$lock/pid
end

function __kokoro_remove_stale_lock --argument-names lock
    set -l owner (cat $lock/pid 2>/dev/null)
    if test -z "$owner"; or kill -0 $owner 2>/dev/null
        return 1
    end
    rm -rf $lock
end

function __kokoro_acquire_lock --argument-names lock
    __kokoro_remove_stale_lock $lock
    __kokoro_attempt_lock $lock
end

function say-kokoro --description 'Speak text via local Kokoro, queueing while playback is busy'
    set -l text "$argv"
    if test -z "$text"
        read -z text
    end
    test -z "$text"; and return 0

    set -l state_directory (string replace -r '/$' '' -- $TMPDIR)/kokoro-say
    set -l queue $state_directory/queue
    set -l lock $state_directory/lock
    mkdir -p $queue

    printf '%s' $text >(mktemp $queue/item.XXXXXX) # No newline, unlike echo

    while __kokoro_acquire_lock $lock
        while true
            set -l items (ls -1tr $queue 2>/dev/null)
            test (count $items) -eq 0; and break
            set -l saying
            for item in $items
                set -a saying (cat $queue/$item | string collect)
                rm -f $queue/$item
            end
            __kokoro_play (string join \n\n -- $saying | string collect)
        end
        rm -rf $lock
        test (count (ls -1 $queue 2>/dev/null)) -eq 0; and break
    end
    return 0
end
