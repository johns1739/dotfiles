function __kokoro_play --description 'Synthesize and play text via local Kokoro'
    set -l model kokoro
    set -l voice bf_isabella
    # set -l voice af_heart # Graded the best.
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

function __kokoro_acquire_lock --argument-names lock
    if mkdir $lock 2>/dev/null
        echo $fish_pid >$lock/pid
        return 0
    end
    set -l owner (cat $lock/pid 2>/dev/null)
    if test -n "$owner"; and kill -0 $owner 2>/dev/null
        return 1
    end
    rm -rf $lock
    if mkdir $lock 2>/dev/null
        echo $fish_pid >$lock/pid
        return 0
    end
    return 1
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

    printf '%s' $text >(mktemp $queue/item.XXXXXX)

    while __kokoro_acquire_lock $lock
        while true
            set -l items (ls -1tr $queue 2>/dev/null)
            test (count $items) -eq 0; and break
            set -l batch
            for item in $items
                set -a batch (cat $queue/$item | string collect)
                rm -f $queue/$item
            end
            __kokoro_play (string join \n\n -- $batch | string collect)
        end
        rm -rf $lock
        test (count (ls -1 $queue 2>/dev/null)) -eq 0; and break
    end
    return 0
end
