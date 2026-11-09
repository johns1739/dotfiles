function __kokoro_play --description 'Synthesize and play text via local Kokoro'
    set -l model kokoro
    set -l voice bf_isabella # af_heart is graded A
    set -l format mp3
    set -l port 8880
    set -l speed 1.1
    set -l output (mktemp -t kokoro)

    jq -n --arg model $model --arg voice $voice --arg format $format --argjson speed $speed --arg input "$argv" \
        '{model:$model, voice:$voice, response_format:$format, speed:$speed, input:$input}' \
        | curl -sf http://localhost:$port/v1/audio/speech -H "Content-Type: application/json" -d @- -o $output
    and afplay $output
    set -l result $status

    rm -f $output
    return $result
end

# TODO: Read and learn
function __kokoro_path_basename --argument-names word
    string match -q '*://*' -- $word; and echo $word; and return
    set -l path_pattern '^([("\'\[]*)(~|\.{1,2})?/?(?:[^/\s]+/)+([^/\s]+)$'
    if not string match -qr $path_pattern -- $word
        echo $word
        return
    end
    set -l slashes (string replace -ra '[^/]' '' -- $word | string length)
    set -l last_segment (string replace -r '.*/' '' -- $word)
    if string match -qr '^[("\'\[]*(~|\.{1,2})?/' -- $word
        or test $slashes -ge 2
        or string match -q '*.*' -- $last_segment
        string replace -r $path_pattern '$1$3' -- $word
    else
        echo $word
    end
end

function __kokoro_plain_text --description 'Convert markdown on stdin to plain spoken text'
    set -l in_fence 0
    set -l output
    while read -l line
        if string match -qr '^\s*(```|~~~)' -- $line
            set in_fence (math 1 - $in_fence)
            continue
        end
        test $in_fence -eq 1; and continue
        string match -qr '^\s*\|?(\s*:?-+:?\s*\|)+\s*:?-*:?\s*$' -- $line; and continue
        string match -qr '^\s*([-*_]\s*){3,}$' -- $line; and continue
        set line (string replace -r '^\s*#{1,6}\s+' '' -- $line)
        set line (string replace -r '^\s*[-*+]\s+' '' -- $line)
        if string match -qr '^\s*\|.*\|\s*$' -- $line
            set line (string replace -r '^\s*\|\s*' '' -- $line)
            set line (string replace -r '\s*\|\s*$' '' -- $line)
            set line (string replace -ra '\s*\|\s*' ', ' -- $line)
        end
        set line (string replace -ra '!?\[([^\]]*)\]\([^)]*\)' '$1' -- $line)
        set line (string replace -ra '`([^`]*)`' '$1' -- $line)
        set line (string replace -ra '\*\*(.+?)\*\*' '$1' -- $line)
        set line (string replace -ra '__(.+?)__' '$1' -- $line)
        set line (string replace -ra '(?<![\w*])\*(?!\s)(.+?)(?<!\s)\*(?![\w*])' '$1' -- $line)
        set line (string replace -ra '(?<!\w)_(?!\s)(.+?)(?<!\s)_(?!\w)' '$1' -- $line)
        set -l words
        for word in (string split ' ' -- $line)
            set -a words (__kokoro_path_basename $word)
        end
        set -a output (string join ' ' -- $words)
    end
    printf '%s\n' $output
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
    set text (printf '%s\n' $text | __kokoro_plain_text | string collect)
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
