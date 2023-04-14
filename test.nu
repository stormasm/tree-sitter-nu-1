export def "build history" [] {
    let data = $in

    let issues = (
        $data
        | default ((date now) + 1day | date format "%Y-%m-%d") closed_at
        | into datetime created_at closed_at
    )

    let dates = (
        seq date
            -b ($issues | sort-by created_at | get 0.created_at | date format "%Y-%m-%d")
            -e (date now | date format "%Y-%m-%d")
        | into datetime
    )

    let count = (
        $dates | each {|date|
            print -n $"(ansi erase_line)computing issues at ($date)\r"
            {
                date: $date
                issues: ($issues | where {|it|
                    ($it.created_at <= $date) and ($it.closed_at >= $date)
                } | length)
            }
        }
    )

    $count
}
