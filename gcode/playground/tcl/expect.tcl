#!/usr/bin/expect -f

spawn {*}$argv
set timeout 1
expect {
    -re "\\$" {
        send "hello world\n\004"
    }
    timeout {
        puts "timeout\n"
        exp_continue
    }

}

expect {
    "hello again" {
        send "what the fuck\n\004"
    }
    timeout {
        puts "timeout\n"
        exp_continue
    }

}

interact
