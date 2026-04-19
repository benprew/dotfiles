add_path ~/.claude/local
if test (uname) = Darwin
    alias cw 'claude_wrap_macos.sh'
else
    alias cw 'claude_wrap.sh'
end
