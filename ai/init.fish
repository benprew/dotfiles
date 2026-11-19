add_path ~/.claude/local
if test (uname) = Darwin
    alias cw 'claude_wrap_macos.sh'
else
    alias cw 'claude_wrap.sh'
end


alias cmed 'codex --model gpt-6.2-sol -c model_reasoning_effort=medium'
alias chi 'codex --model gpt-6.2-sol -c model_reasoning_effort=high'
alias cmax 'codex --model gpt-6.2-sol -c model_reasoning_effort=max'
