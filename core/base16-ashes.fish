#!/usr/bin/env fish

# base16-shell (https://github.com/chriskempson/base16-shell)
# Base16 Shell template by Chris Kempson (http://chriskempson.com)
# Ashes scheme by Jannik Siebert (https://github.com/janniks)

set BASE16_THEME ashes

set color00 "1C/20/23" # Base 00 - Black
set color01 "C7/AE/95" # Base 08 - Red
set color02 "95/C7/AE" # Base 0B - Green
set color03 "AE/C7/95" # Base 0A - Yellow
set color04 "AE/95/C7" # Base 0D - Blue
set color05 "C7/95/AE" # Base 0E - Magenta
set color06 "95/AE/C7" # Base 0C - Cyan
set color07 "C7/CC/D1" # Base 05 - White
set color08 "74/7C/84" # Base 03 - Bright Black
set color09 $color01 # Base 08 - Bright Red
set color10 $color02 # Base 0B - Bright Green
set color11 $color03 # Base 0A - Bright Yellow
set color12 $color04 # Base 0D - Bright Blue
set color13 $color05 # Base 0E - Bright Magenta
set color14 $color06 # Base 0C - Bright Cyan
set color15 "F3/F4/F5" # Base 07 - Bright White
set color16 "C7/C7/95" # Base 09
set color17 "C7/95/95" # Base 0F
set color18 "39/3F/45" # Base 01
set color19 "56/5E/65" # Base 02
set color20 "AD/B3/BA" # Base 04
set color21 "DF/E2/E5" # Base 06
set color_foreground "C7/CC/D1" # Base 05
set color_background "1C/20/23" # Base 00

if set -q TMUX
  function put_template
    printf '\033Ptmux;\033\033]4;%d;rgb:%s\033\033\\\033\\' $argv
  end
  function put_template_var
    printf '\033Ptmux;\033\033]%d;rgb:%s\033\033\\\033\\' $argv
  end
  function put_template_custom
    printf '\033Ptmux;\033\033]%s%s\033\033\\\033\\' $argv
  end
else if string match -q "screen*" $TERM
  function put_template
    printf '\033P\033]4;%d;rgb:%s\007\033\\' $argv
  end
  function put_template_var
    printf '\033P\033]%d;rgb:%s\007\033\\' $argv
  end
  function put_template_custom
    printf '\033P\033]%s%s\007\033\\' $argv
  end
else if string match -q "linux" $TERM
  function put_template
    if test $argv[1] -lt 16
      printf "\e]P%x%s" $argv[1] (string replace -a '/' '' $argv[2])
    end
  end
  function put_template_var; end
  function put_template_custom; end
else
  function put_template
    printf '\033]4;%d;rgb:%s\033\\' $argv
  end
  function put_template_var
    printf '\033]%d;rgb:%s\033\\' $argv
  end
  function put_template_custom
    printf '\033]%s%s\033\\' $argv
  end
end

# 16 color space
put_template 0  $color00
put_template 1  $color01
put_template 2  $color02
put_template 3  $color03
put_template 4  $color04
put_template 5  $color05
put_template 6  $color06
put_template 7  $color07
put_template 8  $color08
put_template 9  $color09
put_template 10 $color10
put_template 11 $color11
put_template 12 $color12
put_template 13 $color13
put_template 14 $color14
put_template 15 $color15

# 256 color space
put_template 16 $color16
put_template 17 $color17
put_template 18 $color18
put_template 19 $color19
put_template 20 $color20
put_template 21 $color21

# foreground / background / cursor color
if set -q ITERM_SESSION_ID
  put_template_custom Pg C7CCD1 # foreground
  put_template_custom Ph 1C2023 # background
  put_template_custom Pi C7CCD1 # bold color
  put_template_custom Pj 565E65 # selection color
  put_template_custom Pk C7CCD1 # selected text color
  put_template_custom Pl C7CCD1 # cursor
  put_template_custom Pm 1C2023 # cursor text
else
  put_template_var 10 $color_foreground
  if test "$BASE16_SHELL_SET_BACKGROUND" != "false"
    put_template_var 11 $color_background
    if string match -q "rxvt" $TERM
      put_template_var 708 $color_background # internal border (rxvt)
    end
  end
  put_template_custom 12 ";7" # cursor (reverse video)
end

# clean up
functions -e put_template
functions -e put_template_var
functions -e put_template_custom
set -e color00
set -e color01
set -e color02
set -e color03
set -e color04
set -e color05
set -e color06
set -e color07
set -e color08
set -e color09
set -e color10
set -e color11
set -e color12
set -e color13
set -e color14
set -e color15
set -e color16
set -e color17
set -e color18
set -e color19
set -e color20
set -e color21
set -e color_foreground
set -e color_background
