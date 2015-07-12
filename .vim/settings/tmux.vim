" tmuxiline
let g:tmuxline_preset = {
      \'b'    : ['#S:#I'],
      \'win'  : ['#I','#W'],
      \'cwin' : ['#I','#W #F'],
      \'x'       : ['%a %d.%m.%Y', '%R %Z'],
      \'y'       : ['#(whoami)'],
      \'options' : {'status-justify': 'left'}}
