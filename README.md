# Vodik's Dotfiles

My comprehensive repository of configured software.  After deployed, it
should Just Work™ with both ``startx`` and [lxdm][].

Vim bundles are included through submodules. Make sure to:

```
git submodules init
git submodules update
```

Some details on the more highly customized programs:

|                 | Description                                       |
| --------------- | :------------------------------------------------ |
| **X**           | Xresource, xinitrc, and gtk settings              |
| **mutt**        | Mutt to access gmail                              |
| **ncmpcpp**     | Music and media library management                |
| **pentadactyl** | Web browser settings                              |
| **termite**     | Terminal emulator of choice                       |
| **tmux**        | Terminal multiplexer config                       |
| **vim**         | My vim and gvim config and all the bundles I use. |
| **weechat**     | My weechat settings, theme, and plugins I use.    |
| **xmonad**      | My window manager, see [README][xmonad]           |
| **zsh**         | My zsh config                                     |

  [lxdm]: https://wiki.archlinux.org/index.php/LXDM
  [vim]: https://github.com/vodik/dotfiles/blob/master/vim/README.md
  [xmonad]: https://github.com/vodik/dotfiles/blob/master/xmonad/README.md
