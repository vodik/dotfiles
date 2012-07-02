## XMonad

My comprehensive XMonad config.

Depends on [udiskie][], [compton][], [dzen2][].

```
cabal install xmonad xmonad-contrib regex-posix libmpd
```

  [udiskie]: https://aur.archlinux.org/packages.php?ID=37279
  [compton]: https://aur.archlinux.org/packages.php?ID=55375
  [dzen2]: http://aur.archlinux.org/packages.php?ID=14470

### Icons

This XMonad config will automatically load workspace icons from
``$HOME/.xmonad/icons`` and layout icons from
``$HOME/.xmonad/icons/layouts``. Workspaces lacking icons are handled
specially.

### Libraries

I've written an extensive library of xmonad modules, here's a run down.

#### Hooks

| Module   | Description                   |
| :------- | :---------------------------- |
| VodikLog | My dzen based pretty printer. |

#### Layouts

| Module       | Description                                                           |
| :----------- | :-------------------------------------------------------------------- |
| BalancedTile | Like ``ResizableTile`` but with a twist.                              |
| GuardLayout  | Like ``onWorkspace`` but with a wider range of conditions.            |
| MinimizePlus | Like Minimize but with a few more features.                           |
| SortWindows  | Put two layouts side by side and specify the rules to sort them with. |
| WindowGaps   | Add *consistent* gaps around windows.                                 |

#### Commands

| Module      | Description                                                          |
| :---------- | :------------------------------------------------------------------- |
| Commands    | A different take on spawning commands.                               |
| CycleWS     | Helpers for xmonad-contrib's ``CycleWS.``                            |
| Environment | Helpers to get certain environmental variables.                      |
| MPD         | MPD integration through ``libmpd``. Connects to mpd daemon directly. |
| Services    | Simple process management.                                           |
| Tmux        | A wrapper around ``tmux``.                                           |
| Undo        | A stack of undoable operations. Not currently used.                  |
