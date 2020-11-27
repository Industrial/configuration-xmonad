-- XMonad itself.
import XMonad

-- XMobar
import System.IO
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn

-- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.Reflect(reflectHoriz)
import XMonad.Layout.Tabbed

-- Used for quitting the X session.
import System.Exit

-- Puspose unknown.
import Data.Monoid
import XMonad.Actions.CycleWS
import qualified Data.Map        as Map
import qualified XMonad.StackSet as StackSet

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc"
  xmonad $ docks $ myConfig xmproc

myStartupHook = do
  spawn "~/.xmonad/autostart"
  spawnOn "Q:Browser" "chromium-browser --new-window https://github.com"
  spawnOn "W:Editor" "~/.bin/terminal"
  spawnOn "E:Terminal" "~/.bin/terminal"
  spawnOn "R:Git" "gitfiend"
  spawnOn "T:Media" "spotify"
  spawnOn "T:Media" "~/.bin/terminal -e sh -c \"tmux attach -t media\""
  -- spawnOn "Y:Social" "slack"
  -- spawnOn "Y:Social" "discord"
  spawnOn "Y:Social" "~/.bin/terminal -e sh -c \"tmux attach -t social\""
  spawnOn "I:System" "~/.bin/terminal -e sh -c \"tmux attach -t system\""
  spawnOn "I:System" "xdman"
  -- spawnOn "U:Games" "lutris"
  spawnOn "O:Office" "chromium-browser --new-window https://google.com"
  spawnOn "P:Scratch" "~/.bin/terminal"

myTheme = defaultTheme {
  activeColor       = "#336699",
  activeBorderColor = "#336699",
  activeTextColor   = "#336699",

  inactiveColor       = "#333333",
  inactiveBorderColor = "#333333",
  inactiveTextColor   = "#333333",

  urgentColor       = "#ff0000",
  urgentBorderColor = "#ff0000",
  urgentTextColor   = "#ff0000"
}

myConfig xmproc = defaultConfig {
  -- simple stuff
  terminal = "~/.bin/terminal",

  focusFollowsMouse = True,
  clickJustFocuses = False,

  modMask = mod4Mask,

  workspaces = [
    "Q:Browser",
    "W:Editor",
    "E:Terminal",
    "R:Git",
    "T:Media",
    "Y:Social",
    "U:Games",
    "I:System",
    "O:Office",
    "P:Scratch"
  ],

  borderWidth = 1,

  normalBorderColor = "#000000",
  focusedBorderColor = "#336699",
  --focusedBorderColor = "#339966",

  -- key bindings
  keys = myKeys,
  mouseBindings = myMouseBindings,

  -- hooks, layouts
  layoutHook = avoidStruts $ myLayout,
  manageHook = manageSpawn <+> manageDocks <+> manageHook defaultConfig,

  -- TODO: Find out what this does
  handleEventHook = mempty,

  logHook = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn xmproc
  },

  startupHook = myStartupHook
}

--myLayout = avoidStruts $ tiled ||| reflectHoriz tiled ||| Grid ||| tabbed shrinkText myTheme ||| Full
myLayout = tiled ||| reflectHoriz tiled ||| Grid ||| simpleTabbed ||| Full
  where
    tiled        = Tall tiledNmaster tiledDelta tiledRatio
    tiledNmaster = 1
    tiledDelta   = 1/100
    tiledRatio   = 1/2

myMouseBindings XConfig {XMonad.modMask = modm} = Map.fromList
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                    >> windows StackSet.shiftMaster),

    -- mod-button2, Raise the window to the top of the stack
    ((modm, button2), \w -> focus w >> windows StackSet.shiftMaster),

    -- mod-button3, Set the window to floating mode and resize by dragging
    ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                     >> windows StackSet.shiftMaster)
  ]

myKeys conf@ XConfig {XMonad.modMask = modm}  = Map.fromList $
  [
    -- launch a terminal
    ((modm, xK_Return), spawn $ XMonad.terminal conf),
    -- launch dmenu
    ((modm .|. shiftMask, xK_Return), spawn "dmenu_run -fn 'Terminess Powerline:style=Bold:size=12'"),

    -- close focused window
    ((modm .|. shiftMask, xK_c), kill),

    -- Rotate through the available layout algorithms
    ((modm, xK_space ), sendMessage NextLayout),
    --  Reset the layouts on the current workspace to default
    ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),

    -- Move focus to the next window
    ((modm, xK_j), windows StackSet.focusDown),
    -- Move focus to the previous window
    ((modm, xK_k), windows StackSet.focusUp),
    -- Swap the focused window with the next window
    ((modm .|. shiftMask, xK_j), windows StackSet.swapDown),
    -- Swap the focused window with the previous window
    ((modm .|. shiftMask, xK_k), windows StackSet.swapUp),

    -- Go to the previous workspace
    ((modm, xK_h), prevWS),
    -- Go to the next workspace
    ((modm, xK_l), nextWS),
    -- Move to the previous workspace
    ((modm .|. shiftMask, xK_h), shiftToPrev >> prevWS),
    -- Move to the next workspace
    ((modm .|. shiftMask, xK_l), shiftToNext >> nextWS),

    -- Shrink the master area
    ((modm, xK_minus), sendMessage Shrink),
    -- Expand the master area
    ((modm, xK_equal), sendMessage Expand),


    -- Increment the number of windows in the master area
    ((modm, xK_comma), sendMessage (IncMasterN 1)),

    -- Deincrement the number of windows in the master area
    ((modm, xK_period), sendMessage (IncMasterN (-1))),

    -- Swap the focused window and the master window
    ((modm, xK_b), windows StackSet.swapMaster),

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    ((modm .|. shiftMask, xK_b), sendMessage ToggleStruts),

    -- Push window back into tiling
    ((modm .|. shiftMask .|. controlMask, xK_b), withFocused $ windows . StackSet.sink),

    -- Restart xmonad
    ((modm, xK_backslash), spawn "xmonad --recompile; xmonad --restart"),

    -- Quit xmonad
    ((modm .|. shiftMask, xK_backslash), io exitSuccess)
  ]
  ++

  --
  -- mod-[1..9], Switch to workspace N
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [
    ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf)
        [
          xK_q, xK_w, xK_e, xK_r, xK_t, xK_y, xK_u, xK_i, xK_o, xK_p
          -- xK_a, xK_s, xK_d, xK_f, xK_g, xK_h, xK_j, xK_k, xK_l, xK_semicolon
        ],
        (f, m) <- [(StackSet.greedyView, 0), (StackSet.shift, shiftMask)]
  ]
  ++

  --
  -- mod-{u,i,o}, Switch to physical/Xinerama screens 1, 2, 3
  -- mod-shift-{u,i,o}, Move client to screen 1, 2, 3
  --
  [
    ((m .|. modm .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_o, xK_i, xK_u] [0..],
        (f, m) <- [(StackSet.view, 0), (StackSet.shift, shiftMask)]
  ]
