
The ocpwin.exe tool
===================

It is currently used for 3 main purposes:
* Post-install configuration of an OCPWin switch (`-installer` argument)
* Pre-uninstall cleaning of an OCPWin switch (`-uninstaller` argument)
* Management of OCPWin switches (other arguments)

Post-install configuration of an OCPWin switch
----------------------------------------------
Registry keys are stored in $OCPWIN_KEY=`Software\OCamlPro\OCPWin`.

ocpwin.exe receives the `switch_name` and `switch_dir`:
1/ it adds the switch in the list of switches 
2/ it records the switch installation directory in
  $OCPWIN_KEY\$SWITCH:CurrentInstallFolder
3/ it adds the switch bindir in the user PATH. The bindir added to the path
  is recorded in $OCPWIN_KEY:CurrentBinDir, and the associated switch name
  in $OCPWIN_KEY:CurrentSwitch.
4/ Finally, it also sets $OCPWIN_KEY:CurrentOcpwinWrapper to the full path
  of ocpwin.exe in case multi-switch mode is used.
5/ If it is the first install, it will also create
  $OCPWIN_KEY:MultiSwitchDir to a directory that can contain wrappers
  in multi-switch mode.
  This directory is _always_ added on top of the current switch bindir in
  the PATH.

Pre-uninstall cleaning of an OCPWin switch
------------------------------------------

ocpwin.exe receives the `switch_name` and `switch_dir`:
1/ it removes the switch from the list of switches 
2/ it forgets the switch installation directory
3/ it removes the switch bindir from the user PATH, if present

List of switches
----------------

The list of switches is stored as:
* A key $OCPWIN_KEY:ListHead, containing the name of the first switch, or "nil".
* A key $OCPWIN_KEY\$SWITCH:ListTail in each switch $SWITCH, containing the
    next switch name, or "nil"

Multi-switch mode
-----------------

There are two modes to work with multiple OCPWin versions:

* By default, the bindir of one switch is added to the PATH. It is
 possible to use `ocpwin -switch SWITCH_NAME` to change that switch,
 but the modification to the PATH variable will only be taken into
 account in another terminal.

* In multi-switch mode, a wrappers directory is created for each switch,
 and it is that wrappers directory that is added to the PATH. Wrappers
 call `ocpwin.exe` to find the current switch and call the according
 command.
