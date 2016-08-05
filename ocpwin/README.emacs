All the files mentioned above should go in your HOME directory. The
HOME directory is determined by following the steps below:

If the environment variable HOME is set, use the directory it
indicates.

If the registry entry HKCU\SOFTWARE\GNU\Emacs\HOME is set, use the
directory it indicates.

If the registry entry HKLM\SOFTWARE\GNU\Emacs\HOME is set, use the
directory it indicates. Not recommended, as it results in users
sharing the same HOME directory.

If C:\.emacs exists, then use C:/. This is for backward compatibility,
as previous versions defaulted to C:/ if HOME was not set.

Use the user’s AppData directory, usually a directory called AppData
under the user’s profile directory, the location of which varies
according to Windows version and whether the computer is part of a
domain.

Within Emacs, ~ at the beginning of a file name is expanded to your
HOME directory, so you can always find your .emacs file by typing the
command C-x C-f ~/.emacs.

