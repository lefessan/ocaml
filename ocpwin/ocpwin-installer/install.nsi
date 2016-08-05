; set RELEASE_DATE
; set OCAML_VERSION
; set OCPWIN_ARCH (also used in install_lines_{...}.nsi)
; set MINGW_PREFIX (x86_64-w64-mingw32 for 64 bits, different for 32 bits)
!include "config.nsi"

; set PRODUCT_OPTION
; set C_COMPILER
!include "system.nsi"


!define FULL_COMPILER "${C_COMPILER}${OCPWIN_ARCH}"
!define MUI_VERSION "${RELEASE_DATE}"
!define MUI_SOURCE_DIR "local"
!define MUI_SYSTEMS "${OCAML_VERSION}-${FULL_COMPILER}"

; used in install_lines_{...}.nsi
!define MINGW_SYSTEM "${FULL_COMPILER}"


!include "MUI2.nsh"
!include "winmessages.nsh"
!include "LogicLib.nsh"
; !include "IfKeyExists.nsh"

; For now, we do not support MultiUser installations
!define MULTIUSER_EXECUTIONLEVEL Standard
!include "MultiUser.nsh"
!include "FileFunc.nsh"
!include "${OCPNSISDIR}/DumpLog.nsh"

!define OCPWIN_UID "${MUI_SYSTEMS}-${MUI_VERSION}"
!define OCPWIN_UNINSTALL_KEY \
   "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCPWin-${OCPWIN_UID}"
!define OCPWIN_INSTALL_KEY "Software\OCamlPro\OCPWin\${OCPWIN_UID}"

!define MUI_PRODUCT "OCamlPro\OCPWin\${OCPWIN_UID}"

!define OCPWIN_DISPLAY_NAME "OCPWin ${MUI_VERSION} ${MUI_SYSTEMS}"

Name "OCamlPro OCPWin${OCPWIN_ARCH}"
OutFile "ocpwin${OCPWIN_ARCH}-${MUI_VERSION}-${MUI_SYSTEMS}.exe"
InstallDir "$PROGRAMFILES\${MUI_PRODUCT}"
!define ROOT_DIR "files"











!define SHCNE_ASSOCCHANGED 0x8000000
!define SHCNF_IDLIST 0

!define env_all     '"SYSTEM\CurrentControlSet\Control\Session Manager\Environment"'
!define env_current '"Environment"'














!define MUI_WELCOMEPAGE_TITLE "OCamlPro's OCPWin${OCPWIN_ARCH} installer."
!define MUI_WELCOMEPAGE_TEXT \
   "This wizard will install OCPWin${OCPWIN_ARCH} version ${MUI_VERSION}, \
for ${MUI_SYSTEMS}.$\n \
OCPWin${OCPWIN_ARCH} is a version of OCaml specially packaged by OCamlPro \
to work better on the Windows operating system. \
   "
!insertmacro MUI_PAGE_WELCOME

!define MUI_STARTMENUPAGE_REGISTRY_ROOT "SHCTX"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\OCamlPro"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
Var STARTMENUFOLDER
!insertmacro MUI_PAGE_STARTMENU Application $STARTMENUFOLDER

!define MUI_LICENSEPAGE_TEXT_TOP "This version of OCaml is distributed under a specific license.$\n"
!define MUI_LICENSEPAGE_TEXT_BOTTOM "You must agree with the terms of that license below before installing this softare."
!insertmacro MUI_PAGE_LICENSE ../../../ocpwin/LICENSE.txt

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"













Function .onInit
  !insertmacro MULTIUSER_INIT

  ; Check to see if already installed
  ReadRegStr $R0 SHCTX "${OCPWIN_UNINSTALL_KEY}" "UninstallString"
  StrCmp $R0 "" NotInstalled

  MessageBox MB_OKCANCEL|MB_ICONEXCLAMATION \
  "${OCPWIN_DISPLAY_NAME} is already installed. $\n$\nClick `OK` to remove the \
  previous version or `Cancel` to cancel this upgrade." \
  IDOK CallUninstall
  Abort

CallUninstall:
  Exec $R0 ; instead of the ExecWait line

NotInstalled:

  ${If} $MultiUser.InstallMode == "AllUsers"

      ReadRegStr $0 HKLM "${OCPWIN_INSTALL_KEY}" "CurrentInstallFolder"
      ${If} $0 != ""
         StrCpy $INSTDIR "$0"
      ${Else}
         StrCpy $INSTDIR  "$PROGRAMFILES\${MUI_PRODUCT}"
      ${EndIf}

  ${ElseIf} $MultiUser.InstallMode == "CurrentUser"

     ReadRegStr $0 HKCU "${OCPWIN_INSTALL_KEY}" "CurrentInstallFolder"
     ${If} $0 != ""
        StrCpy $INSTDIR "$0"
     ${Else}
        StrCpy $INSTDIR "$APPDATA\${MUI_PRODUCT}"
     ${EndIf}

  ${EndIf}

FunctionEnd














Section "OCaml" SecOCaml

  ; LogSet on

  SetOutPath "$INSTDIR"
  File /r ${ROOT_DIR}/bin
  File /r ${ROOT_DIR}/lib

  File ${ROOT_DIR}/Changes.ocaml.txt
  File ${ROOT_DIR}/License.flexdll.txt
  File ${ROOT_DIR}/License.ocamlpro.txt
  File ${ROOT_DIR}/Readme.ocamlpro.txt
  File ${ROOT_DIR}/License.inria.txt
  File ${ROOT_DIR}/Readme.inria.txt
  ; File ${ROOT_DIR}/use_ocaml.bat
  File ${ROOT_DIR}/build-camlp4.cmd

  File /r ${ROOT_DIR}/cmas
  !include "${OCPNSISDIR}/install_lines_${PRODUCT_OPTION}.nsi"

  ${If} $MultiUser.InstallMode == "AllUsers"

    ; This is for the OCamlTopWin thing
    WriteRegStr HKLM "Software\Objective Caml" "InterpreterPath" "$INSTDIR\bin\ocaml.exe"

      ExpandEnvStrings $0 %COMSPEC%
      nsExec::ExecToLog '"$0" "/C" "$INSTDIR\bin\ocpwin" "-installer" "machine" "${OCPWIN_UID}" "$INSTDIR"'

  ${ElseIf} $MultiUser.InstallMode == "CurrentUser"

     ; This is for the OCamlTopWin thing
    WriteRegStr HKCU "Software\Objective Caml" "InterpreterPath" "$INSTDIR\bin\ocaml.exe"



      ExpandEnvStrings $0 %COMSPEC%
      nsExec::ExecToLog '"$0" "/C" "$INSTDIR\bin\ocpwin" "-installer" "user" "${OCPWIN_UID}" "$INSTDIR"'

  ${Else}
    SetErrors
    DetailPrint "Error: $MultiUser.InstallMode unexpected value"
  ${EndIf}

  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  ExpandEnvStrings $0 %COMSPEC%
  nsExec::ExecToLog '"$0" "/C" "$INSTDIR\build-camlp4.cmd" "$INSTDIR"'
;;  ExecWait '"$INSTDIR\build-camlp4.cmd" "$INSTDIR"'
;;  Delete "$INSTDIR\build-camlp4.cmd"

  WriteRegExpandStr SHCTX "${OCPWIN_UNINSTALL_KEY}" "UninstallString" \
        "$\"$INSTDIR\uninstall.exe$\""
  WriteRegExpandStr SHCTX "${OCPWIN_UNINSTALL_KEY}" "InstallLocation" "$INSTDIR"
  WriteRegStr SHCTX "${OCPWIN_UNINSTALL_KEY}" "DisplayName" "${OCPWIN_DISPLAY_NAME}"
  WriteRegStr SHCTX "${OCPWIN_UNINSTALL_KEY}" "DisplayIcon" "$INSTDIR\ocaml-icon.ico"
  WriteRegStr SHCTX "${OCPWIN_UNINSTALL_KEY}" "DisplayVersion" "${MUI_VERSION}"
  WriteRegStr SHCTX "${OCPWIN_UNINSTALL_KEY}" "Publisher" "OCamlPro"

  ${GetSize} "$INSTDIR" "/S=0K" $0 $1 $2
  IntFmt $0 "0x%08X" $0
  WriteRegDWORD SHCTX "${OCPWIN_UNINSTALL_KEY}" "EstimatedSize" "$0"
 
  WriteUninstaller $INSTDIR\uninstall.exe

  StrCpy $0 "$INSTDIR\install.log"
  Push $0
  Call DumpLog

SectionEnd















Function un.onInit
  !insertmacro MULTIUSER_UNINIT
FunctionEnd

Section "Uninstall"

  ${If} $MultiUser.InstallMode == "AllUsers"


  ExpandEnvStrings $0 %COMSPEC%
  nsExec::ExecToLog '"$0" "/C" "$INSTDIR\bin\ocpwin" "-uninstaller" "machine" "${OCPWIN_UID}" "$INSTDIR"'

  ${ElseIf} $MultiUser.InstallMode == "CurrentUser"

  ExpandEnvStrings $0 %COMSPEC%
  nsExec::ExecToLog '"$0" "/C" "$INSTDIR\bin\ocpwin" "-uninstaller" "user" "${OCPWIN_UID}" "$INSTDIR"'

  ${Else}
    SetErrors
    DetailPrint "Error: $MultiUser.InstallMode unexpected value"
  ${EndIf}

  !include "uninstall_lines.nsi"
  Delete "$INSTDIR\uninstall.exe"
  Delete "$INSTDIR\bin\camlp4boot.exe"
  Delete "$INSTDIR\bin\camlp4o.exe"
  Delete "$INSTDIR\bin\camlp4oof.exe"
  Delete "$INSTDIR\bin\camlp4rf.exe"
  Delete "$INSTDIR\bin\camlp4.exe"
  Delete "$INSTDIR\bin\camlp4of.exe"
  Delete "$INSTDIR\bin\camlp4orf.exe"
  Delete "$INSTDIR\bin\camlp4r.exe"

  Delete "$INSTDIR\lib\camlp4\camlp4lib.cma"
  Delete "$INSTDIR\lib\camlp4\camlp4fulllib.cma"
  Delete "$INSTDIR\lib\camlp4\camlp4o.cma"
  Delete "$INSTDIR\lib\camlp4\camlp4oof.cma"
  Delete "$INSTDIR\lib\camlp4\camlp4r.cma"
  Delete "$INSTDIR\lib\camlp4\camlp4of.cma"
  Delete "$INSTDIR\lib\camlp4\camlp4orf.cma"
  Delete "$INSTDIR\lib\camlp4\camlp4rf.cma"

  RMDir /r "$INSTDIR\cmas"
  RMDir "$INSTDIR"

  ${If} $MultiUser.InstallMode == "AllUsers"

    DeleteRegKey HKLM "${OCPWIN_UNINSTALL_KEY}"
    DeleteRegKey HKLM "${OCPWIN_INSTALL_KEY}"

  ${ElseIf} $MultiUser.InstallMode == "CurrentUser"

    DeleteRegKey HKCU "${OCPWIN_UNINSTALL_KEY}"
    DeleteRegKey HKCU "${OCPWIN_INSTALL_KEY}"

  ${Else}
    SetErrors
    DetailPrint "Error: $MultiUser.InstallMode unexpected value"
  ${EndIf}

  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

SectionEnd
