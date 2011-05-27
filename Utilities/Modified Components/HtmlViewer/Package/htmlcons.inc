{Version 7.01}

{$A+,B-,F-,G+,I+,P+,T-,V+,X+,R-}

{$ifdef Win32}
  {$J+}  {typed constants are modifiable}
{$endif}

{$ifndef DebugIt}
  {$W-}  {Stack frames off}
  {$Q-}  {overflow checking off}
  {$S-}  {stack checking off}
  {$ifdef Win32}
    {$O+}  {optimization on}
  {$endif}
{$else}
  {$W+}  {Stack frames on}
  {$Q+}  {overflow checking on}
  {$S+}  {stack checking on}
  {$ifdef Win32}
    {$O-}  {optimization off}
  {$endif}
{$endif}

{$ifdef ver110}    {C++Builder 3}
{$ObjExportAll On}
{$Define CppBuilder}
{$Define Delphi3_4_CppBuilder3_4}
{$endif}

{$ifdef ver125}    {C++Builder 4}
{$ObjExportAll On}  
{$Define CppBuilder}
{$Define Delphi3_4_CppBuilder3_4}
{$endif}

{$ifdef Ver120}   {Delphi 4}
{$Define Delphi4}
{$Define Delphi3_4_CppBuilder3_4}
{$Define Delphi3_4}
{$endif}

{$ifdef ver100}   {Delphi 3}
{$Define Delphi3_4_CppBuilder3_4}
{$Define Delphi3_4}
{$endif}

{$ifdef ver93}     {C++Builder 1}
{$Define CppBuilder}
{$endif}

{.$Define NoDLL} {To eliminate the need for the graphics DLL, define "NoDLL"
           by removing the '.'.  Then do a Build All.

           When the graphics DLL is not used, GIF and bitmap capability will
           remain and there will be JPEG capability only in the Delphi 3 and 4
           versions.  There will be no PNG capability.
           There will also be no color dithering and bitmap and GIF quality may
           suffer.}

{.$Define NoGIF} {To eliminate GIF image capability, define "NoGIF" by
           removing the '.'.}

{Note:  The NoUnderline and PrintTableBackground compiler options are now options
 set in the htOptions and fvOptions properties. }

