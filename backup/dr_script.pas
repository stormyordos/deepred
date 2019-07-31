unit dr_script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
  , uPSCompiler, uPSRuntime, uPSComponent, uPSDisassembly, uPSR_std, uPSC_std,
    uPSR_classes, uPSC_classes, (*uPSC_controls, uPSR_controls, uPSC_forms, uPSR_forms,*)
    uPSR_dateutils, uPSC_dateutils, uPSR_dll, uPSC_dll
  ;

type
    TPascalScriptHelper =
    class (TObject)
          protected
                _compiler : TPSPascalCompiler;
                procedure SaveCompiled(var Data: string; outName : string = 'out');
                procedure SaveDisassembly(var Data: string; outName : string = 'dis');
                procedure ExtendRuntime(runtime: TPSExec; classImporter: TPSRuntimeClassImporter);
               (* procedure OnCompile(Sender: TPSPascalCompiler);
                procedure OnExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter); *)
          public
                constructor Create;
                destructor Destroy; override;
                function CompileScript(script: string; out bytecode, messages: string): boolean;
                function RunCompiledScript(bytecode: string; out runtimeErrors: string): boolean;
                (*function Compile(const FileName: string): Boolean;
                function Execute: Boolean;*)
    end;


function ExtendCompiler(compiler: TPSPascalCompiler; const name: string): boolean;


implementation


procedure MWritedt(d : TDateTime);
var
   s: String;
begin
     s:= DateToStr(d) + ' ' + TimeToStr(d);
     Write(s);
end;


procedure MWrites(const s: string);
begin
     Write(s);
end;


procedure MWritei(const i: Integer);
begin
     Write(i);
end;


procedure MWrited(const d: Double);
begin
     Write(d:0:1);
end;


procedure MWriteln;
begin
     Writeln;
end;


procedure MVal(const s: string; var n, z: Integer);
begin
     Val(s, n, z);
end;


function ExtendCompiler(compiler: TPSPascalCompiler; const name: string): boolean;
var
   customClass: TPSCompileTimeClass;
begin
     try
        //compiler.AddDelphiFunction('procedure print(const AText: AnsiString);');
        RegisterDateTimeLibrary_C(compiler);
        compiler.AddDelphiFunction('procedure Writes(const s: string)');
        compiler.AddDelphiFunction('procedure WriteDT(d : TDateTime)');
        compiler.AddDelphiFunction('procedure Writei(const i: Integer)');
        compiler.AddDelphiFunction('procedure Writed(const f: Double)');
        compiler.AddDelphiFunction('procedure Writeln');
        compiler.AddDelphiFunction('procedure Val(const s: string; var n, z: Integer)');
        compiler.AddDelphiFunction('function FileCreate(const FileName: string): integer)');
        compiler.AddDelphiFunction('function FileWrite(Handle: Integer; const Buffer: pChar; Count: LongWord): Integer)');
        compiler.AddDelphiFunction('procedure FileClose(handle: integer)');
        //Sender.AddRegisteredVariable('Application', 'TApplication');
        SIRegister_Std(compiler);
        SIRegister_Classes(compiler,true);
        //SIRegister_Controls(compiler);
        //SIRegister_Forms(compiler);

        SIRegisterTObject(compiler); // Add compile-time definition for TObject
        //customClass := compiler.AddClass(compiler.FindClass('TObject'), TAccumulator);
        //customclass.RegisterMethod('procedure Add(AValue: Integer)');
        //customClass.RegisterMethod('function GetTotal: Integer');
        Result := True;
     except
        Result := False; // will halt compilation
     end;
end;




end.

