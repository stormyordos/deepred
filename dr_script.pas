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

//==============================================================================
// TPascalScriptHelper
//==============================================================================

constructor TPascalScriptHelper.Create;
begin
     _compiler := TPSPascalCompiler.Create;
end;


destructor TPascalScriptHelper.Destroy;
begin
     _compiler.Free;
end;


procedure TPascalScriptHelper.SaveCompiled(var data : String; outName : string);
var
   outFile: string;
   fx: Longint;
begin
     OutFile:= ExtractFilePath(ParamStr(0)) + ChangeFileExt(outName,'.out');
     Fx:= FileCreate(outFile);
     FileWrite(fx,data[1],Length(data));
     FileClose(fx);
end;


procedure TPascalScriptHelper.SaveDisassembly(var data: string; outName : string);
var
   outFile: string;
   fx: Longint;
begin
     OutFile:= ExtractFilePath(ParamStr(0)) + ChangeFileExt(outName,'.dis');
     fx:= FileCreate(outFile);
     FileWrite(fx, data[1], Length(data));
     FileClose(fx);
end;


function TPascalScriptHelper.CompileScript(script: string; out bytecode, messages: string): boolean;
var
    i: Integer;
begin
     bytecode := '';
     messages := '';

     _compiler.OnUses := @ExtendCompiler;

     try
        Result := _compiler.Compile(script) and _compiler.GetOutput(bytecode);
        for i := 0 to _compiler.MsgCount - 1 do
            if Length(messages) = 0 then
               messages := _compiler.Msg[i].MessageToString
            else
               messages := messages + #13#10 + _compiler.Msg[i].MessageToString;
     finally
        _compiler.Free;
    end;
end;


function TPascalScriptHelper.RunCompiledScript(bytecode: string; out runtimeErrors: string): boolean;
var
   runtime: TPSExec;
   classImporter: TPSRuntimeClassImporter;
begin
     runtime := TPSExec.Create;
     classImporter := TPSRuntimeClassImporter.CreateAndRegister(runtime, false);
     try
        ExtendRuntime(runtime, classImporter);
        Result := runtime.LoadData(Bytecode) and runtime.RunScript
                  and (runtime.ExceptionCode = erNoError);
        if not Result then
           runtimeErrors :=  PSErrorToString(runtime.LastEx, '');
     finally
        classImporter.Free;
        runtime.Free;
     end;
end;


procedure TPascalScriptHelper.ExtendRuntime(runtime: TPSExec; classImporter: TPSRuntimeClassImporter);
var
   runtimeClass: TPSRuntimeClass;
begin
     //runtime.RegisterDelphiMethod(Self, @TForm1.MyPrint, 'print', cdRegister);
     runtime.RegisterDelphiFunction(@MWrites, 'procedure Writes(const s: string)', cdRegister);
     runtime.RegisterDelphiFunction(@MWriteDT, 'procedure WriteDT(d : TDateTime)', cdRegister);
     runtime.RegisterDelphiFunction(@MWritei, 'procedure Writei(const i: Integer)', cdRegister);
     runtime.RegisterDelphiFunction(@MWrited, 'procedure Writed(const f: Double)', cdRegister);
     runtime.RegisterDelphiFunction(@MWriteln, 'procedure Writeln', cdRegister);
     runtime.RegisterDelphiFunction(@MVal, 'procedure Val(const s: string; var n, z: Integer)', cdRegister);
     runtime.RegisterDelphiFunction(@FileCreate, 'function FileCreate(const FileName: string): integer)', cdRegister);
     runtime.RegisterDelphiFunction(@FileWrite, 'function FileWrite(Handle: Integer; const Buffer: pChar; Count: LongWord): Integer)', cdRegister);
     runtime.RegisterDelphiFunction(@FileClose, 'procedure FileClose(handle: integer)', cdRegister);

     RIRegister_Std(classImporter);
     RIRegister_Classes(classImporter,true);
     //RIRegister_Controls(classImporter);
     //RIRegister_Forms(classImporter);
     RegisterDateTimeLibrary_R(runtime);
     RegisterDLLRuntime(runtime);

     RIRegisterTObject(classImporter);
     //RuntimeClass := ClassImporter.Add(TAccumulator);
     //RuntimeClass.RegisterMethod(@TAccumulator.Add, 'Add');
     //RuntimeClass.RegisterMethod(@TAccumulator.GetTotal, 'GetTotal');
end;


(*
procedure TPascalScriptHelper.OnExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
     RIRegister_Std(x);
     RIRegister_Classes(x,true);
     RIRegister_Controls(x);
     RIRegister_Forms(x);
     RegisterDateTimeLibrary_R(se);
     RegisterDLLRuntime(se);
end;


procedure TPascalScriptHelper.OnCompile(Sender: TPSScript);
begin
     RegisterDateTimeLibrary_C(Sender.Comp);
     Sender.AddFunction(@MWrites, 'procedure Writes(const s: string)');
     Sender.AddFunction(@MWritedt,'procedure WriteDT(d : TDateTime)');
     Sender.AddFunction(@MWritei, 'procedure Writei(const i: Integer)');
     Sender.AddFunction(@MWrited, 'procedure Writed(const f: Double)');
     Sender.AddFunction(@MWriteln, 'procedure Writeln');
     Sender.AddFunction(@MyVal, 'procedure Val(const s: string; var n, z: Integer)');
     Sender.AddFunction(@FileCreate, 'Function FileCreate(const FileName: string): integer)');
     Sender.AddFunction(@FileWrite, 'function FileWrite(Handle: Integer; const Buffer: pChar; Count: LongWord): Integer)');
     Sender.AddFunction(@FileClose, 'Procedure FileClose(handle: integer)');
     //Sender.AddRegisteredVariable('Application', 'TApplication');
     SIRegister_Std(Sender.Comp);
     SIRegister_Classes(Sender.Comp,true);
     SIRegister_Controls(Sender.Comp);
     SIRegister_Forms(Sender.Comp);
end;


function TPascalScriptHelper.Compile(const FileName: string): Boolean;
var
   S: TStringList;
   i: Integer;
begin
     Result:= False;
     if FileExists(FileName) then
     begin
          S:= TStringList.Create;
          S.LoadFromFile(FileName);
          _script.Script:= S;
          Result:= _script.Compile;
          for i:= 0 to _script.CompilerMessageCount - 1 do
              writeln(_script.CompilerMessages[i].MessageToString);
          S.Free;
          if not Result then
             if _script.CompilerMessageCount > 0 then
                for i:= 0 to _script.CompilerMessageCount-1 do
                    Writeln(_script.CompilerErrorToStr(i));
             end else
                Writeln('Script File not found: ', FileName);
end;

function TPascalScriptHelper.Execute: Boolean;
begin
     //_script.SetVarToInstance('APPLICATION', Application);
     //_script.SetVarToInstance('SELF', Self);
     Result:= _script.Execute;
     //writeln(_script.About);
     if not Result then
        Writeln('Run-time error:' + _script.ExecErrorToString);
end;
    *)



end.

