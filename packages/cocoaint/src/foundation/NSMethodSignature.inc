{ Parsed from Foundation.framework NSMethodSignature.h }


{$ifdef TYPES}
type
  NSMethodSignaturePtr = ^NSMethodSignature;
{$endif}

{$ifdef CLASSES}

type
  NSMethodSignature = objcclass external (NSObject)
  private
    _private: pointer;
    _reserved:array[0..5] of pointer;
  public
    class function signatureWithObjCTypes (types: PAnsiChar): NSMethodSignature; message 'signatureWithObjCTypes:';
    function numberOfArguments: NSUInteger; message 'numberOfArguments';
    function getArgumentTypeAtIndex (idx: NSUInteger): PAnsiChar; message 'getArgumentTypeAtIndex:';
    function frameLength: NSUInteger; message 'frameLength';
    function isOneway: ObjCBOOL; message 'isOneway';
    function methodReturnType: PAnsiChar; message 'methodReturnType';
    function methodReturnLength: NSUInteger; message 'methodReturnLength';
  end;
{$endif}

